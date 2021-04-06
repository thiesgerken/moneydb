{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}

module Moneydb.Api.Query(queryHandlers, QueryAPIs) where

import           Control.Lens                 ((&), (.~), (?~))
import           Control.Monad                (unless)
import           Control.Monad.IO.Class       (MonadIO (..))
import           Control.Monad.IO.Unlift      (MonadUnliftIO)
import           Control.Monad.Trans.Reader   (ReaderT)
import           Control.Monad.Trans.Resource (ResourceT)
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Char                    (isDigit, toLower)
import           Data.Int                     (Int64)
import           Data.List                    (foldl1', isPrefixOf)
import           Data.List.Utils              (replace)
import           Data.Maybe                   (catMaybes)
import           Data.Proxy                   (Proxy (..))
import           Data.Swagger                 hiding (fieldLabelModifier)
import qualified Data.Swagger                 as Swagger
import qualified Data.Text                    as T
import           Data.Time                    (UTCTime)
import           Database.Esqueleto           hiding (replace)
import qualified Database.Esqueleto           as E
import           GHC.Generics                 (Generic)
import           Safe                         (readMay)
import           Safe.Foldable                (foldl1May)
import           Servant.API
import           Servant.Server

import           Moneydb.Database
import           Moneydb.Types

newtype NoData = NoData String deriving (Generic, FromJSON, ToSchema)

instance TypeName NoData where
  typeName _ = ""

data Query b = Query {queryDraw :: Int, queryStart :: Int, queryLength :: Int, queryColumns :: [QueryColumn], queryOrder :: [Order], querySearch :: SearchOptions, queryExtra :: b } deriving Generic

data QueryColumn = QueryColumn {columnData :: String, columnName :: String, columnSearchable :: Bool, columnOrderable :: Bool, columnSearch :: SearchOptions} deriving Generic

data Order = Order {orderColumn :: Int, orderDir :: String} deriving Generic
data SearchOptions = SearchOptions {searchValue :: String, searchRegex :: Bool} deriving Generic

data BalanceQueryOptions = BalanceQueryOptions {balanceOptionFrom :: Maybe UTCTime, balanceOptionTo :: Maybe UTCTime, balanceOptionAccounts :: [Int64] } deriving (Generic, TypeName)
instance ToSchema BalanceQueryOptions where declareNamedSchema = genericDeclareNamedSchema $ defaultSchemaOptions { Swagger.fieldLabelModifier = prefixModifier'' "balanceOption" }
$(deriveFromJSON defaultOptions{fieldLabelModifier = prefixModifier'' "balanceOption"} ''BalanceQueryOptions)

data ExpenseQueryOptions = ExpenseQueryOptions {expenseOptionFrom :: Maybe UTCTime, expenseOptionTo :: Maybe UTCTime, expenseOptionCategories :: [Int64], expenseOptionAccounts :: [Int64] } deriving (Generic, TypeName)
instance ToSchema ExpenseQueryOptions where declareNamedSchema = genericDeclareNamedSchema $ defaultSchemaOptions { Swagger.fieldLabelModifier = prefixModifier'' "expenseOption" }
$(deriveFromJSON defaultOptions{fieldLabelModifier = prefixModifier'' "expenseOption" } ''ExpenseQueryOptions)

class Searchable a where
  orderByField :: (forall x. PersistField x => SqlExpr (E.Value x) -> SqlExpr OrderBy) -> SqlExpr (Entity a) -> String -> Maybe (SqlExpr OrderBy)
  visible :: Entity User -> SqlExpr (Entity a) -> SqlExpr (E.Value Bool)
  filters :: SqlExpr (Entity a) -> [SqlExpr (E.Value String)]
  advancedFilters :: String -> SqlExpr (Entity a) -> [SqlExpr (E.Value Bool)]
  advancedFilters _ _ = []

directionToSql :: PersistField a => String -> SqlExpr (E.Value a) -> SqlExpr OrderBy
directionToSql "asc" = asc
directionToSql _     = desc

orderByColumn :: Searchable a => [QueryColumn] -> SqlExpr (Entity a) -> Order -> Maybe (SqlExpr OrderBy)
orderByColumn cols x (Order i d) =
    orderByField (directionToSql d) x (columnData (cols !! i))

instance ToSchema SearchOptions where declareNamedSchema = genericDeclareNamedSchema $ defaultSchemaOptions { Swagger.fieldLabelModifier = prefixModifier }
$(deriveFromJSON defaultOptions{fieldLabelModifier = prefixModifier} ''SearchOptions)

instance ToSchema Order where declareNamedSchema = genericDeclareNamedSchema $ defaultSchemaOptions { Swagger.fieldLabelModifier = prefixModifier }
$(deriveFromJSON defaultOptions{fieldLabelModifier = prefixModifier} ''Order)

instance (TypeName a, ToSchema a) => ToSchema (Query a) where
  declareNamedSchema _ = do
    intSchema <- declareSchemaRef (Proxy :: Proxy Int)
    colSchema <- declareSchemaRef (Proxy :: Proxy [QueryColumn])
    orderSchema <- declareSchemaRef (Proxy :: Proxy [Order])
    searchSchema <- declareSchemaRef (Proxy :: Proxy SearchOptions)
    extraSchema <- declareSchemaRef (Proxy :: Proxy a)

    return . NamedSchema (Just $ "Query" <> T.pack (typeName (Proxy :: Proxy a))) $ mempty
      & type_ ?~ SwaggerObject
      & properties .~
           [("draw", intSchema), ("start", intSchema), ("length", intSchema), ("columns", colSchema), ("order", orderSchema), ("search", searchSchema), ("extra", extraSchema) ]
      & required .~ ["draw", "start", "length", "columns", "order", "search", "extra"]

$(deriveFromJSON defaultOptions{fieldLabelModifier = prefixModifier} ''Query)

instance ToSchema QueryColumn where declareNamedSchema = genericDeclareNamedSchema $ defaultSchemaOptions { Swagger.fieldLabelModifier = prefixModifier }
$(deriveFromJSON defaultOptions{fieldLabelModifier = prefixModifier} ''QueryColumn)

data QueryResponse a = QueryResponse {draw' :: Int, recordsTotal :: Int, recordsFiltered :: Int, data' :: [Record a]} deriving Generic

instance (TypeName a, ToSchema a) => ToSchema (QueryResponse a) where
  declareNamedSchema _ = do
    intSchema <- declareSchemaRef (Proxy :: Proxy Int)
    dataSchema <- declareSchemaRef (Proxy :: Proxy [Record a])
    return . NamedSchema (Just $ "QueryResponse" <> T.pack (typeName (Proxy :: Proxy a))) $ mempty
      & type_ ?~ SwaggerObject
      & properties .~
           [("draw", intSchema), ("recordsTotal", intSchema), ("recordsFiltered", intSchema), ("data", dataSchema)]
      & required .~ ["draw", "recordsTotal", "recordsFiltered", "data"]

instance ToJSON a => ToJSON (QueryResponse a) where
    -- this generates a Value
    toJSON c =
        object ["draw" .= draw' c, "recordsTotal" .= recordsTotal c, "recordsFiltered" .= recordsFiltered c, "data" .= data' c]

    -- this encodes directly to a bytestring Builder
    toEncoding c =
        pairs ("draw" .= draw' c <> "recordsTotal" .= recordsTotal c <> "recordsFiltered" .= recordsFiltered c <> "data" .= data' c)

-- opts: Query as a JSON-serialized string
type QueryAPI a b = ReqBody '[JSON] (Query a) :> Post '[JSON] (QueryResponse b)

type QueryAPIs = "expenses" :> "rendered" :> "query" :> QueryAPI ExpenseQueryOptions RenderedExpense
            :<|> "balances" :> "query" :> QueryAPI BalanceQueryOptions Balance
            :<|> "stocks" :> "query" :> QueryAPI NoData RenderedStock
            :<|> "stocks" :> "accounts":> "query" :> QueryAPI NoData SecuritiesAccount
            :<|> "stocks" :> "transactions":> "query" :> QueryAPI NoData StockTransaction
            :<|> "rules" :> "automation" :> "query" :> QueryAPI NoData AutomationRule
            :<|> "rules" :> "periodic" :> "query" :> QueryAPI NoData PeriodicRule
            :<|> "devices" :> "rendered" :> "query" :> QueryAPI NoData RenderedDevice
            :<|> "accounts" :> "rendered" :> "query" :> QueryAPI NoData RenderedAccount
            :<|> "categories" :> "rendered" :> "query" :> QueryAPI NoData RenderedCategory

queryHandlers :: MoneyDBConn -> Entity User -> Server QueryAPIs
queryHandlers mc u = queryHandler mc u :<|> queryHandler mc u :<|>
                      queryHandler mc u :<|> queryHandler mc u :<|> queryHandler mc u :<|> queryHandler mc u :<|> queryHandler mc u :<|> queryHandler mc u :<|> queryHandler mc u :<|> queryHandler mc u

queryHandler :: Queryable a b => MoneyDBConn -> Entity User -> Server (QueryAPI a b)
queryHandler mc u q = liftIO $ runTransaction mc (query u q)

class Queryable a b where
  query :: Entity User -> Query a -> ReaderT SqlBackend (ResourceT IO) (QueryResponse b)

instance Queryable BalanceQueryOptions Balance where
  query = defaultQuery (fmap (fmap (fmap entityVal)) . renderElements) extra
    where extra d x = foldl1May (&&.) $ catMaybes [fmap (f x) (balanceOptionFrom d), fmap (t x) (balanceOptionTo d), a x (balanceOptionAccounts d)]
          a _ [] = Nothing
          a x xs = Just $ (x ^. BalanceAccountId `E.in_` aids') ||. (x ^. BalanceAccountId `E.in_` sync1) ||. (x ^. BalanceAccountId `E.in_` sync2)
             where aids' = valList (toSqlKey <$> xs) :: SqlExpr (ValueList AccountId)
                   sync1 = subList_select $ from $ \as -> do
                                     where_ $ (as ^. AccountSyncingAccount1) `E.in_` aids'
                                     return $ as ^. AccountSyncingAccount2
                   sync2 = subList_select $ from $ \as -> do
                                     where_ $ (as ^. AccountSyncingAccount2) `E.in_` aids'
                                     return $ as ^. AccountSyncingAccount1
          t x y = x ^. BalanceDate <=. val y
          f x y = x ^. BalanceDate >=. val y

instance Queryable NoData RenderedDevice where
  query = defaultQuery'' (\_ -> mapM getRenderedDevice)

instance Queryable NoData RenderedCategory where
  query = defaultQuery'' (mapM . getRenderedCategory . entityKey)

instance Queryable NoData RenderedStock where
  query = defaultQuery'' (\_ -> mapM getRenderedStock)

instance Queryable NoData StockTransaction where
  query = defaultQuery'

instance Queryable NoData SecuritiesAccount where
  query = defaultQuery'

instance Queryable NoData AutomationRule where
  query = defaultQuery'

instance Queryable NoData PeriodicRule where
  query = defaultQuery'

instance Queryable NoData RenderedAccount where
  query = defaultQuery'' (\_ -> mapM getRenderedAccount)

instance Queryable ExpenseQueryOptions RenderedExpense where
  query = defaultQuery (getRenderedExpenses . entityKey) extra
    where extra d x = foldl1May (&&.) $ catMaybes [fmap (f x) (expenseOptionFrom d), fmap (t x) (expenseOptionTo d), c x (expenseOptionCategories d), a x (expenseOptionAccounts d)]
          c _ [] = Nothing
          c x xs = Just $ x ^. ExpenseCategoryId `E.in_` valList (toSqlKey <$> xs)
          a _ [] = Nothing
          a x xs = Just $ self ||. shared
             where aids = valList (toSqlKey <$> xs) :: SqlExpr (ValueList AccountId)
                   sync1 = subList_select $ from $ \as -> do
                                     where_ $ (as ^. AccountSyncingAccount1) `E.in_` aids
                                     return $ as ^. AccountSyncingAccount2
                   sync2 = subList_select $ from $ \as -> do
                                     where_ $ (as ^. AccountSyncingAccount2) `E.in_` aids
                                     return $ as ^. AccountSyncingAccount1
                   self = (x ^. ExpenseAccountId `E.in_` aids) ||. (x ^. ExpenseAccountId `E.in_` sync1) ||. (x ^. ExpenseAccountId `E.in_` sync2)
                   shared = exists $ from $ \es -> do
                                      where_ $ ((es ^. ExpenseSharingExpenseId) ==. (x ^. ExpenseId))
                                                &&. ( (es ^. ExpenseSharingAccountId `E.in_` aids) ||. (es ^. ExpenseSharingAccountId `E.in_` sync1) ||. (es ^. ExpenseSharingAccountId `E.in_` sync2) )
                                      return ()
          t x y = (x ^. ExpenseValueDate <=. val y) ||. (x ^. ExpenseBookingDate <=. just (val y))
          f x y = (x ^. ExpenseValueDate >=. val y) ||. (x ^. ExpenseBookingDate >=. just (val y))

defaultQuery :: forall a b c (m :: * -> *). (DBElement a, MonadUnliftIO m, MonadFail m, Searchable a) => (Entity User -> [Entity a] -> ReaderT SqlBackend (ResourceT m) [b]) -> (c -> SqlExpr (Entity a) -> Maybe (SqlExpr (E.Value Bool))) -> Entity User -> Query c -> ReaderT SqlBackend (ResourceT m) (QueryResponse b)
defaultQuery renderer extra u q = do
  [E.Value (totalCount :: Int)] <-
            select $ from $ \x -> do
              where_ $ visible u (x :: SqlExpr (Entity a))
              mapM_ where_ (extra (queryExtra q) x)
              return countRows

  let needle = searchValue $ querySearch q

  [E.Value (filteredCount :: Int)] <-
    if null needle  then return [E.Value totalCount]
    else select $ from $ \x -> do
      mapM_ where_ (extra (queryExtra q) x)
      where_ $ visible u (x :: SqlExpr (Entity a))

      let fs = filters x
      let fs1 = advancedFilters needle x
      unless (null fs && null fs1) $
        where_ $ foldl1' (||.) $ ((\f -> f `like` (%) ++. val needle ++. (%)) <$> fs) ++ fs1
      return countRows

  xs <- select $ from $ \x -> do
    where_ $ visible u x
    mapM_ where_ (extra (queryExtra q) x)

    let fs = filters x
    let fs1 = advancedFilters needle x
    unless (null needle || (null fs && null fs1)) $
      where_ $ foldl1' (||.) $ ((\f -> f `like` (%) ++. val needle ++. (%)) <$> fs) ++ fs1

    orderBy . catMaybes $ orderByColumn (queryColumns q) x <$> queryOrder q
    paginate (Just . fromIntegral $ queryStart q) (Just . fromIntegral $ queryLength q)
    return x

  xs' <- renderer u xs
  -- let xs'' = map (\(Entity k e) -> (Record (fromSqlKey k) e)) xs'
  let xs'' = zipWith (\ent x -> Record (fromSqlKey $ entityKey ent) x) xs xs'

  return $ QueryResponse (queryDraw q) totalCount filteredCount xs''

defaultQuery' :: forall a c (m :: * -> *). (DBElement a, MonadUnliftIO m, MonadFail m, Searchable a) => Entity User -> Query c -> ReaderT SqlBackend (ResourceT m) (QueryResponse a)
defaultQuery' = defaultQuery (fmap (fmap (fmap entityVal)) . renderElements) (\_ _ -> Nothing)

defaultQuery'' :: forall a b c (m :: * -> *). (DBElement a, MonadUnliftIO m, MonadFail m, Searchable a) => (Entity User -> [Entity a] -> ReaderT SqlBackend (ResourceT m) [b]) -> Entity User -> Query c -> ReaderT SqlBackend (ResourceT m) (QueryResponse b)
defaultQuery'' r = defaultQuery r (\_ _ -> Nothing)

instance Searchable Expense where
  orderByField d x "id"          = Just . d $ x ^. ExpenseId
  orderByField d x "valueDate"   = Just . d $ x ^. ExpenseValueDate
  orderByField d x "bookingDate" = Just . d $ x ^. ExpenseBookingDate
  orderByField d x "accountId"   = Just . d $ x ^. ExpenseAccountId -- might not be what is shown after rendering
  orderByField d x "categoryId"  = Just . d $ x ^. ExpenseCategoryId -- might not be what is shown after rendering
  orderByField d x "store"       = Just . d $ x ^. ExpenseStore
  orderByField d x "description" = Just . d $ x ^. ExpenseDescription
  orderByField d x "transaction" = Just . d $ x ^. ExpenseTransaction
  orderByField d x "title"       = Just . d $ x ^. ExpenseTitle
  orderByField d x "amount"       = Just . d $ x ^. ExpenseAmount -- (does not include sharing, sign might be wrong)
  orderByField d x "checked"     = Just . d $ (needsAttention &&. notTemplate) ||. preliminary
      where needsAttention = exists . from $ \flag -> where_ ((flag ^. ExpenseFlagExpenseId) ==. (x ^. ExpenseId) &&. ((flag ^. ExpenseFlagFlagging) ==. val NeedsAttention))
            preliminary = exists . from $ \flag -> where_ ((flag ^. ExpenseFlagExpenseId) ==. (x ^. ExpenseId) &&. ((flag ^. ExpenseFlagFlagging) ==. val Preliminary))
            notTemplate = notExists . from $ \flag -> where_ ((flag ^. ExpenseFlagExpenseId) ==. (x ^. ExpenseId) &&. ((flag ^. ExpenseFlagFlagging) ==. val Template))
  orderByField _ _ _             = Nothing

  visible = readable
  filters x = [x ^. ExpenseStore,  x ^. ExpenseDescription, x ^. ExpenseTransaction, x ^. ExpenseTitle, x ^. ExpenseComments]
  --     where accTitle = sub_select $ from $ \y -> do
  --                       where_ $ (y ^. AccountId) ==. (x ^. ExpenseAccountId)
  --                       return $ y ^. AccountTitle -- might not be what is shown after rendering
  --           catTitle = sub_select $ from $ \y -> do
  --                       where_ $ (y ^. CategoryId) ==. (x ^. ExpenseCategoryId)
  --                       return $ y ^. CategoryTitle -- might not be what is shown after rendering
  advancedFilters needle x = [idFilter, amountFilter, taxFilter]
        where amountFilter
                 | all isDigit needle, Just y <- readMay needle = ((x ^. ExpenseAmount) >=. val y) &&. ((x ^. ExpenseAmount) <. val (y+1))
                 | not (null needle) && head needle == '-' && all isDigit (tail needle), Just y <- readMay needle = ((x ^. ExpenseAmount) <=. val y) &&. ((x ^. ExpenseAmount) >. val (y-1))
                 | Just y <- readMay (replace "," "." needle) = (x ^. ExpenseAmount) ==. val y
                 | otherwise = val False
              idFilter
                 | all isDigit needle, Just y <- readMay needle = (x ^. ExpenseId) ==. val (toSqlKey y)
                 | otherwise = val False
              taxFilter  -- TODO: not very nice ...
                 | "tax" `isPrefixOf` fmap toLower needle = exists $ from $ \y -> do
                                         where_ $ ((y ^. ExpenseFlagExpenseId) ==. (x ^. ExpenseId)) &&. (y ^. ExpenseFlagFlagging ==. val TaxRelevant)
                                         return ()
                 | otherwise = val False
  --     where accTitleSync = exists $ from $ \(as, a1, a2) -> do
  --                             where_ $ ((a1 ^. AccountId) ==. (as ^. AccountSyncingAccount1))
  --                                 &&. ((a2 ^. AccountId) ==. (as ^. AccountSyncingAccount2))
  --                                 &&. ( (((x ^. ExpenseAccountId) ==. (as ^. AccountSyncingAccount1))
  --                                        &&. ((a2 ^. AccountTitle) `like` needle))
  --                                  ||.  (((x ^. ExpenseAccountId) ==. (as ^. AccountSyncingAccount2))
  --                                            &&. ((a1 ^. AccountTitle) `like` needle)))
  --                             return ()
  --           sharedAccTitle = exists $ from $ \(a, es) -> do
  --                              where_ $ ((es ^. ExpenseSharingExpenseId) ==. (x ^. ExpenseId))
  --                                        &&. ((es ^. ExpenseSharingAccountId) ==. (a ^. AccountId))
  --                                        &&. ((a ^. AccountTitle) `like` needle)
  --                              return ()
  --           sharedAccTitleSync = exists $ from $ \(a1, a2, es, as) -> do
  --                              where_ $ ((es ^. ExpenseSharingExpenseId) ==. (x ^. ExpenseId))
  --                                     &&.  ((a1 ^. AccountId) ==. (as ^. AccountSyncingAccount1))
  --                                     &&.  ((a2 ^. AccountId) ==. (as ^. AccountSyncingAccount2))
  --                                     &&. ( (((es ^. ExpenseSharingAccountId) ==. (as ^. AccountSyncingAccount1))
  --                                            &&. ((a2 ^. AccountTitle) `like` needle))
  --                                      ||.  (((es ^. ExpenseSharingAccountId) ==. (as ^. AccountSyncingAccount2))
  --                                                &&. ((a1 ^. AccountTitle) `like` needle)))
  --                              return ()

instance Searchable Balance where
  orderByField d x "id"        = Just . d $ x ^. BalanceId
  orderByField d x "date"      = Just . d $ x ^. BalanceDate
  orderByField d x "accountId" = Just . d $ x ^. BalanceAccountId -- might not be what is shown after rendering
  orderByField d x "amount"    = Just . d $ x ^. BalanceAmount
  orderByField _ _ _           = Nothing

  visible = readable
  filters x = [accTitle]
             where accTitle = subSelectUnsafe $ from $ \y -> do
                    where_ $ (y ^. AccountId) ==. (x ^. BalanceAccountId)
                    return $ y ^. AccountTitle -- might not be what is shown after rendering
  advancedFilters needle x = [idFilter, amountFilter]
        where amountFilter
                 | all isDigit needle, Just y <- readMay needle = ((x ^. BalanceAmount) >=. val y) &&. ((x ^. BalanceAmount) <. val (y+1))
                 | not (null needle) && head needle == '-' && all isDigit (tail needle), Just y <- readMay needle = ((x ^. BalanceAmount) <=. val y) &&. ((x ^. BalanceAmount) >. val (y-1))
                 | Just y <- readMay (replace "," "." needle) = (x ^. BalanceAmount) ==. val y
                 | otherwise = val False
              idFilter
                 | all isDigit needle, Just y <- readMay needle = (x ^. BalanceId) ==. val (toSqlKey y)
                 | otherwise = val False

instance Searchable AutomationRule where
  orderByField d x "id"               = Just . d $ x ^. AutomationRuleId
  orderByField d x "title"            = Just . d $ x ^. AutomationRuleTitle
  orderByField d x "templateId"       = Just . d $ x ^. AutomationRuleTemplateId
  orderByField d x "priority"         = Just . d $ x ^. AutomationRulePriority
  orderByField d x "regexTime"        = Just . d $ x ^. AutomationRuleRegexTime
  orderByField d x "filterAccount"    = Just . d $ x ^. AutomationRuleFilterAccount
  orderByField d x "regexTransaction" = Just . d $ x ^. AutomationRuleRegexTransaction
  orderByField d x "filterAmount"     = Just . d $ x ^. AutomationRuleFilterAmount
  orderByField d x "lastDelivery"     = Just . d $ x ^. AutomationRuleLastDelivery
  orderByField _ _ _                  = Nothing

  visible = readable
  filters x = [x ^. AutomationRuleTitle]
  advancedFilters needle x = [idFilter, templateFilter]
        where idFilter
                 | all isDigit needle, Just y <- readMay needle = (x ^. AutomationRuleId) ==. val (toSqlKey y)
                 | otherwise = val False
              templateFilter
                 | all isDigit needle, Just y <- readMay needle = (x ^. AutomationRuleTemplateId) ==. val (toSqlKey y)
                 | otherwise = val False

instance Searchable PeriodicRule where
  orderByField d x "id"                = Just . d $ x ^. PeriodicRuleId
  orderByField d x "templateId"        = Just . d $ x ^. PeriodicRuleTemplateId
  orderByField d x "title"             = Just . d $ x ^. PeriodicRuleTitle
  orderByField d x "period"            = Just . d $ x ^. PeriodicRulePeriod
  orderByField d x "titleReplacement"  = Just . d $ x ^. PeriodicRuleTitleReplacement
  orderByField d x "amountReplacement" = Just . d $ x ^. PeriodicRuleAmountReplacement
  orderByField d x "lastCreation"      = Just . d $ x ^. PeriodicRuleLastCreation
  orderByField _ _ _                   = Nothing

  visible = readable
  filters x = [x ^. PeriodicRuleTitle]
  advancedFilters needle x = [idFilter, templateFilter]
        where idFilter
                 | all isDigit needle, Just y <- readMay needle = (x ^. PeriodicRuleId) ==. val (toSqlKey y)
                 | otherwise = val False
              templateFilter
                 | all isDigit needle, Just y <- readMay needle = (x ^. PeriodicRuleTemplateId) ==. val (toSqlKey y)
                 | otherwise = val False

instance Searchable StockTransaction where
  orderByField d x "id"        = Just . d $ x ^. StockTransactionId
  orderByField d x "accountId" = Just . d $ x ^. StockTransactionAccountId
  orderByField d x "stockId"   = Just . d $ x ^. StockTransactionStockId
  orderByField d x "date"      = Just . d $ x ^. StockTransactionDate
  orderByField d x "units"     = Just . d $ x ^. StockTransactionUnits
  orderByField d x "amount"    = Just . d $ x ^. StockTransactionAmount
  orderByField d x "fees"      = Just . d $ x ^. StockTransactionFees
  orderByField d x "exchange"  = Just . d $ x ^. StockTransactionExchange
  orderByField _ _ _           = Nothing

  visible = readable
  filters x = [x ^. StockTransactionExchange]
  advancedFilters needle x = [idFilter]
          where idFilter
                 | all isDigit needle, Just y <- readMay needle = (x ^. StockTransactionId) ==. val (toSqlKey y)
                 | otherwise = val False

instance Searchable SecuritiesAccount where
  orderByField d x "id"     = Just . d $ x ^. SecuritiesAccountId
  orderByField d x "title"  = Just . d $ x ^. SecuritiesAccountTitle
  orderByField d x "iban"   = Just . d $ x ^. SecuritiesAccountIban
  orderByField d x "broker" = Just . d $ x ^. SecuritiesAccountBroker
  orderByField _ _ _        = Nothing

  visible = readable
  filters x = [x ^. SecuritiesAccountTitle, x ^. SecuritiesAccountIban, x ^. SecuritiesAccountBroker]
  advancedFilters needle x = [idFilter]
          where idFilter
                  | all isDigit needle, Just y <- readMay needle = (x ^. SecuritiesAccountId) ==. val (toSqlKey y)
                  | otherwise = val False

instance Searchable Stock where
  orderByField d x "id"   = Just . d $ x ^. StockId
  orderByField d x "isin" = Just . d $ x ^. StockIsin
  orderByField _ _ _      = Nothing

  visible = readable
  filters x = [x ^. StockIsin]
  advancedFilters needle x = [idFilter]
          where idFilter
                 | all isDigit needle, Just y <- readMay needle = (x ^. StockId) ==. val (toSqlKey y)
                 | otherwise = val False

instance Searchable Device where
  orderByField d x "id"                = Just . d $ x ^. DeviceId
  orderByField d x "token"             = Just . d $ x ^. DeviceToken
  orderByField d x "model"             = Just . d $ x ^. DeviceModel
  orderByField d x "firstContact"      = Just . d $ x ^. DeviceFirstContact
  orderByField d x "lastContact"       = Just . d $ x ^. DeviceLastContact
  orderByField d x "notificationCount" = Just . d $ x ^. DeviceNotificationCount
  orderByField d x "lastNotification"  = Just . d $ x ^. DeviceLastNotification
  orderByField _ _ _                   = Nothing

  visible = readable
  filters x = [x ^. DeviceModel, x ^. DeviceToken]
  advancedFilters needle x = [idFilter]
          where idFilter
                 | all isDigit needle, Just y <- readMay needle = (x ^. DeviceId) ==. val (toSqlKey y)
                 | otherwise = val False

instance Searchable Category where
  orderByField d x "id"          = Just . d $ x ^. CategoryId
  orderByField d x "title"       = Just . d $ x ^. CategoryTitle
  orderByField d x "description" = Just . d $ x ^. CategoryDescription
  orderByField d x "color"       = Just . d $ x ^. CategoryColor
  orderByField _ _ _             = Nothing

  visible (Entity uk _) x = x ^. CategoryOwnerId ==. E.val uk
  filters x = [x ^. CategoryTitle, x ^. CategoryDescription, x ^. CategoryColor]
  advancedFilters needle x = [idFilter]
          where idFilter
                 | all isDigit needle, Just y <- readMay needle = (x ^. CategoryId) ==. val (toSqlKey y)
                 | otherwise = val False

instance Searchable Account where
  orderByField d x "id"           = Just . d $ x ^. AccountId
  orderByField d x "title"        = Just . d $ x ^. AccountTitle
  orderByField d x "description"  = Just . d $ x ^. AccountDescription
  orderByField d x "color"        = Just . d $ x ^. AccountColor
  orderByField d x "kind"         = Just . d $ x ^. AccountKind
  orderByField d x "availability" = Just . d $ x ^. AccountAvailability
  orderByField d x "hidden"       = Just . d $ x ^. AccountHidden
  orderByField _ _ _              = Nothing

  visible (Entity uk _) x = x ^. AccountOwnerId ==. E.val uk
  filters x = [x ^. AccountTitle, x ^. AccountDescription, x ^. AccountColor]
  advancedFilters needle x = [idFilter]
          where idFilter
                 | all isDigit needle, Just y <- readMay needle = (x ^. AccountId) ==. val (toSqlKey y)
                 | otherwise = val False
