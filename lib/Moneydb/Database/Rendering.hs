{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

-- | database bindings for moneydb.
--   Note that this module may abort execution on malformed queries or process them, leading to a corrupt database.
--   (example: Adding an expense with different ownerid and owner of account & account is not shared)
module Moneydb.Database.Rendering
    ( getRenderedDevice
    , getRenderedStock
    , getRenderedExpense
    , getRenderedAccount
    , getRenderedCategory
    , getRenderedExpenses
    , getRenderedExpenses'
    , validateRenderedExpense
    , changeBalancePerspective
    , changeBalancePerspectives
    ) where

import           Control.Monad                   (unless, when)
import           Control.Monad.IO.Class          (MonadIO (..))
import           Control.Monad.Trans.Class       (lift)
import           Control.Monad.Trans.Maybe       (MaybeT (..))
import           Control.Monad.Trans.Reader      (ReaderT)
import           Data.Decimal                    (Decimal, divide, roundTo)
import           Data.Maybe                      (catMaybes, listToMaybe)
import           Database.Esqueleto              hiding (isNothing)
import qualified Database.Persist                as P
import           Moneydb.Types
import qualified Moneydb.Types.Rendering.Expense as RE
import qualified Moneydb.Types.Rendering.Stock   as RS

-- at least sqlite has problems with query with a lot of variables (e.g. huge `in` clauses with >~ 1000 values)
-- we do not suspect this to happen with accounts and account syncings, but it can easily happen with expenses.
limitVariables :: (PersistField a, Monad m)
               => [a]
               -> (SqlExpr (ValueList a) -> m [b])
               -> m [b]
limitVariables xs f = fmap concat . mapM f $ part 800 xs
  where
    part n ys
        | length ys <= n = [ valList ys ]
        | otherwise = valList (take n ys) : part n (drop n ys)

handleSyncingCached :: [Entity Account]
                    -> [AccountSyncing]
                    -> Key User
                    -> Key Account
                    -> (Key Account, Bool, Bool)
handleSyncingCached as ass uid aid -- will crash if the account does not exist
    | (accountOwnerId . entityVal . head . filter ((==) aid . entityKey) $ as)
        == uid = (aid, True, True)
    | otherwise = case listToMaybe
        . filter (\sync -> accountSyncingAccount1 sync == aid
                  || accountSyncingAccount2 sync == aid) $ ass of
            Just (AccountSyncing a1 a2 f) ->
                (entityKey a', f, accountOwnerId (entityVal a') == uid)
              where
                a' = head . filter ((==) aid' . entityKey) $ as

                aid' = if aid == a1 then a2 else a1
            _ -> (aid, True, False)

-- first bool: sign (True -> 1, False -> -1), second bool: (replaced) account belongs to user
handleSyncing :: (MonadIO m, BaseBackend SqlBackend ~ SqlBackend)
              => Key User
              -> Key Account
              -> ReaderT SqlBackend m (Key Account, Bool, Bool)
handleSyncing uid aid = do
    a <- P.getJust aid -- will crash if the account does not exist
    if accountOwnerId a == uid
        then return (aid, True, True)
        else do
            s <- fmap entityVal
                <$> P.selectFirst ([ AccountSyncingAccount1 P.==. aid
                                   ] P.||. [ AccountSyncingAccount2 P.==. aid ])
                                  []
            case s of
                Just (AccountSyncing a1 a2 f) -> do
                    let aid' = if aid == a1 then a2 else a1
                    a' <- P.getJust aid'
                    return (aid', f, accountOwnerId a' == uid)
                _ -> return (aid, True, False)

handleSyncing' :: (MonadIO m, BaseBackend SqlBackend ~ SqlBackend)
               => Key User
               -> ExpenseSharing
               -> ReaderT SqlBackend m (ExpenseSharing, Bool)
handleSyncing' uid s = do
    (a, x, own) <- handleSyncing uid (expenseSharingAccountId s)
    return ( s { expenseSharingAccountId = a
               , expenseSharingParam     = changeSign x (expenseSharingParam s)
               }
           , own
           )

handleSyncingCached' :: [Entity Account]
                     -> [AccountSyncing]
                     -> Key User
                     -> ExpenseSharing
                     -> (ExpenseSharing, Bool)
handleSyncingCached' as ass uid s =
    ( s { expenseSharingAccountId = a
        , expenseSharingParam     = changeSign x (expenseSharingParam s)
        }
    , own
    )
  where
    (a, x, own) = handleSyncingCached as ass uid (expenseSharingAccountId s)

calculateSharedAmount :: Decimal -> ExpenseSharing -> Decimal -> Decimal
calculateSharedAmount _ (ExpenseSharing _ _ Equal p) equalAmount =
    roundTo 2 $ -p * equalAmount
calculateSharedAmount amount (ExpenseSharing _ _ FixedFraction p) _ =
    roundTo 2 $ -p * amount
calculateSharedAmount _ (ExpenseSharing _ _ FixedAmount p) _ = roundTo 2 $ -p

renderExpense :: (Expense, Bool)
              -> [(ExpenseSharing, Bool)]
              -> [ExpenseFlagging]
              -> Maybe RenderedExpense
renderExpense (e, own) sharing flags
    | own || any snd sharing = Just $ convertExpense e x sharing' flags
    | otherwise = Nothing
  where
    equalN = 1 + length (filter ((Equal ==) . expenseSharingType . fst) sharing)

    equalAmount = snd . head . divide (expenseAmount e) $ equalN

    sharing' =
        fmap (\(s, _) -> convertSharing s
                                        (calculateSharedAmount (expenseAmount e)
                                                               s
                                                               equalAmount))
             sharing

    x = (if own then expenseAmount e else roundTo 2 0)
        + sum (RE.calculatedAmount . fst
               <$> filter snd (zipWith (\y (_, b) -> (y, b)) sharing' sharing))

changeSigns :: Bool -> ExpenseSharing -> ExpenseSharing
changeSigns _ x@(ExpenseSharing _ _ FixedAmount _) = x
changeSigns y x@(ExpenseSharing _ _ _ p) =
    x { expenseSharingParam = if y then p else -p }

changeSign :: Bool -> Decimal -> Decimal
changeSign y p = if y then p else -p

changePerspective
    :: MonadIO m
    => Key User
    -> Entity Expense
    -> ReaderT SqlBackend m ((Expense, Bool), [(ExpenseSharing, Bool)])
changePerspective uk (Entity ek e) = do
    sharing <- fmap entityVal
        <$> P.selectList [ ExpenseSharingExpenseId P.==. ek ] []
    cat <- maybe (expenseCategoryId e)
                 (categoryReplacementReplacement . entityVal)
        <$> P.selectFirst [ CategoryReplacementOriginal
                                P.==. expenseCategoryId e
                          , CategoryReplacementOwnerId P.==. uk
                          ]
                          []
    (a', f, own) <- handleSyncing uk (expenseAccountId e)
    let e' = e { expenseCategoryId = cat
               , expenseAccountId  = a'
               , expenseAmount     = changeSign f (expenseAmount e)
               }
    let sharing' = fmap (changeSigns f) sharing
    sharing'' <- mapM (handleSyncing' uk) sharing'
    return ((e', own), sharing'')

changePerspectiveCached
    :: MonadIO m
    => [Entity Account]
    -> [AccountSyncing]
    -> [CategoryReplacement]
    -> [ExpenseSharing]
    -> Key User
    -> Entity Expense
    -> ReaderT SqlBackend m ((Expense, Bool), [(ExpenseSharing, Bool)])
changePerspectiveCached as ass catr ss uk (Entity ek e) = do
    let sharing = filter ((==) ek . expenseSharingExpenseId) ss
    let cat = maybe (expenseCategoryId e)
                    categoryReplacementReplacement . listToMaybe
            . filter (\cr ->
                      categoryReplacementOriginal cr == expenseCategoryId e
                      && categoryReplacementOwnerId cr == uk) $ catr
        (a', f, own) = handleSyncingCached as ass uk (expenseAccountId e)
        e' = e { expenseCategoryId = cat
               , expenseAccountId  = a'
               , expenseAmount     = changeSign f (expenseAmount e)
               }
        sharing' = fmap (changeSigns f) sharing
        sharing'' = handleSyncingCached' as ass uk <$> sharing'
    return ((e', own), sharing'')

changeBalancePerspective :: MonadIO m
                         => Key User
                         -> Balance
                         -> ReaderT SqlBackend m Balance
changeBalancePerspective uk b = do
    (a', f, _) <- handleSyncing uk (balanceAccountId b)
    let b' = b { balanceAccountId = a'
               , balanceAmount    = changeSign f (balanceAmount b)
               }
    return b'

changeBalancePerspectiveCached
    :: [Entity Account]
    -> [AccountSyncing]
    -> Key User
    -> Balance
    -> Balance
changeBalancePerspectiveCached as ass uk b = b'
  where
    (a', f, _) = handleSyncingCached as ass uk (balanceAccountId b)

    b' = b { balanceAccountId = a'
           , balanceAmount    = changeSign f (balanceAmount b)
           }

-- essentially `mapM changeBalancePerspective`, but retrieves all accounts, syncings and category replacements before. This is a lot faster when retrieving a lot of expenses.
changeBalancePerspectives :: (MonadIO m, BaseBackend SqlBackend ~ SqlBackend)
                          => Key User
                          -> [Balance]
                          -> ReaderT SqlBackend m [Balance]
changeBalancePerspectives uk xs
    | length xs <= 5 = mapM (changeBalancePerspective uk) xs
    | otherwise = do
        as <- P.selectList [] []
        ass <- fmap entityVal <$> P.selectList [] []
        return $ changeBalancePerspectiveCached as ass uk <$> xs

-- returns Nothing if the user does not have access to the expense.
getRenderedExpense :: (MonadIO m, BaseBackend SqlBackend ~ SqlBackend)
                   => Key User
                   -> Entity Expense
                   -> ReaderT SqlBackend m (Maybe RenderedExpense)
getRenderedExpense uk x@(Entity ek _) = do
    (e, sharing) <- changePerspective uk x
    flags <- fmap (expenseFlagFlagging . entityVal)
        <$> P.selectList [ ExpenseFlagExpenseId P.==. ek ] []
    return $ renderExpense e sharing flags

getRenderedExpenseCached
    :: MonadIO m
    => [Entity Account]
    -> [AccountSyncing]
    -> [CategoryReplacement]
    -> [ExpenseSharing]
    -> [ExpenseFlag]
    -> Key User
    -> Entity Expense
    -> ReaderT SqlBackend m (Maybe RenderedExpense)
getRenderedExpenseCached as ass catr sharing fs uk x@(Entity ek _) = do
    (e, sharing') <- changePerspectiveCached as ass catr sharing uk x
    let flags = expenseFlagFlagging
            <$> filter ((==) ek . expenseFlagExpenseId) fs
    return $ renderExpense e sharing' flags

-- essentially `mapM getRenderedExpense`, but retrieves all accounts, syncings and category replacements before. This is a lot faster when retrieving a lot of expenses.
getRenderedExpenses :: MonadIO m
                    => Key User
                    -> [Entity Expense]
                    -> ReaderT SqlBackend m [RenderedExpense]
getRenderedExpenses = getRenderedExpenses' False

-- essentially `mapM getRenderedExpense`, but retrieves all accounts, syncings and category replacements before. This is a lot faster when retrieving a lot of expenses.
getRenderedExpenses' :: MonadIO m
                     => Bool
                     -> Key User
                     -> [Entity Expense]
                     -> ReaderT SqlBackend m [RenderedExpense]
getRenderedExpenses' light uk xs
    | length xs <= 5 = catMaybes <$> mapM (getRenderedExpense uk) xs
    | otherwise = do
        as <- P.selectList [] []
        ass <- fmap entityVal <$> P.selectList [] []
        catr <- fmap entityVal <$> P.selectList [] []
        sharing <- fmap (fmap entityVal) $ limitVariables (entityKey <$> xs) $
            \vals -> select $ from $ \s -> do
                where_ $ (s ^. ExpenseSharingExpenseId) `in_` vals
                return s
        flags
            <- if light
               then return []
               else fmap (fmap entityVal) $ limitVariables (entityKey <$> xs) $
                   \vals -> select $ from $ \s -> do
                       where_ $ (s ^. ExpenseFlagExpenseId) `in_` vals
                       return s
        catMaybes
            <$> mapM (getRenderedExpenseCached as ass catr sharing flags uk) xs

getRenderedCategory :: MonadIO m
                    => Key User
                    -> Entity Category
                    -> ReaderT SqlBackend m RenderedCategory
getRenderedCategory uk c = do
    rs <- select $ from $ \catr -> do
        where_ $ ((catr ^. CategoryReplacementOwnerId) ==. val uk)
            &&. ((catr ^. CategoryReplacementReplacement) ==. val (entityKey c))
        return $ catr ^. CategoryReplacementOriginal
    return $ convertCategory (entityVal c) (fmap unValue rs)

getRenderedAccount :: MonadIO m
                   => Entity Account
                   -> ReaderT SqlBackend m RenderedAccount
getRenderedAccount c = do
    rs <- select $ from $ \acs -> do
        where_ $ ((acs ^. AccountSyncingAccount1) ==. val (entityKey c))
            ||. ((acs ^. AccountSyncingAccount2) ==. val (entityKey c))
        return acs
    return $ convertAccount (entityVal c) (entityVal <$> listToMaybe rs)

getRenderedDevice :: MonadIO m
                  => Entity Device
                  -> ReaderT SqlBackend m RenderedDevice
getRenderedDevice c = do
    rs <- select $ from $ \ef -> do
        where_ $ (ef ^. ExpenseFilterDeviceId) ==. val (entityKey c)
        return ef
    return $ convertDevice (entityVal c) (entityVal <$> rs)

getRenderedStockExchange :: MonadIO m
                         => Entity StockExchange
                         -> ReaderT SqlBackend m (Record RenderedStockExchange)
getRenderedStockExchange ex = do
    cnt <- select $ from $ \x -> do
        where_ $ (x ^. StockHistoricalPriceExchangeId) ==. val (entityKey ex)
        return countRows
    firstR <- select $ from $ \x -> do
        where_ $ (x ^. StockHistoricalPriceExchangeId) ==. val (entityKey ex)
        orderBy [ asc $ x ^. StockHistoricalPriceDay ]
        limit 1
        return x
    lastR <- select $ from $ \x -> do
        where_ $ (x ^. StockHistoricalPriceExchangeId) ==. val (entityKey ex)
        orderBy [ desc $ x ^. StockHistoricalPriceDay ]
        limit 1
        return x
    lastC <- select $ from $ \x -> do
        where_ $ (x ^. StockCurrentPriceExchangeId) ==. val (entityKey ex)
        orderBy [ desc $ x ^. StockCurrentPriceDate ]
        limit 1
        return x
    let firstR' = listToMaybe $ fmap entityToRecord firstR
    let lastR' = listToMaybe $ fmap entityToRecord lastR
    let lastC' = listToMaybe $ fmap entityToRecord lastC
    let totalCount = head $ (unValue <$> cnt) ++ [ 0 ]
    let StockExchange _ a b c d = entityVal ex
    return . Record (fromSqlKey $ entityKey ex) $ RS.RenderedStockExchange a b c d totalCount firstR' lastR' lastC'

getRenderedStock :: MonadIO m
                 => Entity Stock
                 -> ReaderT SqlBackend m RenderedStock
getRenderedStock s = do
    si <- select $ from $ \ex -> do
        where_ $ (ex ^. StockInfoIsin) ==. val (stockIsin $ entityVal s)
        return ex
    if null si
        then return $ RS.RenderedStock (stockIsin $ entityVal s) Nothing []
        else do
            let si' = head si
            exs <- select $ from $ \ex -> do
                where_ $ (ex ^. StockExchangeStockId) ==. val (entityKey si')
                return ex
            rexs <- mapM getRenderedStockExchange exs
            return $ RS.RenderedStock (stockIsin $ entityVal s)
                                      (Just $ entityToRecord si')
                                      rexs

validateRenderedExpense :: MonadIO m
                        => Entity User
                        -> RenderedExpense
                        -> ReaderT SqlBackend m (Maybe RenderedExpense)
validateRenderedExpense u rexp = runMaybeT $ do
    let uid = entityKey u
    -- * one of the accounts has to belong to the user or is synced with one of his
    ownAccounts <- fmap (fmap unValue) <$> lift $ select $ from $ \account -> do
        where_ $ (account ^. AccountOwnerId) ==. val uid
        return (account ^. AccountId)
    let ownAccountsSub = subList_select $ from $ \account -> do
            where_ $ (account ^. AccountOwnerId) ==. val uid
            return (account ^. AccountId)
    syncedAccounts1
        <- fmap (fmap unValue) <$> lift $ select $ from $ \accountsync -> do
            where_ $ (accountsync ^. AccountSyncingAccount1)
                `in_` ownAccountsSub
            return (accountsync ^. AccountSyncingAccount2)
    syncedAccounts2
        <- fmap (fmap unValue) <$> lift $ select $ from $ \accountsync -> do
            where_ $ (accountsync ^. AccountSyncingAccount2)
                `in_` ownAccountsSub
            return (accountsync ^. AccountSyncingAccount1)
    let accEffect x = x `elem` ownAccounts || x `elem` syncedAccounts1
            || x `elem` syncedAccounts2
    when ((not . accEffect $ RE.accountId rexp)
          && not (any (accEffect . RE.sharingAccountId) (RE.sharing rexp))) $
        fail ""
    uids <- case userGroupId (entityVal u) of
        Nothing -> return [ uid ]
        Just gid -> fmap (fmap unValue) <$> lift $ select $ from $ \user -> do
            where_ $ (user ^. UserGroupId) ==. just (val gid)
            return (user ^. UserId)
    -- * Account has to be visible by user
    acc <- MaybeT $ get (RE.accountId rexp)
    unless (accountOwnerId acc `elem` uids) $ fail ""
    -- * so have to be shared accounts
    saccs <- mapM (MaybeT . get) (RE.sharingAccountId <$> RE.sharing rexp)
    unless (all (\x -> accountOwnerId x `elem` uids) saccs) $ fail ""
    -- * ... and categories
    cat <- MaybeT $ get (RE.categoryId rexp)
    unless (categoryOwnerId cat `elem` uids) $ fail ""
    return rexp
