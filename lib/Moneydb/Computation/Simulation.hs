{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Moneydb.Computation.Simulation(simulate, simulate', simulates, simulateCached, extendSimulation, SimulatedBalance(..), Simulation(..), SearchDirection(..), Simulations(..), ByAccount(..), ByAvailability(..)) where

import           Control.Monad.IO.Class          (MonadIO (..))
import           Control.Monad.IO.Unlift         (MonadUnliftIO)
import           Control.Monad.Trans.Reader      (ReaderT)
import           Control.Monad.Trans.Resource    (ResourceT)
import           Data.Aeson                      (FromJSON, ToJSON)
import           Data.Aeson.TH
import           Data.Decimal                    (Decimal)
import           Data.Foldable                   (foldl', foldr')
import           Data.Int                        (Int64)
import           Data.Map                        (Map)
import qualified Data.Map                        as Map
import           Data.Maybe                      (fromMaybe, listToMaybe)
import           Data.Ord                        (comparing)
import           Data.Swagger                    (ToParamSchema, ToSchema (..), defaultSchemaOptions,
                                                  genericDeclareNamedSchema)
import qualified Data.Swagger                    as Swagger
import           Data.Text                       (pack, unpack)
import           Data.Time
import           Database.Esqueleto
import           GHC.Generics                    (Generic)
import           Safe                            (minimumBound, minimumByMay)
import           Text.Read                       (readEither)
import           Web.HttpApiData                 (FromHttpApiData (..))

import           Moneydb.Database
import           Moneydb.Types
import qualified Moneydb.Types.Rendering.Expense as RE

data SearchDirection = Future | Past | Both deriving (Generic, Read, FromJSON, ToJSON, ToParamSchema)

instance FromHttpApiData SearchDirection where
  parseQueryParam x = case readEither (unpack x) of
    Left s  -> Left (pack s)
    Right a -> Right a

data SimulatedBalance = SimulatedBalance {simulatedBase :: Maybe SimulationBase, simulatedAccountId :: Int64,  simulatedAmount :: Maybe Decimal, simulatedSteps :: Maybe Int} deriving Generic

data SimulationBase = SimulationBase {baseId :: Int64, baseAmount :: Decimal, baseDate :: UTCTime} deriving Generic

data Available = Available {availableWhen :: !Availability, availableAmount :: Maybe Decimal} deriving Generic

data Simulation = Simulation {simulationBalances :: [SimulatedBalance], simulationTotal :: [Available], simulationDate :: UTCTime} deriving Generic

data ByAccount = ByAccount {byAccountId :: Int64, byAccountData :: [Maybe Decimal]} deriving Generic
data ByAvailability = ByAvailability {byAvailabilityWhen :: Availability, byAvailabilityData :: [Maybe Decimal]} deriving Generic

-- less data and more efficient storage than [Simulation]
data Simulations = Simulations {simulationDates :: [UTCTime], simulationByAccount :: [ByAccount], simulationByAvailability :: [ByAvailability]} deriving Generic

instance ToSchema Simulation where declareNamedSchema = genericDeclareNamedSchema $ defaultSchemaOptions { Swagger.fieldLabelModifier = prefixModifier }
$(deriveToJSON defaultOptions{fieldLabelModifier = prefixModifier} ''Simulation)

instance ToSchema SimulationBase where declareNamedSchema = genericDeclareNamedSchema $ defaultSchemaOptions { Swagger.fieldLabelModifier = prefixModifier }
$(deriveToJSON defaultOptions{fieldLabelModifier = prefixModifier} ''SimulationBase)

instance ToSchema Available where declareNamedSchema = genericDeclareNamedSchema $ defaultSchemaOptions { Swagger.fieldLabelModifier = prefixModifier }
$(deriveToJSON defaultOptions{fieldLabelModifier = prefixModifier} ''Available)

instance ToSchema ByAccount where declareNamedSchema = genericDeclareNamedSchema $ defaultSchemaOptions { Swagger.fieldLabelModifier = prefixModifier'' "ByAccount" }
$(deriveToJSON defaultOptions{fieldLabelModifier = prefixModifier'' "ByAccount"} ''ByAccount)

instance ToSchema ByAvailability where declareNamedSchema = genericDeclareNamedSchema $ defaultSchemaOptions { Swagger.fieldLabelModifier = prefixModifier'' "ByAvailability" }
$(deriveToJSON defaultOptions{fieldLabelModifier = prefixModifier'' "ByAvailability"} ''ByAvailability)

instance ToSchema Simulations where declareNamedSchema = genericDeclareNamedSchema $ defaultSchemaOptions { Swagger.fieldLabelModifier = prefixModifier }
$(deriveToJSON defaultOptions{fieldLabelModifier = prefixModifier} ''Simulations)

instance ToSchema SimulatedBalance where declareNamedSchema = genericDeclareNamedSchema $ defaultSchemaOptions { Swagger.fieldLabelModifier = prefixModifier }
$(deriveToJSON defaultOptions{fieldLabelModifier = prefixModifier} ''SimulatedBalance)

extend :: RenderedExpense -> Map AccountId (Decimal, Int) -> Map AccountId (Decimal, Int)
extend e = Map.mapWithKey f
   where f aid (amount, steps) = (amount - impact aid e, if impact aid e == 0 then steps else steps + 1)

manipTotals :: Map AccountId Account -> AccountId -> (Decimal, Int) -> [Available] -> [Available]
manipTotals as aid (inc, _) avs
  | Just a <- Map.lookup aid as = let upd (Available av x) = Available av $ fmap (if av >= accountAvailability a then (+) inc else id) x in upd <$> avs
  | otherwise = avs

manipBals :: AccountId -> (Decimal, Int) -> [SimulatedBalance] -> [SimulatedBalance]
manipBals aid (inc, steps) bals = upd <$> bals
      where upd b@(SimulatedBalance _ aid' (Just x) (Just y)) = if aid' == fromSqlKey aid then b {simulatedAmount = Just (x + inc), simulatedSteps = Just (y + steps)} else b
            upd x = x

extendSimulation :: Simulation -> UTCTime -> [Entity Account] -> [RenderedExpense] -> Simulation
extendSimulation s t as es = Simulation bals totals (simulationDate s)
  where diff = foldr' extend (Map.fromList $ (\x -> (entityKey x, (0, 0))) <$> as) es'
        as' = Map.fromList $ (\x -> (entityKey x, entityVal x)) <$> as
        es' = filter (\e -> RE.valueDate e < t && RE.valueDate e >= simulationDate s) es
        totals = Map.foldrWithKey' (manipTotals as') (simulationTotal s) diff
        bals = Map.foldrWithKey' manipBals (simulationBalances s) diff

-- account has to belong to the user
nearestBalance :: MonadIO m => Bool -> Entity User -> AccountId -> [AccountSyncing] -> UTCTime -> ReaderT SqlBackend m [Entity Balance]
nearestBalance future u aid as t = do
          let s1 = accountSyncingAccount2 <$> filter ((==) aid . accountSyncingAccount1) as
          let s2 = accountSyncingAccount1 <$> filter ((==) aid . accountSyncingAccount2) as

          bs <- select $ from $ \bal -> do
              where_ $ (if future then (>=.) else (<=.)) (bal ^. BalanceDate) (val t)
                   &&. ( ((bal ^. BalanceAccountId) ==. val aid)
                          ||. ((bal ^. BalanceAccountId) `in_` valList (s1++s2)) )
              orderBy [(if future then asc else desc) (bal ^. BalanceDate)]
              limit 1
              return bal
          zipWith Entity (entityKey <$> bs) <$> changeBalancePerspectives (entityKey u) (entityVal <$> bs)

-- direction == Past -> only balances with date <= t are considered
findNearestBalance :: MonadUnliftIO m => Entity User -> AccountId -> [AccountSyncing] -> UTCTime -> SearchDirection -> ReaderT SqlBackend (ResourceT m) (Maybe (Entity Balance))
findNearestBalance u aid as t Past = listToMaybe <$> nearestBalance False u aid as t
findNearestBalance u aid as t Future = listToMaybe <$> nearestBalance True u aid as t
findNearestBalance u aid as t Both = do
        past <- nearestBalance False u aid as t
        future <- nearestBalance True u aid as t

        return . minimumByMay (comparing (abs . diffUTCTime t . balanceDate . entityVal)) $ past ++ future

-- direction == Past -> only balances with date <= t are considered
-- input balances are assumed to be rendered (i.e. we only have to look for AccountId)
findNearestBalanceCached :: AccountId -> [Entity Balance] -> UTCTime -> SearchDirection -> Maybe (Entity Balance)
findNearestBalanceCached a bs t Past = findNearestBalanceCached a (filter ((>=) t . balanceDate . entityVal) bs) t Both
findNearestBalanceCached a bs t Future = findNearestBalanceCached a (filter ((<=) t . balanceDate . entityVal) bs) t Both
findNearestBalanceCached a bs t Both =
        minimumByMay (comparing (abs . diffUTCTime t . balanceDate . entityVal)) $ filter ((==) a . balanceAccountId . entityVal) bs

simulate ::  MonadUnliftIO m => Entity User -> SearchDirection -> UTCTime -> ReaderT SqlBackend (ResourceT m) Simulation
simulate u@(Entity uid _) dir t = do
  accs <- select $ from $ \a -> do
        where_ $ (a ^. AccountOwnerId) ==. val uid
        orderBy [asc $ a ^. AccountTitle]
        return a
  as <- select $ from $ \x -> return x

  bals' <- mapM (\a -> simulateBalance u (entityKey a) (entityVal <$> as) t dir) accs
  let bals = zip accs bals'
  let totals = fmap (\av -> Available av . fmap sum . mapM (simulatedAmount . snd) $ filter ((>=) av . accountAvailability . entityVal . fst) bals) [minBound..]
  return $ Simulation (snd <$> filter (not . accountHidden . entityVal . fst) bals) totals t

simulates ::  MonadUnliftIO m => Entity User -> SearchDirection -> Maybe UTCTime -> Maybe UTCTime -> Int -> ReaderT SqlBackend (ResourceT m) Simulations
simulates u@(Entity uid _) dir ta' tb' ppm = do
  as <- select $ from $ \a -> do where_ $ (a ^. AccountOwnerId) ==. val uid; return a
  bs <- listReadableElements u Nothing Nothing
  es <- select $ from $ \e -> do
       where_ $ expenseVisible uid e
       return (e ^. ExpenseId, e ^. ExpenseAmount, e ^. ExpenseAccountId, e ^. ExpenseValueDate)
  rexps <- getRenderedExpenses' True uid (lightExpense' uid <$> es)

  now <- liftIO getCurrentTime
  let minDate = minimumBound now (balanceDate . entityVal <$> bs)

  let ta = fromMaybe minDate ta'
  let tb = fromMaybe now tb'

  lta <- liftIO $ localDay' ta
  ltb <- liftIO $ localDay' tb
  ms <- mapM (liftIO . midnightLocal) $ getMonths lta ltb
  let times = filter (\t -> t >= ta && t <= tb) . concatMap (uncurry $ partition ppm) $ zip ms (drop 1 ms)

  let sims = reverse $ simulateCached as bs rexps dir <$> times
  let (byAcc, byAv) = if null sims then ([], []) else foldl' simsFolder (simsStart $ head sims) (tail sims)
  return $ Simulations times byAcc byAv

simsStart :: Simulation -> ([ByAccount], [ByAvailability])
simsStart (Simulation bals tots _) = (cB <$> bals, cT <$> tots)
    where cB (SimulatedBalance _ ac a _) = ByAccount ac [a]
          cT (Available w a) = ByAvailability w [a]

simsFolder :: ([ByAccount], [ByAvailability]) -> Simulation -> ([ByAccount], [ByAvailability])
simsFolder (byAc, byAv) (Simulation bals tots _) = (zipWith addAc byAc bals, zipWith addAv byAv tots)
    where addAv (ByAvailability bw ba) (Available w a)
              | bw == w = ByAvailability bw (a : ba)
              | otherwise = error "simsFolder :: wrong zipping"
          addAc (ByAccount bac ba) (SimulatedBalance _ ac a _)
              | bac == ac = ByAccount bac (a : ba)
              | otherwise = error "simsFolder :: wrong zipping"

partition :: Int -> UTCTime -> UTCTime -> [UTCTime]
partition ppm ta tb = take ppm $ iterate (addUTCTime snds) ta
   where snds = (tb `diffUTCTime` ta) / fromIntegral ppm'
         ppm' = max ppm 1

-- given a start day and end day, returns the first of every month between those days, plus the first of the months before and after that.
getMonths :: Day -> Day -> [Day]
getMonths s e = takeWhile (en >=) . iterate (addGregorianMonthsClip 1) $ st
  where st = fromGregorian sy sm 1
        en = addGregorianMonthsClip 1 e
        (sy, sm, _) = toGregorian s

midnightLocal :: Day -> IO UTCTime
midnightLocal d = do
    tz <- getTimeZone $ UTCTime d 0
    return . localTimeToUTC tz $ LocalTime d midnight

localDay' :: UTCTime -> IO Day
localDay' t = do
    tz <- getTimeZone t
    return . localDay $ utcToLocalTime tz t

simulate' ::  MonadUnliftIO m => Entity User -> SearchDirection -> UTCTime -> ReaderT SqlBackend (ResourceT m) Simulation
simulate' u@(Entity uid _) dir t = do
   bs <- listReadableElements u Nothing Nothing
   as <- select $ from $ \a -> do where_ $ (a ^. AccountOwnerId) ==. val uid; return a
   es <- select $ from $ \e -> do
         where_ $ expenseVisible uid e
         return (e ^. ExpenseId, e ^. ExpenseAmount, e ^. ExpenseAccountId, e ^. ExpenseValueDate)
   rexps <- getRenderedExpenses' True uid (lightExpense' uid <$> es)

   return $ simulateCached as bs rexps dir t

simulateCached :: [Entity Account] -> [Entity Balance] -> [RenderedExpense] -> SearchDirection -> UTCTime -> Simulation
simulateCached as bs rexps dir t = Simulation (snd <$> filter (not . accountHidden . entityVal . fst) bals) totals t
  where bals = (\a -> (a, simulateBalanceCached (entityKey a) bs rexps t dir)) <$> as
        totals = fmap (\av -> Available av . fmap sum . mapM (simulatedAmount . snd) $ filter ((>=) av . accountAvailability . entityVal . fst) bals) [minBound..]

simulateBalance :: MonadUnliftIO m => Entity User -> AccountId -> [AccountSyncing] -> UTCTime -> SearchDirection -> ReaderT SqlBackend (ResourceT m) SimulatedBalance
simulateBalance u a as t dir = do
  b <- findNearestBalance u a as t dir
  case b of
    Just b' -> simulateBalance' t u as b'
    Nothing -> return $ SimulatedBalance Nothing (fromSqlKey a) Nothing Nothing

-- input balances are assumed to be rendered
simulateBalanceCached :: AccountId -> [Entity Balance] -> [RenderedExpense] -> UTCTime -> SearchDirection -> SimulatedBalance
simulateBalanceCached a bs es t dir = case b of
        Just b' -> simulateBalanceCached' t es b'
        Nothing -> SimulatedBalance Nothing (fromSqlKey a) Nothing Nothing
  where b = findNearestBalanceCached a bs t dir

simulateBalanceCached' :: UTCTime -> [RenderedExpense] -> Entity Balance -> SimulatedBalance
simulateBalanceCached' t es (Entity bid b') = SimulatedBalance (Just $ SimulationBase (fromSqlKey bid) (balanceAmount b') (balanceDate b')) (fromSqlKey $ balanceAccountId b') (Just amount) (Just $ length rexps)
   where op = if balanceDate b' < t then (-) else (+)
         rexps = filter ((/=) 0 . impact (balanceAccountId b')) $ filter fop es
         amount = balanceAmount b' `op` sum (fmap (impact (balanceAccountId b')) rexps)
         fop x = if t < balanceDate b' then RE.valueDate x >= t && RE.valueDate x < balanceDate b'
                 else RE.valueDate x < t && RE.valueDate x >= balanceDate b'

-- | simulate a balance for a given time based on another balance used as reference and expenses since then / until then
-- balance account must belong to user (use changePerspective first)
simulateBalance' :: MonadUnliftIO m => UTCTime -> Entity User -> [AccountSyncing] -> Entity Balance -> ReaderT SqlBackend (ResourceT m) SimulatedBalance
simulateBalance' t u as (Entity bid b') = do
    let s1 = accountSyncingAccount2 <$> filter ((==) (balanceAccountId b') . accountSyncingAccount1) as
    let s2 = accountSyncingAccount1 <$> filter ((==) (balanceAccountId b') . accountSyncingAccount2) as
    let s = valList $ balanceAccountId b' : (s1 ++ s2)

    es' <- select $ from $ \e -> do
          where_ $ (if t < balanceDate b' then (e ^. ExpenseValueDate) >=. val t &&. (e ^. ExpenseValueDate) <. val (balanceDate b') else (e ^. ExpenseValueDate) <. val t &&. (e ^. ExpenseValueDate) >=. val (balanceDate b') )
              &&. ( ((e ^. ExpenseAccountId) `in_` s)
                  ||. exists (from $ \es -> do
                      where_ $ ((es ^. ExpenseSharingAccountId) `in_` s)
                            &&. ((es ^. ExpenseSharingExpenseId) ==. (e ^. ExpenseId))
                      return ()    ) )
          return (e ^. ExpenseId, e ^. ExpenseAmount, e ^. ExpenseAccountId)
    let es = lightExpense (entityKey u) <$> es'

    rexps <- getRenderedExpenses' True (entityKey u) es
    let op = if balanceDate b' < t then (-) else (+)
    let amount = balanceAmount b' `op` sum (fmap (impact (balanceAccountId b')) rexps)
    return $ SimulatedBalance (Just $ SimulationBase (fromSqlKey bid) (balanceAmount b') (balanceDate b')) (fromSqlKey $ balanceAccountId b') (Just amount) (Just $ length rexps)

lightExpense :: UserId -> (Value ExpenseId, Value Decimal, Value AccountId) -> Entity Expense
lightExpense uid (Value eid, Value amount, Value aid) = Entity eid $ Expense uid amount d Nothing aid (toSqlKey 0) "" "" "" "" "" d d "" uid
  where d = UTCTime (fromGregorian 2000 1 1) (secondsToDiffTime 0)

lightExpense' :: UserId -> (Value ExpenseId, Value Decimal, Value AccountId, Value UTCTime) -> Entity Expense
lightExpense' uid (Value eid, Value amount, Value aid, Value vd) = Entity eid $ Expense uid amount vd Nothing aid (toSqlKey 0) "" "" "" "" "" d d "" uid
  where d = UTCTime (fromGregorian 2000 1 1) (secondsToDiffTime 0)

impact :: AccountId -> RenderedExpense -> Decimal
impact aid e = i1 + sum (i2 <$> RE.sharing e)
   where i1 = if RE.accountId e == aid then RE.amount e else 0
         i2 s = if RE.sharingAccountId s == aid then RE.calculatedAmount s else 0
