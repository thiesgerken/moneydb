{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

module Moneydb.Computation.Overview(Overview(..), Overviews(..), computeOverview, computeOverview', computeOverviews) where

import           Control.Monad.IO.Unlift         (MonadUnliftIO, liftIO)
import           Control.Monad.Trans.Reader      (ReaderT)
import           Control.Monad.Trans.Resource    (ResourceT)
import           Data.Aeson.TH
import           Data.Decimal                    (Decimal)
import           Data.Swagger                    (ToSchema (..), defaultSchemaOptions, genericDeclareNamedSchema)
import qualified Data.Swagger                    as Swagger
import           Data.Time
import           Database.Esqueleto
import           GHC.Generics                    (Generic)
import           Safe                            (minimumBound)

import           Moneydb.Computation.Simulation
import           Moneydb.Computation.Statistic
import           Moneydb.Database
import           Moneydb.Types
import qualified Moneydb.Types.Rendering.Expense as RE

data Overview =
      Overview {ovStartDate         :: UTCTime,
                ovEndDate           :: UTCTime,
                ovStartBalances     :: Simulation,  -- ^ Balances at the start of the month (best approx)
                ovEndBalances       :: Simulation,   -- ^ Balances at the end of the month (best approx)
                ovEndBalancesApprox :: Simulation, -- ^ Balances at the end of the month (based on expenses + startBalances)
                ovExpenseCount      :: Int,
                ovStatsExps         :: Statistic, -- ^ stats for the n most important categories
                ovStatsIncome       :: Statistic -- ^ stats for the n most important categories
              } deriving Generic

data Overviews = Overviews {ovsMonthly :: [Overview], ovsStatsExps :: Statistic, ovsStatsIncome :: Statistic } deriving Generic

instance ToSchema Overview where declareNamedSchema = genericDeclareNamedSchema $ defaultSchemaOptions { Swagger.fieldLabelModifier = prefixModifier }
$(deriveToJSON defaultOptions{fieldLabelModifier = prefixModifier} ''Overview)

instance ToSchema Overviews where declareNamedSchema = genericDeclareNamedSchema $ defaultSchemaOptions { Swagger.fieldLabelModifier = prefixModifier }
$(deriveToJSON defaultOptions{fieldLabelModifier = prefixModifier} ''Overviews)

getMonths :: Day -> Day -> [(Integer, Int)]
getMonths s e = (\(y,m,_) -> (y,m)) . toGregorian <$> (takeWhile (e >=) . iterate (addGregorianMonthsClip 1) $ st)
  where st = fromGregorian sy sm 1
        (sy, sm, _) = toGregorian s

monthStart :: Integer -> Int -> IO UTCTime
monthStart y m = do
    tz <- liftIO . getTimeZone $ UTCTime (fromGregorian y m 1) 0
    return . localTimeToUTC tz $ LocalTime (fromGregorian y m 1) midnight

computeOverviews :: MonadUnliftIO m => Entity User -> Int -> Int -> ReaderT SqlBackend (ResourceT m) Overviews
computeOverviews u@(Entity uid _) ecount ccount = do
  bs <- listReadableElements u Nothing Nothing
  as <- select $ from $ \a -> do where_ $ (a ^. AccountOwnerId) ==. val uid; return a
  cs <- listReadableElements u Nothing Nothing
  es <- select $ from $ \e -> do
       where_ $ expenseVisible uid e
       return (e ^. ExpenseId, e ^. ExpenseTitle, e ^. ExpenseAmount, e ^. ExpenseAccountId, e ^. ExpenseCategoryId, e ^. ExpenseValueDate)
  rexps <- getRenderedExpenses' True uid (lightExpense uid <$> es)

  today <- localDay . zonedTimeToLocalTime <$> liftIO getZonedTime
  now <- liftIO getCurrentTime
  let minDate = minimumBound now (balanceDate . entityVal <$> bs)
  minTZ <- liftIO $ getTimeZone minDate
  let minDay = localDay $ utcToLocalTime minTZ minDate

  -- includes all month starts including the start of the next month
  times <- liftIO . mapM (uncurry monthStart) $ getMonths minDay (addGregorianMonthsClip 1 today)
  let sims = simulateCached as bs rexps Both <$> times
  let sims' = zip sims (drop 1 sims)
  let monthly = reverse $ uncurry (computeOverviewCached ecount ccount as rexps cs) <$> sims'

  -- all expenses that are includes in the monthly overviews
  let rexps' = if null times then [] else filter (\e -> RE.valueDate e >= head times && RE.valueDate e < last times) rexps
  let sInc = getStatistic (0 >) ecount ccount cs rexps'
  let sExp = getStatistic (0 <) ecount ccount cs rexps'

  return $ Overviews monthly sExp sInc

computeOverviewCached :: Int -> Int -> [Entity Account] -> [RenderedExpense] -> [Entity Category]
                    -> Simulation -> Simulation -> Overview
computeOverviewCached ecount ccount as es cs st en
       = Overview (simulationDate st) (simulationDate en) st en endSimApprox (length es') sExp sInc
         where endSimApprox = extendSimulation st (simulationDate en) as es'
               sInc = getStatistic (0 >) ecount ccount cs es'
               sExp = getStatistic (0 <) ecount ccount cs es'
               es' = filter (\e -> RE.valueDate e >= simulationDate st && RE.valueDate e < simulationDate en) es

computeOverview :: MonadUnliftIO m => Entity User -> Int -> Int -> UTCTime -> UTCTime -> ReaderT SqlBackend (ResourceT m) Overview
computeOverview u@(Entity uid _) ecount ccount st en = do
   startSim <- simulate u Both st
   endSim <- simulate u Both en

   as <- select $ from $ \a -> do where_ $ (a ^. AccountOwnerId) ==. val uid; return a
   cs <- select $ from $ \c -> do where_ $ (c ^. CategoryOwnerId) ==. val uid; return c
   es <- select $ from $ \e -> do
         where_ $ (e ^. ExpenseValueDate) >=. val st &&. (e ^. ExpenseValueDate) <. val en
             &&. expenseVisible uid e
         return (e ^. ExpenseId, e ^. ExpenseTitle, e ^. ExpenseAmount, e ^. ExpenseAccountId, e ^. ExpenseCategoryId, e ^. ExpenseValueDate)

   rexps <- getRenderedExpenses' True uid (lightExpense uid <$> es)

   let endSimApprox = extendSimulation startSim en as rexps
   let sInc = getStatistic (0 >) ecount ccount cs rexps
   let sExp = getStatistic (0 <) ecount ccount cs rexps

   return $ Overview st en startSim endSim endSimApprox (length rexps) sExp sInc

lightExpense :: UserId -> (Value ExpenseId, Value String, Value Decimal, Value AccountId, Value CategoryId, Value UTCTime) -> Entity Expense
lightExpense uid (Value eid, Value title, Value amount, Value aid, Value cid, Value vd) = Entity eid $ Expense uid amount vd Nothing aid cid title "" "" "" "" d d "" uid
 where d = UTCTime (fromGregorian 2000 1 1) (secondsToDiffTime 0)

computeOverview' :: MonadUnliftIO m => Entity User -> Int -> Int -> Integer -> Int -> ReaderT SqlBackend (ResourceT m) Overview
computeOverview' u ecount ccount y m = do
    stTZ <- liftIO . getTimeZone $ UTCTime (fromGregorian y m 1) 0
    enTZ <- liftIO . getTimeZone $ UTCTime (addGregorianMonthsClip 1 (fromGregorian y m 1)) 0

    let st = localTimeToUTC stTZ $ LocalTime (fromGregorian y m 1) midnight
    let en = localTimeToUTC enTZ $ LocalTime (addGregorianMonthsClip 1 (fromGregorian y m 1)) midnight

    computeOverview u ecount ccount st en
