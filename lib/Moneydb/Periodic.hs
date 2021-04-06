{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Moneydb.Periodic(checkPeriodicExpenses) where

import           Control.Monad              (unless)
import           Control.Monad.IO.Class     (MonadIO (..), liftIO)
import           Control.Monad.IO.Unlift    (MonadUnliftIO)
import           Control.Monad.Logger
import           Control.Monad.Trans.Reader (ReaderT)
import           Data.List.Utils            (replace)
import qualified Data.Map                   as Map
import           Data.Maybe                 (fromMaybe)
import qualified Data.Text                  as T
import           Data.Time
import           Database.Persist           hiding (replace)
import           Database.Persist.Sqlite    (SqlBackend)

import           Moneydb.Config
import           Moneydb.Database
import           Moneydb.Types

getTimes :: UTCTime -> Interval -> UTCTime -> UTCTime -> [UTCTime]
getTimes base p start end = takeWhile (end >=) . dropWhile (start >=) . iterate (next p) $ base

next :: Interval -> UTCTime -> UTCTime
next Daily (UTCTime d t)   = UTCTime (addDays 1 d) t
next Weekly (UTCTime d t)  = UTCTime (addDays 7 d) t
next Monthly (UTCTime d t) = UTCTime (addGregorianMonthsClip 1 d) t
next Yearly (UTCTime d t)  = UTCTime (addGregorianMonthsClip 12 d) t

adjust :: UTCTime -> UTCTime -> IO UTCTime
adjust tpl ut = do
  tz <- getTimeZone tpl
  let lut = utcToLocalTime tz ut

  ctz <- getCurrentTimeZone
  return $ localTimeToUTC ctz lut

instantiate :: Expense -> PeriodicRule -> UTCTime -> Expense
instantiate tpl rule t@(UTCTime dd _) = tpl {expenseValueDate = t, expenseBookingDate = Nothing, expenseAmount = amount', expenseTitle = title'', expenseLastModifiedThrough = "moneydb-server"}
 where title' = fromMaybe (expenseTitle tpl) (periodicRuleTitleReplacement rule)
       amount' = fromMaybe (expenseAmount tpl) (periodicRuleAmountReplacement rule)
       (y, m, d) = toGregorian dd
       title'' = foldr1 (.) [replace "%d" (show d), replace "%y" (show y), replace "%m" (show m)] title'

generatePeriodicExpenses :: (MonadIO m, PersistStoreWrite backend, PersistQueryRead backend, BaseBackend backend ~ SqlBackend) => MoneyDBConn -> Entity PeriodicRule -> Expense -> ReaderT backend m Int
generatePeriodicExpenses mc rule tpl = do
  let rule' = entityVal rule
  now <- liftIO getCurrentTime
  let add = addUTCTime (fromRational . toRational $ (3600 * periodicAdvance (moneyConfig mc)))

  let ts = getTimes (expenseValueDate tpl) (periodicRulePeriod rule') (add $ periodicRuleLastCreation rule') (add now)
  ts' <- liftIO $ mapM (adjust (expenseValueDate tpl)) ts

  if null ts' then return 0
  else do
    es <- insertMany (instantiate tpl rule' <$> ts')

    -- copy sharing settings and flags (except for Template) to the new expenses
    flags <- filter ((/=) Template . expenseFlagFlagging) . fmap entityVal <$> selectList [ExpenseFlagExpenseId ==. periodicRuleTemplateId rule'] []
    sharing <- fmap entityVal <$> selectList [ExpenseSharingExpenseId ==. periodicRuleTemplateId rule'] []

    let flags' = concatMap (\e -> map (\f -> f {expenseFlagExpenseId = e}) flags) es
    _ <- insertMany flags'
    let sharing' = concatMap (\e -> map (\s -> s {expenseSharingExpenseId = e}) sharing) es
    _ <- insertMany sharing'

    unless (null es) $ update (entityKey rule) [PeriodicRuleLastCreation =. add now]
    return $ length es

checkPeriodicExpenses :: (MonadLogger m, MonadUnliftIO m) => MoneyDBConn -> m ()
checkPeriodicExpenses mc = runTransaction mc $ do
  logDebugN "Checking Periodic Rules"
  pes <- selectList ([] :: [Filter PeriodicRule]) []

  -- might not return the template for every rule, ignore the rest.
  tpls <- getMany (periodicRuleTemplateId . entityVal <$> pes)
  cnt <- sum <$> mapM (uncurry (generatePeriodicExpenses mc)) [(a, b) | (a, Just b) <- (\x -> (x, Map.lookup (periodicRuleTemplateId $ entityVal x) tpls)) <$> pes]
  unless (cnt == 0) . logInfoN $ ("Added " <> T.pack (show cnt) <> " expense" <> (if cnt /= 1 then "s" else ""))
