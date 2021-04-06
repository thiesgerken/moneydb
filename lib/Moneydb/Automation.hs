{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Moneydb.Automation(addAutomatedExpenses, refilterExpense, checkPreliminaryExpenses) where

import           Control.Monad                (filterM, unless, zipWithM)
import           Control.Monad.IO.Class       (MonadIO (..), liftIO)
import           Control.Monad.IO.Unlift      (MonadUnliftIO)
import           Control.Monad.Logger
import           Control.Monad.Trans.Reader   (ReaderT)
import           Control.Monad.Trans.Resource (ResourceT)
import           Data.Maybe                   (catMaybes)
import qualified Data.Text                    as T
import           Data.Time
import           Database.Esqueleto
import           Text.Printf                  (printf)
import           Text.Regex.TDFA              ((=~))

import           Moneydb.Config               (preliminaryMaxAge)
import           Moneydb.Database
import           Moneydb.Types

checkPreliminaryExpenses :: (MonadLogger m, MonadUnliftIO m) => MoneyDBConn -> m ()
checkPreliminaryExpenses mc = runTransaction mc $ do
  logDebugN "Checking for old preliminary expenses"

  (UTCTime dnow tnow) <- liftIO getCurrentTime
  let maxValueDate = UTCTime (addDays ((-1) * max 0 (preliminaryMaxAge (moneyConfig mc)) ) dnow) tnow

  prelims <- select $ from $ \x -> do
    where_ $ exists . from $ \flag -> where_ $ ((flag ^. ExpenseFlagExpenseId) ==. (x ^. ExpenseId)) &&. ((flag ^. ExpenseFlagFlagging) ==. val Preliminary) &&. ((x ^. ExpenseValueDate) <=. val maxValueDate)
    return x

  let cnt = length prelims

  mapM_ deleteElement (entityKey <$> prelims)
  unless (cnt == 0) . logInfoN $ ("Removed " <> T.pack (show cnt) <> " old preliminary expense" <> (if cnt /= 1 then "s" else ""))

getRules :: (MonadIO m, BackendCompatible SqlBackend backend, PersistQueryRead backend, PersistUniqueRead backend) =>
              Entity User -> ReaderT backend m [(Entity AutomationRule, Entity Expense)]
getRules u = select $ from $ \(ar, e) -> do
  where_ $ ((ar ^. AutomationRuleOwnerId) ==. val (entityKey u)) &&. ((ar ^. AutomationRuleTemplateId) ==. (e ^. ExpenseId))
  orderBy [desc $ ar ^. AutomationRulePriority]
  return (ar, e)

toSeen :: ExpenseDelivery -> SeenExpense
toSeen (ExpenseDelivery a b c d e _ f) = SeenExpense a b c (toSqlKey d) f e

addAutomatedExpenses :: MonadUnliftIO m => Entity User -> [ExpenseDelivery] ->  ReaderT SqlBackend (ResourceT m) (Maybe [ExpenseId])
addAutomatedExpenses u ds = do
  rules <- getRules u

  -- checking permissions not needed (rule cannot exist otherwise)
  -- but we have to check whether all deliveries have a rule first.
  -- otherwise, an attacker could (at least) find out if specific expenses exist!

  if any null (map (\d -> filter (isMatch d . entityVal . fst) rules) ds) then
    return Nothing
  else do
    ds' <- filterM isNew ds
    ds'' <- mapM (checkForPreliminary (entityKey u)) ds'

    let firstMatch = map (\d -> head $ filter (isMatch d . entityVal. fst) rules) ds''
    now <- liftIO getCurrentTime
    let es = zipWith (impose (entityKey u) now) (map (entityVal . snd) firstMatch) ds''

    -- set last delivery
    update $ \ar -> do
     set ar [ AutomationRuleLastDelivery =. just (val now)]
     where_ $ ar ^. AutomationRuleId `in_` valList (entityKey . fst <$> firstMatch)

    _ <- insertMany (toSeen <$> ds'')
    ids <- zipWithM copyExpense (map (entityKey . snd) firstMatch) es

    -- add preliminary tags if needed
    let flags = (\(_,i) -> ExpenseFlag i Preliminary ) <$> filter fst (zip (deliveryPreliminary <$> ds'') ids)
    _ <- insertMany flags

    return $ Just ids

-- might change the given expense and issue a delete statement to existing preliminary expenses
checkForPreliminary :: MonadUnliftIO m => UserId -> ExpenseDelivery -> ReaderT SqlBackend (ResourceT m) ExpenseDelivery
checkForPreliminary uid d
  | deliveryPreliminary d = return d
  | otherwise = do
      -- only consider own expenses, i.e. might not work if the expense has been changed by another user!
      prelims <- select $ from $ \(e, ef) -> do
          where_ $ ((e ^. ExpenseOwnerId) ==. val uid) &&. ((ef ^. ExpenseFlagExpenseId) ==. (e ^.  ExpenseId)) &&. ((ef ^. ExpenseFlagFlagging) ==. val Preliminary) &&. ((e ^. ExpenseAmount) ==. val (deliveryAmount d)) &&. ((e ^. ExpenseAccountId) ==. val (toSqlKey $ deliveryAccountId d))
          return e

      if null prelims then
        return d
      else do
        let valDates = expenseValueDate . entityVal <$> prelims
        let bookDates = catMaybes $ expenseBookingDate . entityVal <$> prelims
        let myBookDate = minimum (valDates ++ bookDates)

        mapM_ deleteElement $ entityKey <$> prelims
        let d' = d {deliveryBookingDate = Just myBookDate}
        return d'

copyExpense :: (MonadIO m, PersistStoreWrite backend, BackendCompatible SqlBackend backend, PersistQueryRead backend,
                  PersistUniqueRead backend, BaseBackend backend ~ SqlBackend) =>
                 Key Expense -> Expense -> ReaderT backend m (Key Expense)
copyExpense tplId e = do
  newId <- insert e
  sharings <- select $ from $ \es -> do
                where_ $ es ^. ExpenseSharingExpenseId ==. val tplId
                return es
  let sharings' = fmap (\s -> (entityVal s) {expenseSharingExpenseId = newId}) sharings
  _ <- insertMany sharings'

  flags <- select $ from $ \es -> do
                where_ $ (es ^. ExpenseFlagExpenseId ==. val tplId) &&. (es ^. ExpenseFlagFlagging !=. val Template)
                return es
  let flags' = fmap (\s -> (entityVal s) {expenseFlagExpenseId = newId}) flags
  _ <- insertMany flags'

  return newId

isNew :: (MonadIO m, BackendCompatible SqlBackend backend, PersistQueryRead backend, PersistUniqueRead backend) =>
           ExpenseDelivery -> ReaderT backend m Bool
isNew e = do
  seen <- select $ from $ \se -> do
     where_ $ ((se ^. SeenExpenseAmount) ==. val (deliveryAmount e)) &&. ((se ^. SeenExpenseAccountId) ==. val (toSqlKey $ deliveryAccountId e)) &&. ((se ^. SeenExpensePreliminary) ==. val (deliveryPreliminary e))
     return se

  return . not $ any (\x -> abs (diffUTCTime (seenExpenseValueDate $ entityVal x) (deliveryValueDate e)) < 24*3600 ) seen

impose :: UserId -> UTCTime -> Expense -> ExpenseDelivery -> Expense
impose uid now tpl d = tpl {expenseTransaction = deliveryTransaction d, expenseAccountId = toSqlKey (deliveryAccountId d), expenseAmount = deliveryAmount d, expenseValueDate = deliveryValueDate d, expenseBookingDate = bd, expenseLastModifiedThrough = deliverySource d, expenseCreationDate = now, expenseLastModifiedBy = uid, expenseLastModified = now}
      where bd = case deliveryBookingDate d of
                         Just x  -> Just x
                         Nothing -> if utctDay (deliveryValueDate d) >= utctDay now then Just now else Nothing

isMatch :: ExpenseDelivery -> AutomationRule -> Bool
isMatch e r = and [regexMaybe (automationRuleRegexTime r) (formatUTCTime . deliveryValueDate $ e),
    regexMaybe (automationRuleRegexTransaction r) (deliveryTransaction e),
    compareMaybe (automationRuleFilterAmount r) (deliveryAmount e),
    compareMaybe (automationRuleFilterAccount r) (toSqlKey $ deliveryAccountId e)]
  where regexMaybe Nothing _  = True
        regexMaybe (Just s) x = x =~ s
        compareMaybe Nothing _  = True
        compareMaybe (Just x) y = x == y

formatUTCTime :: UTCTime -> String
formatUTCTime t = printf "%02d.%02d.%04d %02d:%02d" d m y hh mm
  where (y,m,d) = toGregorian day
        (LocalTime day (TimeOfDay hh mm _)) = utcToLocalTime utc t

-- ^ treat a given expense as a freshly delivered automated one.
refilterExpense :: MonadUnliftIO m => Entity User -> ExpenseId -> ReaderT SqlBackend (ResourceT m) (Maybe ExpenseId)
refilterExpense u eid = do
  rules <- getRules u

  e' <- getEntity eid
  case e' of
    Nothing -> return Nothing
    Just e@(Entity _ ex) -> do
      perm <- checkPermissions u e
      if perm < ReadWritePermissions then return Nothing
      else do
        preliminary <- select $ from $ \flag -> do
          where_ ((flag ^. ExpenseFlagExpenseId) ==. val eid &&. ((flag ^. ExpenseFlagFlagging) ==. val Preliminary))
          return flag
        let d = ExpenseDelivery (expenseValueDate ex) (expenseBookingDate ex) (expenseAmount ex) (fromSqlKey $ expenseAccountId ex) (expenseTransaction ex) "moneydb-server" (not $ null preliminary)

        let matches = filter (isMatch d . entityVal . fst) rules
        -- n <- isNew d -- does not have to be new!

        if null matches then
          return Nothing
        else do
          now <- liftIO getCurrentTime
          let enew = impose (entityKey u) now (entityVal . snd $ head matches) d

          -- set last delivery
          update $ \ar -> do
           set ar [ AutomationRuleLastDelivery =. just (val now)]
           where_ $ ar ^. AutomationRuleId ==. val (entityKey . fst $ head matches)

          _ <- deleteElement eid
          _ <- insert (toSeen d)
          Just <$> copyExpense (entityKey . snd $ head matches) enew
