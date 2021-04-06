module Moneydb.Demo(insertDemoData) where

import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.IO.Unlift (MonadUnliftIO)
import           Data.Time               (UTCTime (..), fromGregorian, secondsToDiffTime)

import           Moneydb.Database        (MoneyDBConn, insertElement, runTransaction)
import           Moneydb.Types
import           Moneydb.Util            (hashPassword')

insertDemoData :: MonadUnliftIO m => MoneyDBConn -> m ()
insertDemoData mc = runTransaction mc $ do
    demoHash <- liftIO $ hashPassword' "demo"

    maxId <- insertElement $ User "max" demoHash "Max Mustermann" Nothing False
    erikaId <- insertElement $ User "erika" demoHash "Erika Mustermann" Nothing False

    acc1 <- insertElement $ Account maxId "Volksbank" "Girokonto" "#d8f3ff" Debit Immediately (Just "DE12345") False
    acc2 <- insertElement $ Account maxId "Bargeld" "Geld in der Tasche" "#fff8dd" Cash Immediately Nothing False
    acc3 <- insertElement $ Account maxId "Schulden bei Erika" "" "#ffe5e5" Debt Weeks Nothing False
    acc4 <- insertElement $ Account erikaId "Schulden bei Max" "" "#ffe5e5" Debt Weeks Nothing False
    _ <- insertElement $ AccountSyncing acc3 acc4 False

    cat1 <- insertElement $ Category maxId "Essen" "" "#ffd5d5"
    cat2 <- insertElement $ Category maxId "Hobby" "" "#fff8dd"
    cat3 <- insertElement $ Category maxId "Kontobewegung" "" "#e7ffff"

    let testDate = UTCTime (fromGregorian 2015 12 25) (secondsToDiffTime 7200)
    let testDate' = UTCTime (fromGregorian 2000 1 1) (secondsToDiffTime 0)

    _ <- insertElement $ Expense maxId 2.50 testDate Nothing acc2 cat1 "Marmelade" "" "REWE" "" "" testDate' testDate' "" maxId

    e1 <- insertElement $ Expense maxId 2.00 testDate Nothing acc3 cat1 "Brot" "" "REWE" "" "" testDate' testDate' "" maxId
    _ <- insertElement $ ExpenseSharing e1 acc3 Equal 0

    e2 <- insertElement $ Expense maxId 19.90 testDate Nothing acc1 cat2 "Brettspiel" "" "amazon.de" "" "" testDate' testDate' "" maxId
    _ <- insertElement $ ExpenseSharing e2 acc2 FixedAmount 5

    e3 <- insertElement $ Expense maxId 19.90 testDate Nothing acc1 cat2 "Haushaltszeug" "" "amazon.de" "" "" testDate' testDate' "" maxId
    _ <- insertElement $ ExpenseSharing e3 acc2 FixedAmount 5

    e4 <- insertElement $ Expense maxId (-80) testDate Nothing acc1 cat3 "Geldautomat" "" "amazon.de" "" "" testDate' testDate' "" maxId
    _ <- insertElement $ ExpenseSharing e4 acc1 FixedFraction 1.00

    _ <- insertElement $ Balance acc1 (UTCTime (fromGregorian 2015 12 1) (secondsToDiffTime 7200)) 5387.20
    _ <- insertElement $ Balance acc2 (UTCTime (fromGregorian 2015 7 3) (secondsToDiffTime 7200)) 45.67
    _ <- insertElement $ Balance acc3 (UTCTime (fromGregorian 2015 7 3) (secondsToDiffTime 7200)) 0.0

    return ()
