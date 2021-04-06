{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}

module Moneydb.Types.Export (Export(..)) where

import           Data.Aeson
import           Database.Persist              (Entity)
import           GHC.Generics                  (Generic)
import           Moneydb.Types.Stocks
import           Moneydb.Types.Basic.Schema
import           Moneydb.Types.Expenses.Schema

data Export =
    Export { version :: String
           , users :: [Entity User]
           , groups :: [Entity Group]
           , accounts :: [Entity Account]
           , accountSyncings :: [Entity AccountSyncing]
           , balances :: [Entity Balance]
           , periodicRules :: [Entity PeriodicRule]
           , automationRules :: [Entity AutomationRule]
           , categories :: [Entity Category]
           , categoryReplacements :: [Entity CategoryReplacement]
           , devices :: [Entity Device]
           , expenseFilters :: [Entity ExpenseFilter]
           , seenExpenses :: [Entity SeenExpense]
           , expenses :: [Entity Expense]
           , expenseSharings :: [Entity ExpenseSharing]
           , expenseFlags :: [Entity ExpenseFlag]
           , stocks :: Maybe [Entity Stock]
           , securitiesAccounts :: Maybe [Entity SecuritiesAccount]
           , stockTransactions :: Maybe [Entity StockTransaction]
           }
    deriving (Generic, FromJSON, ToJSON)
