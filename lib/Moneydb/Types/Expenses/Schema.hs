{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Definitions of types that go into the database as tables
module Moneydb.Types.Expenses.Schema
    ( module Moneydb.Types.Expenses.Schema
    ) where

import           Data.Decimal                 (Decimal)
import           Data.Proxy                   (Proxy (..))
import           Data.Swagger                 (ToSchema (..), defaultSchemaOptions, fieldLabelModifier,
                                               genericDeclareNamedSchema)
import           Data.Time.Clock              (UTCTime)
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           GHC.Generics                 (Generic)
import           Moneydb.Types.Basic.Schema
import           Moneydb.Types.Basic.Types
import           Moneydb.Types.Expenses.Types

share [ mkPersist sqlSettings, mkMigrate "migrateExpenses" ]
      [persistLowerCase|
Expense json sql=expenses
    ownerId              UserId
    amount               Decimal
    valueDate            UTCTime
    bookingDate          UTCTime Maybe
    accountId            AccountId
    categoryId           CategoryId
    title                String
    description          String
    store                String
    transaction          String
    comments             String
    creationDate         UTCTime
    lastModified         UTCTime
    lastModifiedThrough  String
    lastModifiedBy       UserId
    deriving Show Generic

ExpenseSharing json sql=expense_sharings
    expenseId ExpenseId
    accountId AccountId
    type      ExpenseSharingType
    param     Decimal
    deriving Show Generic

ExpenseFlag json sql=expense_flags
    expenseId ExpenseId
    flagging ExpenseFlagging
    deriving Show Generic

PeriodicRule json sql=periodic_rules
    ownerId            UserId
    templateId         ExpenseId
    title              String
    period             Interval
    titleReplacement   String Maybe
    amountReplacement  Decimal Maybe
    lastCreation       UTCTime
    deriving Show Generic

AutomationRule json sql=automation_rules
    ownerId           UserId
    title             String
    templateId        ExpenseId
    priority          Int
    regexTime         String Maybe
    filterAccount     AccountId Maybe
    regexTransaction  String Maybe
    filterAmount      Decimal Maybe
    lastDelivery      UTCTime Maybe
    deriving Show Generic

SeenExpense json sql=seen_expenses
    valueDate     UTCTime
    bookingDate   UTCTime Maybe
    amount        Decimal
    accountId     AccountId
    preliminary   Bool
    transaction   String
    deriving Show Generic
|]

instance ToSchema AutomationRule where
    declareNamedSchema = genericDeclareNamedSchema $
        defaultSchemaOptions { fieldLabelModifier =
                                   prefixModifier' (Proxy
                                                    :: Proxy AutomationRule)
                             }

instance ToSchema Expense where
    declareNamedSchema = genericDeclareNamedSchema $
        defaultSchemaOptions { fieldLabelModifier =
                                   prefixModifier' (Proxy :: Proxy Expense)
                             }

instance ToSchema ExpenseFlag where
    declareNamedSchema = genericDeclareNamedSchema $
        defaultSchemaOptions { fieldLabelModifier =
                                   prefixModifier' (Proxy :: Proxy ExpenseFlag)
                             }

instance ToSchema ExpenseSharing where
    declareNamedSchema = genericDeclareNamedSchema $
        defaultSchemaOptions { fieldLabelModifier =
                                   prefixModifier' (Proxy
                                                    :: Proxy ExpenseSharing)
                             }

instance ToSchema PeriodicRule where
    declareNamedSchema = genericDeclareNamedSchema $
        defaultSchemaOptions { fieldLabelModifier =
                                   prefixModifier' (Proxy :: Proxy PeriodicRule)
                             }

instance ToSchema SeenExpense where
    declareNamedSchema = genericDeclareNamedSchema $
        defaultSchemaOptions { fieldLabelModifier =
                                   prefixModifier' (Proxy :: Proxy SeenExpense)
                             }

instance TypeName AutomationRule

instance TypeName PeriodicRule

instance TypeName Expense

instance TypeName ExpenseFlag

instance TypeName ExpenseSharing

instance TypeName SeenExpense
