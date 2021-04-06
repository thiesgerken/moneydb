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

-- ^ Definitions of types that go into the database as tables
module Moneydb.Types.Basic.Schema(module Moneydb.Types.Basic.Schema) where

import           Data.Decimal              (Decimal)
import           Data.Proxy                (Proxy (..))
import           Data.Swagger              (ToSchema (..), defaultSchemaOptions, fieldLabelModifier,
                                            genericDeclareNamedSchema)
import           Data.Time.Clock           (UTCTime)
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           GHC.Generics              (Generic)

import           Moneydb.Types.Basic.Types

share [mkPersist sqlSettings, mkMigrate "migrateBasic"] [persistLowerCase|
User json sql=users
    name              String
    passwordHash      String
    fullName          String
    groupId           GroupId Maybe
    isAdmin           Bool
    UserNameKey name
    deriving Show Generic

Group json sql=groups
    name  String
    GroupNameKey name
    deriving Show Generic

Account json sql=accounts
    ownerId       UserId
    title         String
    description   String
    color         String
    kind          AccountKind
    availability  Availability
    iban          String Maybe
    hidden        Bool
    deriving Show Generic

AccountSyncing json sql=account_syncings
    account1 AccountId
    account2 AccountId
    sign   Bool
    UniqueSyncingPair account1 account2
    deriving Show Generic

Category json sql=categories
    ownerId      UserId
    title        String
    description  String
    color        String
    deriving Show Generic

CategoryReplacement json sql=category_replacements
    ownerId         UserId
    original        CategoryId
    replacement     CategoryId
    UniqueReplacement ownerId original
    deriving Show Generic

Balance json sql=balances
    accountId AccountId
    date      UTCTime -- includes expenses with date < this value (i.e. what happened the same second is not included)
    amount    Decimal
    deriving Show Generic

Device json sql=devices
    ownerId            UserId
    token              String
    DeviceTokenKey token
    model              String
    firstContact       UTCTime
    lastContact        UTCTime
    notificationCount  Int
    lastNotification   UTCTime Maybe
    deriving Show Generic Eq Ord

ExpenseFilter json sql=expense_filters
    deviceId           DeviceId
    onlyNew            Bool
    onlySomeoneElse    Bool
    onlyThrough        String Maybe
    deriving Show Generic
|]

instance ToSchema Device where declareNamedSchema = genericDeclareNamedSchema $ defaultSchemaOptions {fieldLabelModifier = prefixModifier' (Proxy :: Proxy Device)}
instance ToSchema Account where declareNamedSchema = genericDeclareNamedSchema $ defaultSchemaOptions {fieldLabelModifier = prefixModifier' (Proxy :: Proxy Account)}
instance ToSchema Balance where declareNamedSchema = genericDeclareNamedSchema $ defaultSchemaOptions {fieldLabelModifier = prefixModifier' (Proxy :: Proxy Balance)}
instance ToSchema Category where declareNamedSchema = genericDeclareNamedSchema $ defaultSchemaOptions {fieldLabelModifier = prefixModifier' (Proxy :: Proxy Category)}
instance ToSchema CategoryReplacement where declareNamedSchema = genericDeclareNamedSchema $ defaultSchemaOptions {fieldLabelModifier = prefixModifier' (Proxy :: Proxy CategoryReplacement)}
instance ToSchema Group where declareNamedSchema = genericDeclareNamedSchema $ defaultSchemaOptions {fieldLabelModifier = prefixModifier' (Proxy :: Proxy Group)}
instance ToSchema AccountSyncing where declareNamedSchema = genericDeclareNamedSchema $ defaultSchemaOptions {fieldLabelModifier = prefixModifier' (Proxy :: Proxy AccountSyncing)}
instance ToSchema ExpenseFilter where declareNamedSchema = genericDeclareNamedSchema $ defaultSchemaOptions {fieldLabelModifier = prefixModifier' (Proxy :: Proxy ExpenseFilter)}

instance TypeName ExpenseFilter
instance TypeName AccountSyncing
instance TypeName Account
instance TypeName Balance
instance TypeName Category
instance TypeName CategoryReplacement
instance TypeName Device
instance TypeName Group
