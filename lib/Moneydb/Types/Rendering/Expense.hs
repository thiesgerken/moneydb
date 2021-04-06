{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Moneydb.Types.Rendering.Expense
    ( RenderedExpense(..)
    , RenderedSharing(..)
    ) where

import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.Decimal                 (Decimal)
import           Data.Swagger                 (ToSchema)
import           Data.Time.Clock              (UTCTime)
import           GHC.Generics                 (Generic)
import           Moneydb.Types.Basic.Schema   hiding (User)
import           Moneydb.Types.Basic.Types    (TypeName)
import           Moneydb.Types.Expenses.Types (ExpenseFlagging
                                             , ExpenseSharingType)

data RenderedExpense =
    RenderedExpense { amount :: Decimal
                    , effectiveAmount :: Decimal
                    , valueDate :: UTCTime
                    , bookingDate :: Maybe UTCTime
                    , accountId :: AccountId
                    , categoryId :: CategoryId
                    , title :: String
                    , description :: String
                    , store :: String
                    , transaction :: String
                    , comments :: String
                    , creationDate :: UTCTime
                    , lastModified :: UTCTime
                    , lastModifiedThrough :: String
                    , lastModifiedBy :: UserId
                    , sharing :: [RenderedSharing]
                    , flags :: [ExpenseFlagging]
                    }
    deriving (Show, ToJSON, FromJSON, Generic, ToSchema, TypeName)

data RenderedSharing = RenderedSharing { sharingAccountId :: AccountId
                                       , sharingType      :: ExpenseSharingType
                                       , parameter        :: Decimal
                                       , calculatedAmount :: Decimal
                                       }
    deriving (Show, ToJSON, FromJSON, Generic, ToSchema)
