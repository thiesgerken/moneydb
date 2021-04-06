{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

-- | 'Basic' type definitions for types that are needed by other types, but cannot appear in the same module due to usage of TemplateHaskell
module Moneydb.Types.Expenses.Types
    ( ExpenseDelivery(..)
    , ExpenseSharingType(..)
    , ExpenseFlagging(..)
    ) where

import           Data.Aeson                (FromJSON, ToJSON)
import           Data.Decimal              (Decimal)
import           Data.Int                  (Int64)
import           Data.Swagger              (ToSchema)
import           Data.Time                 (UTCTime)
import           Database.Persist.TH
import           GHC.Generics              (Generic)
import           Moneydb.Types.Basic.Types ()
-- ^ automated expense that is delivered by some grabber

data ExpenseDelivery =
    ExpenseDelivery { deliveryValueDate   :: UTCTime
                    , deliveryBookingDate :: Maybe UTCTime
                    , deliveryAmount      :: Decimal
                    , deliveryAccountId   :: Int64
                    , deliveryTransaction :: String
                    , deliverySource      :: String
                    , deliveryPreliminary :: Bool
                    }
    deriving (Eq, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

-- Equal: amount divided by (amount of "Equal" in the sharing infos + 1) times the parameter.
data ExpenseSharingType = Equal | FixedAmount | FixedFraction
    deriving (Eq, Show, Read, Ord, Generic, FromJSON, ToJSON, ToSchema)

derivePersistField "ExpenseSharingType"

data ExpenseFlagging =
    Template | NeedsAttention | TaxRelevant | Compensated | Preliminary
    deriving (Eq, Show, Ord, Read, Generic, FromJSON, ToJSON, ToSchema)

derivePersistField "ExpenseFlagging"
