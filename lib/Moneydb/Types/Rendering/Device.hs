{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Moneydb.Types.Rendering.Device (RenderedDevice(..)) where

import           Data.Aeson                 (FromJSON, ToJSON)
import           Data.Swagger               (ToSchema)
import           Data.Time.Clock            (UTCTime)
import           GHC.Generics               (Generic)
import           Moneydb.Types.Basic.Schema (ExpenseFilter)
import           Moneydb.Types.Basic.Types  (TypeName)

data RenderedDevice =
    RenderedDevice { token :: String
                   , model :: String
                   , firstContact :: UTCTime
                   , lastContact :: UTCTime
                   , notificationCount :: Int
                   , lastNotification :: Maybe UTCTime
                   , filters :: [ExpenseFilter]
                   }
    deriving (Show, ToJSON, FromJSON, Generic, ToSchema, TypeName)
