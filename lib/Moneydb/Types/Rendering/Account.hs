{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Moneydb.Types.Rendering.Account (RenderedAccount(..)) where

import           Data.Aeson                 (FromJSON, ToJSON)
import           Data.Swagger               (ToSchema)
import           GHC.Generics               (Generic)
import           Moneydb.Types.Basic.Schema (AccountSyncing)
import           Moneydb.Types.Basic.Types  (AccountKind, Availability, TypeName)

data RenderedAccount =
    RenderedAccount { title        :: String
                    , description  :: String
                    , color        :: String
                    , kind         :: AccountKind
                    , availability :: Availability
                    , iban         :: Maybe String
                    , hidden       :: Bool
                    , syncing      :: Maybe AccountSyncing
                    }
    deriving (Show, ToJSON, FromJSON, Generic, ToSchema, TypeName)
