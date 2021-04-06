{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}

module Moneydb.Types.DummyExport (DummyExport(..)) where

import           Data.Aeson
import           GHC.Generics (Generic)

newtype DummyExport = DummyExport { version :: String }
    deriving (Generic, ToJSON, FromJSON)
