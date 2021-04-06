{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Moneydb.Types.Rendering.Category (RenderedCategory(..)) where

import           Data.Aeson                 (FromJSON, ToJSON)
import           Data.Swagger               (ToSchema)
import           GHC.Generics               (Generic)
import           Moneydb.Types.Basic.Schema (CategoryId)
import           Moneydb.Types.Basic.Types  (TypeName)

data RenderedCategory = RenderedCategory { title       :: String
                                         , description :: String
                                         , color       :: String
                                         , replaces    :: [CategoryId]
                                         }
    deriving (Show, ToJSON, FromJSON, Generic, ToSchema, TypeName)
