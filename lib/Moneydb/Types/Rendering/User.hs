{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Moneydb.Types.Rendering.User (RenderedUser(..)) where

import           Data.Aeson                 (FromJSON, ToJSON)
import           Data.Swagger               (ToSchema)
import           GHC.Generics               (Generic)
import           Moneydb.Types.Basic.Schema (GroupId)
import           Moneydb.Types.Basic.Types  (TypeName)

data RenderedUser = RenderedUser { name     :: String
                                 , fullName :: String
                                 , groupId  :: Maybe GroupId
                                 , isAdmin  :: Bool
                                 }
    deriving (TypeName, ToJSON, FromJSON, Generic, ToSchema)
