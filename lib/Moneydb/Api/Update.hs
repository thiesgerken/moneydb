{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Moneydb.Api.Update(UpdateAPI, UpdateSettings(..), updateHandler) where

import           Control.Monad.IO.Class  (liftIO)
import           Data.Aeson              (FromJSON)
import           Data.Swagger            (ToSchema)
import           Data.Time               (UTCTime)
import           Database.Persist        (Entity (..))
import           Database.Persist.Sqlite (fromSqlKey)
import           GHC.Generics            (Generic)
import           Servant.API
import           Servant.Server

import           Moneydb.Database
import           Moneydb.Notification
import           Moneydb.Types

data UpdateSettings = UpdateSettings {since :: UTCTime, filters :: [ExpenseFilter]}
    deriving (Generic, FromJSON, ToSchema, TypeName)

type UpdateAPI = "expenses" :> "rendered" :> "updates" :> ReqBody '[JSON] UpdateSettings :> Post '[JSON] [Record RenderedExpense]

updateHandler :: MoneyDBConn -> Entity User -> Server UpdateAPI
updateHandler mc u (UpdateSettings t fs) = liftIO $ runTransaction mc $ do
                      es <- getUpdates u t fs
                      res <- getRenderedExpenses (entityKey u) es
                      return $ zipWith (\x -> Record (fromSqlKey $ entityKey x)) es res
