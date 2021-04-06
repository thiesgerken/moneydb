{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Moneydb.Api.Automation (automationHandler, AutomationAPI) where

import           Control.Monad.IO.Class (liftIO)
import           Data.Int               (Int64)
import           Data.Maybe             (fromMaybe, maybeToList)
import           Database.Esqueleto
import           Servant.API
import           Servant.Server
import           Moneydb.Automation
import           Moneydb.Database
import           Moneydb.Notification   (checkNotifications)
import           Moneydb.Types

-- | a type to wrap our private api
type AutomationAPI = "expenses"
    :> ("deliver" :> ReqBody '[JSON] [ExpenseDelivery]
        :> Post '[JSON] (Maybe [Int64]) :<|> "recheck" :> Capture "id" Int64
        :> Post '[JSON] (Maybe Int64))

automationHandler :: MoneyDBConn -> Entity User -> Server AutomationAPI
automationHandler mc u = deliverHandler :<|> recheckHandler
  where
    deliverHandler d = liftIO . runTransaction mc $ do
        ids <- addAutomatedExpenses u d
        checkNotifications mc (fromMaybe [] ids)
        return $ fmap fromSqlKey <$> ids

    recheckHandler x = liftIO . runTransaction mc $ do
        id' <- refilterExpense u (toSqlKey x)
        checkNotifications mc $ maybeToList id'
        return $ fromSqlKey <$> id'

