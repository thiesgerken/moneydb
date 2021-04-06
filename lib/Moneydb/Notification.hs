{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Moneydb.Notification(checkNotifications, notify, getUpdates) where

import           Control.Monad                (filterM)
import           Control.Monad.IO.Unlift      (MonadIO, MonadUnliftIO, liftIO)
import           Control.Monad.Logger
import           Control.Monad.Trans.Reader   (ReaderT)
import           Control.Monad.Trans.Resource (ResourceT)
import           Data.Aeson                   (encode, object, (.=))
import qualified Data.ByteString.Char8        as BS
import qualified Data.ByteString.Lazy         as BSL
import           Data.Int                     (Int64)
import qualified Data.Map                     as Map
import           Data.Maybe                   (fromMaybe)
import qualified Data.Set                     as Set
import qualified Data.Text                    as T
import           Data.Time                    (UTCTime)
import           Data.Time.Clock              (getCurrentTime)
import           Database.Esqueleto
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Status    (statusCode)
import           Text.Regex.TDFA              ((=~))

import           Moneydb.Config
import           Moneydb.Database
import           Moneydb.Types

-- sending of ids is optional. empty list is considered as a 'ping' so that the device may ask for updates itself
sendNotification :: String -> [Int64] -> Device -> IO (Response BSL.ByteString)
sendNotification token payload d = do
  manager <- newManager tlsManagerSettings

  let requestObject = object ("to" .= deviceToken d : if null payload then [] else ["data" .= object ["ids" .= payload]])
  initialRequest <- parseRequest "https://fcm.googleapis.com/fcm/send"
  let request = initialRequest { method = "POST", requestBody = RequestBodyLBS $ encode requestObject, requestHeaders = [(hContentType, "application/json"), (hAuthorization, BS.pack $ "key=" <> token)] }

  httpLbs request manager

notify :: (MonadLogger m, MonadIO m) => String -> Entity Device -> [Int64] -> m Bool
notify token (Entity _ d) payload = do
  logInfoN $ "Notifying device " <> T.pack (take 8 $ deviceToken d) <>
              case length payload of
                0 -> " for updates"
                1 -> " for expense " <> T.pack (show $ head payload);
                n -> " for " <> T.pack (show n) <> " expenses"
  r <- liftIO $ sendNotification token payload d
  case statusCode (responseStatus r) of
    200 -> do logInfoN "Notification successful"
              return True
    x   -> do logErrorN $ "Got status code " <> T.pack (show x) <> ", response:\n" <> T.pack (show $ responseBody r)
              return False

checkNotifications :: MonadUnliftIO m => MoneyDBConn -> [ExpenseId] -> ReaderT SqlBackend (ResourceT m) ()
checkNotifications mc eids
  | firebaseEnabled (moneyConfig mc) = do
     es <- fmap (uncurry Entity) . Map.toList <$> getMany eids

     -- devices that can access the expense(s)
     xs <- zip es <$> mapM concerns es -- [(Expense, [Device])]

     -- devices that also want to be notified
     xs' <- zip es <$> mapM (\(a, bs) -> filterM (wantsNotification a) bs) xs -- [(Expense, [Device])]

     let ds = Set.toList . Set.fromList . concat $ map snd xs'
     let xs'' = fmap (\d -> (d, fst <$> filter (elem d . snd) xs')) ds -- [(Device, [Expense])]

     let notify' = (`runLoggingT` moneyLogger mc) . (\(d, ess) -> notify (firebaseToken $ moneyConfig mc) d (fromSqlKey . entityKey <$> ess))
     ds' <- liftIO $ filterM notify' xs''
     t <- liftIO getCurrentTime

     update $ \dev -> do
       set dev [DeviceLastNotification =. just (val t), DeviceNotificationCount +=. val 1]
       where_ $ (dev ^. DeviceToken) `in_` valList (deviceToken . entityVal . fst <$> ds')
  | otherwise = return ()

wantsNotification :: MonadUnliftIO m => Entity Expense -> Entity Device -> ReaderT SqlBackend (ResourceT m) Bool
wantsNotification (Entity _ e) d = do
          let new = expenseCreationDate e == expenseLastModified e
          fs <- select $ from $ \x -> do
              where_ $ (x ^. ExpenseFilterDeviceId) ==. val (entityKey d)
              return x

          return $ any (matchesFilter (deviceOwnerId $ entityVal d) new e) (entityVal <$> fs)

regexMaybe :: Maybe String -> String
regexMaybe = fromMaybe ".*"

matchesFilter' :: UserId -> UTCTime -> Expense -> ExpenseFilter -> Bool
matchesFilter' uid t e f = ((fromSqlKey (expenseLastModifiedBy e) /= fromSqlKey uid) || not (expenseFilterOnlySomeoneElse f))
           && (expenseLastModifiedThrough e =~ regexMaybe (expenseFilterOnlyThrough f))
           && (t < if expenseFilterOnlyNew f then expenseCreationDate e else expenseLastModified e)

matchesFilter :: UserId -> Bool -> Expense -> ExpenseFilter -> Bool
matchesFilter uid new e f = ((fromSqlKey (expenseLastModifiedBy e) /= fromSqlKey uid) || not (expenseFilterOnlySomeoneElse f))
           && (expenseLastModifiedThrough e =~ regexMaybe (expenseFilterOnlyThrough f))
           && (new || not (expenseFilterOnlyNew f))

concerns :: MonadUnliftIO m => Entity Expense -> ReaderT SqlBackend (ResourceT m) [Entity Device]
concerns e = select $ from $ \(d, e') -> do
        where_ $ expenseVisible' (d ^. DeviceOwnerId) e'
                   &&. (e' ^. ExpenseId) ==. val (entityKey e)
        return d

getUpdates :: MonadUnliftIO m => Entity User -> UTCTime -> [ExpenseFilter] -> ReaderT SqlBackend (ResourceT m) [Entity Expense]
getUpdates (Entity uid _) t fs = do
        es <- select $ from $ \e -> do
            where_ $ expenseVisible uid e
                &&. (e ^. ExpenseLastModified) >=. val t
            return e

        return $ filter (\e -> any (matchesFilter' uid t (entityVal e)) fs) es
