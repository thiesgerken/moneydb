{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module Moneydb.Transfer(importJSON, exportJSON) where

import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.IO.Unlift    (MonadUnliftIO)
import           Control.Monad.Logger       (MonadLogger, logErrorN, logInfoN)
import           Control.Monad.Trans.Reader (ReaderT)
import           Data.Aeson                 (eitherDecode, encode)
import qualified Data.ByteString.Lazy       as BSL
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Database.Persist           (BaseBackend, Entity, PersistEntity, PersistEntityBackend,
                                             PersistStoreWrite, insertEntityMany, selectList)

import           Moneydb.Database
import qualified Moneydb.Types.DummyExport  as DummyExport
import qualified Moneydb.Types.Export       as Export

insertEntityMany' :: (MonadIO m, MonadLogger m, PersistStoreWrite backend,
                            PersistEntity record,
                            PersistEntityBackend record ~ BaseBackend backend) =>
                           [Entity record] -> Text -> ReaderT backend m ()
insertEntityMany' y s = insertEntityMany y >> logInfoN ("Imported " <> (T.pack . show $ length y) <> " " <> s)

importJSON :: (MonadUnliftIO m, MonadLogger m) => MoneyDBConn -> BSL.ByteString -> m ()
importJSON mc input = case eitherDecode input of
                        Right (DummyExport.DummyExport v) -> importJSON' mc v input
                        Left s                            -> logErrorN (T.pack s)

importJSON' :: (MonadUnliftIO m, MonadLogger m) => MoneyDBConn -> String -> BSL.ByteString -> m ()
importJSON' mc "2.1" input =
          case eitherDecode input :: Either String Export.Export of
            Right x -> runTransaction mc $ do
             logInfoN "Recognized and parsed input data in version 2.1"
             logInfoN "Clearing database content"
             clear
             insertEntityMany' (Export.groups x) "groups"
             insertEntityMany' (Export.users x) "users"
             insertEntityMany' (Export.accounts x) "accounts"
             insertEntityMany' (Export.accountSyncings x) "account syncings"
             insertEntityMany' (Export.balances x) "balances"
             insertEntityMany' (Export.categories x) "categories"
             insertEntityMany' (Export.categoryReplacements x) "category replacements"
             insertEntityMany' (Export.devices x) "devices"
             insertEntityMany' (Export.expenseFilters x) "expense filters"
             insertEntityMany' (Export.seenExpenses x) "seen expenses"
             insertEntityMany' (Export.expenses x) "expenses"
             insertEntityMany' (Export.expenseSharings x) "expense sharings"
             insertEntityMany' (Export.expenseFlags x) "expense flags"
             insertEntityMany' (Export.periodicRules x) "periodic rules"
             insertEntityMany' (Export.automationRules x) "automation rules"

             maybe (return ()) (`insertEntityMany'` "stock identifiers") (Export.stocks x) 
             maybe (return ()) (`insertEntityMany'` "securities accounts") (Export.securitiesAccounts x) 
             maybe (return ()) (`insertEntityMany'` "stock transactions") (Export.stockTransactions x) 
     
            Left s -> logErrorN (T.pack s)
importJSON' _ "2.0" _ = logErrorN "Importing of 2.0 data is not supported anymore. Please try revision #76e48b4c."
importJSON' _ v _     = logErrorN $ "Unknown version of data: " <> T.pack v

exportJSON :: MonadUnliftIO m => MoneyDBConn -> m BSL.ByteString
exportJSON mc = runTransaction mc $ do
  users <- selectList [] []
  groups <- selectList [] []
  accounts <- selectList [] []
  accountSyncings <- selectList [] []
  balances <- selectList [] []
  periodicRules <- selectList [] []
  automationRules <- selectList [] []
  categories <- selectList [] []
  categoryReplacements <- selectList [] []
  devices <- selectList [] []
  expenseFilters <- selectList [] []
  seenExpenses <- selectList [] []
  expenses <- selectList [] []
  expenseSharings <- selectList [] []
  expenseFlags <- selectList [] []

  stocks              <- selectList [] []
  securitiesAccounts  <- selectList [] []
  stockTransactions   <- selectList [] []

  return $ encode (Export.Export "2.1" users groups accounts accountSyncings balances periodicRules automationRules categories categoryReplacements devices expenseFilters seenExpenses expenses expenseSharings expenseFlags (Just stocks) (Just securitiesAccounts) (Just stockTransactions))
