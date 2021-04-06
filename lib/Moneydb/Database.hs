{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

-- | database bindings for moneydb.
--   Note that this module may abort execution on malformed queries or process them, leading to a corrupt database.
--   (example: Adding an expense with different ownerid and owner of account & account is not shared)
module Moneydb.Database
    ( connect
    , commit
    , runTransaction
    , MoneyDBConn(moneyConfig, moneyLogger, moneyMessage)
    , RuntimeMessage(..)
    , paginate
    , listReadableElements
    , module Moneydb.Database.Rendering
    , module Moneydb.Database.Permissions
    , module Moneydb.Database.Tables
    , module Moneydb.Database.Stocks
    ) where

import           Control.Concurrent.MVar      (MVar, newEmptyMVar)
import           Control.Monad.IO.Class       (MonadIO (..), liftIO)
import           Control.Monad.IO.Unlift      (MonadUnliftIO)
import           Control.Monad.Logger
import           Control.Monad.Trans.Reader   (ReaderT)
import           Control.Monad.Trans.Resource (ResourceT, runResourceT)
import           Data.Int                     (Int64)
import           Data.Pool                    (destroyAllResources)
import qualified Data.Text                    as T
import           Database.Esqueleto           hiding (isNothing)
import           Database.Persist.MySQL       (createMySQLPool)
import           Database.Persist.Postgresql  (createPostgresqlPool)
import           Database.Persist.Sqlite      (createSqlitePool)
import           Moneydb.Config
import           Moneydb.Database.Permissions
import           Moneydb.Database.Rendering
import           Moneydb.Database.Stocks
import           Moneydb.Database.Tables
import           Moneydb.Types

data MoneyDBConn = MoneyDBConn { moneyPool    :: ConnectionPool
                               , moneyConfig  :: Config
                               , moneyLogger  :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
                               , moneyMessage :: MVar RuntimeMessage
                               }

data RuntimeMessage = RefreshStocks

connect :: MonadUnliftIO m => Config -> (Loc -> LogSource -> LogLevel -> LogStr -> IO ())  -> m MoneyDBConn
connect cfg f = (`runLoggingT` f) $ do
    pool <- case dbBackend cfg of
        SQLiteBackend conf     -> createSqlitePool (T.pack $ sqlitePath conf) (sqliteConns conf)
        PostgreSQLBackend conf -> createPostgresqlPool (postgresConnString conf) (postgresConns conf)
        MySQLBackend conf      -> createMySQLPool (mysqlConnInfo conf) (mysqlConns conf)

    mv <- liftIO newEmptyMVar
    let mc = MoneyDBConn pool cfg f mv

    runTransaction mc $ do
        runMigration migrateBasic
        runMigration migrateExpenses
        runMigration migrateStocks

    return mc

-- should make sure that all transactions from the write-ahead-log (sqlite) are committed to the database
commit :: MonadIO m => MoneyDBConn -> m ()
commit mc = liftIO $ destroyAllResources (moneyPool mc)

runTransaction :: MonadUnliftIO m => MoneyDBConn -> ReaderT SqlBackend (ResourceT m) a -> m a
runTransaction mc x = runResourceT $ runSqlPool x (moneyPool mc)

paginate :: Maybe Int64 -> Maybe Int64 -> SqlQuery ()
paginate off lim = maybe (return ()) offset off >> maybe (return ()) limit lim

listReadableElements :: (DBElement a, MonadUnliftIO m)
                     => Entity User
                     -> Maybe Int64
                     -> Maybe Int64
                     -> ReaderT SqlBackend (ResourceT m) [Entity a]
listReadableElements u off lim = do
    xs <- select $ from $ \x -> do
        where_ $ readable u x
        orderBy [ nativeOrdering x ]
        paginate off lim
        return x
    renderElements u xs
