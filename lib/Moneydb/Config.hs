{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Moneydb.Config(Config(..), DatabaseBackend(..), PostgreSQLConfig(..), SQLiteConfig(..), MySQLConfig(..), loadConfig, postgresConnString, mysqlConnInfo) where

import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Char8   as BS8
import           Data.Configurator       (Worth (..), load, lookupDefault)
import           Data.Configurator.Types (Configured (..))
import qualified Data.Configurator.Types as CT
import qualified Data.Text               as T
import           Data.Word               (Word16)
import           Database.Persist.MySQL  (ConnectInfo (..), defaultConnectInfo)

postgresConnString :: PostgreSQLConfig -> ByteString
postgresConnString (PostgreSQLConfig host port u p db _) = "host=" <> host <> " port=" <> BS8.pack (show port) <> (if BS8.null u then "" else " user=" <> u <> " password=" <> p) <> " dbname=" <> db

mysqlConnInfo :: MySQLConfig -> ConnectInfo
mysqlConnInfo (MySQLConfig host port u p db _) = defaultConnectInfo {connectHost = host, connectDatabase = db, connectUser = u, connectPassword = p, connectPort = port}

data SQLiteConfig = SQLiteConfig {sqlitePath :: FilePath, sqliteConns :: Int} deriving (Show)

data PostgreSQLConfig = PostgreSQLConfig {postgresHost :: ByteString, postgresPort :: Word16, postgresUser :: ByteString,  postgresPassword :: ByteString, postgresDatabase :: ByteString, postgresConns :: Int} deriving (Show)

data MySQLConfig = MySQLConfig {mysqlHost :: String, mysqlPort :: Word16, mysqlUser :: String,  mysqlPassword :: String, mysqlDatabase :: String, mysqlConns :: Int} deriving (Show)

data DatabaseBackend = SQLiteBackend SQLiteConfig | PostgreSQLBackend PostgreSQLConfig | MySQLBackend MySQLConfig deriving (Show)

data DatabaseType = SQLite | PostgreSQL | MySQL

instance Configured DatabaseType where
  convert (CT.String x) = case T.toLower x of
                              "postgresql" -> Just PostgreSQL
                              "sqlite"     -> Just SQLite
                              "mysql"      -> Just MySQL
                              _            -> Nothing
  convert _             = Nothing

data Config = Config { dbBackend :: DatabaseBackend, firebaseToken :: String, firebaseEnabled :: Bool, periodicAdvance :: Double, preliminaryMaxAge :: Integer} deriving (Show)

loadConfig :: FilePath -> IO Config
loadConfig cfgPath = do
  cfg <- load [Optional cfgPath, Optional "./money.cfg"]
  pa <- lookupDefault 12.0 cfg "general.periodic_advance"
  pmax <- lookupDefault 7 cfg "general.preliminary_max_age"

  fbToken <- lookupDefault "" cfg "firebase.token"
  fbEnabled <- lookupDefault False cfg "firebase.enabled"

  dbType <- lookupDefault PostgreSQL cfg "general.database_backend"

  backend <- case dbType of
    SQLite -> do
      conns <- lookupDefault 25 cfg "sqlite.connections"
      db <- lookupDefault "./money.sqlite3" cfg "sqlite.path"
      return . SQLiteBackend $ SQLiteConfig db conns
    PostgreSQL -> do
      conns <- lookupDefault 25 cfg "postgresql.connections"
      host <- lookupDefault "localhost" cfg "postgresql.host"
      port <- lookupDefault 5432 cfg "postgresql.port"
      db <- lookupDefault "moneydb" cfg "postgresql.database"
      user <- lookupDefault "" cfg "postgresql.user"
      pass <- lookupDefault "" cfg "postgresql.password"

      return . PostgreSQLBackend $ PostgreSQLConfig host port user pass db conns
    MySQL -> do
      conns <- lookupDefault 25 cfg "mysql.connections"
      host <- lookupDefault "localhost" cfg "mysql.host"
      port <- lookupDefault 3306 cfg "mysql.port"
      db <- lookupDefault "moneydb" cfg "mysql.database"
      user <- lookupDefault "" cfg "mysql.user"
      pass <- lookupDefault "" cfg "mysql.password"

      return . MySQLBackend $ MySQLConfig host port user pass db conns
  return $ Config backend fbToken fbEnabled pa pmax
