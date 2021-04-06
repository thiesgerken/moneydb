{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

-- ^ Definitions of types that go into the database as tables

module Moneydb.Types.Stocks (module Moneydb.Types.Stocks) where

import           Data.Decimal               (Decimal)
import           Data.Proxy                 (Proxy (..))
import           Data.Swagger               (ToSchema (..), defaultSchemaOptions, fieldLabelModifier,
                                             genericDeclareNamedSchema)
import           Data.Time.Clock            (UTCTime)
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           GHC.Generics               (Generic)
import           Moneydb.Types.Basic.Schema
import           Moneydb.Types.Basic.Types

share [ mkPersist sqlSettings, mkMigrate "migrateStocks" ]
      [persistLowerCase|
Stock json sql=stocks -- Watchlist per user, user can only add transactions for watched stocks.
    ownerId              UserId
    isin                 String
    deriving Show Generic

SecuritiesAccount json sql=securities_accounts
    ownerId              UserId
    title                String
    broker               String
    iban                 String
    deriving Show Generic

StockTransaction json sql=stock_transactions
    accountId            SecuritiesAccountId
    stockId              StockId
    date                 UTCTime
    units                Double
    amount               Decimal -- = Units * Rate, i.e. does NOT include fees.
    fees                 Decimal -- = Total Price - Price
    exchange             String
    deriving Show Generic

StockInfo json sql=stock_infos -- grabbed periodically for watched ISINs
    isin                 String
    wkn                  String
    onvistaUrl           String
    kind                 String
    focus                String
    managingCompany      String
    title                String
    countryCode          String
    country              String
    lastHistoricalUpdate UTCTime
    lastCurrentUpdate    UTCTime
    deriving Show Generic

StockExchange json sql=stock_exchanges -- grabbed periodically for watched ISINs
    stockId              StockInfoId
    name                 String
    code                 String
    onvistaRecordId      Int -- ID specific to exchange+stock
    onvistaExchangeId    Int -- ID specific to exchange only
    deriving Show Eq Generic

StockHistoricalPrice json sql=stock_historical_prices -- grabbed periodically for watched ISINs
    exchangeId           StockExchangeId
    day                  UTCTime -- midnight
    opening              Double
    closing              Double
    high                 Double
    low                  Double
    volume               Int Maybe
    deriving Show Generic

StockCurrentPrice json sql=stock_current_prices -- grabbed periodically for watched ISINs and can be updated manually
    exchangeId           StockExchangeId
    date                 UTCTime
    price                Double
    deriving Show Generic
|]

instance ToSchema Stock where
    declareNamedSchema = genericDeclareNamedSchema $
        defaultSchemaOptions { fieldLabelModifier = prefixModifier' (Proxy :: Proxy Stock) }

instance ToSchema SecuritiesAccount where
    declareNamedSchema = genericDeclareNamedSchema $
        defaultSchemaOptions { fieldLabelModifier = prefixModifier' (Proxy :: Proxy SecuritiesAccount) }

instance ToSchema StockTransaction where
    declareNamedSchema = genericDeclareNamedSchema $
        defaultSchemaOptions { fieldLabelModifier = prefixModifier' (Proxy :: Proxy StockTransaction) }

instance ToSchema StockInfo where
    declareNamedSchema = genericDeclareNamedSchema $
        defaultSchemaOptions { fieldLabelModifier = prefixModifier' (Proxy :: Proxy StockInfo) }

instance ToSchema StockExchange where
    declareNamedSchema = genericDeclareNamedSchema $
        defaultSchemaOptions { fieldLabelModifier = prefixModifier' (Proxy :: Proxy StockExchange) }

instance ToSchema StockHistoricalPrice where
    declareNamedSchema = genericDeclareNamedSchema $
        defaultSchemaOptions { fieldLabelModifier = prefixModifier' (Proxy :: Proxy StockHistoricalPrice) }

instance ToSchema StockCurrentPrice where
    declareNamedSchema = genericDeclareNamedSchema $
        defaultSchemaOptions { fieldLabelModifier = prefixModifier' (Proxy :: Proxy StockCurrentPrice) }

instance TypeName Stock

instance TypeName SecuritiesAccount

instance TypeName StockTransaction

instance TypeName StockInfo

instance TypeName StockExchange

instance TypeName StockHistoricalPrice

instance TypeName StockCurrentPrice
