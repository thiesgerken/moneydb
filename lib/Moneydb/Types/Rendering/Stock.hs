{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}

module Moneydb.Types.Rendering.Stock
    ( RenderedStockExchange(..)
    , RenderedStock(..)
    ) where

import           Data.Aeson                (FromJSON, ToJSON)
import           Data.Swagger              (ToSchema)

import           GHC.Generics              (Generic)

import           Moneydb.Types.Basic.Types (Record, TypeName)
import           Moneydb.Types.Stocks

data RenderedStockExchange =
    RenderedStockExchange { name              :: String
                          , code              :: String
                          , onvistaRecordId   :: Int
                          , onvistaExchangeId :: Int
                          , recordCount       :: Int
                          , firstRecord       :: Maybe (Record StockHistoricalPrice)
                          , lastRecord        :: Maybe (Record StockHistoricalPrice)
                          , lastRealtime      :: Maybe (Record StockCurrentPrice)
                          }
    deriving (Show, ToJSON, FromJSON, Generic, ToSchema, TypeName)

data RenderedStock = RenderedStock { isin      :: String
                                   , info      :: Maybe (Record StockInfo)
                                   , exchanges :: [Record RenderedStockExchange]
                                   }
    deriving (Show, ToJSON, FromJSON, Generic, ToSchema, TypeName)
