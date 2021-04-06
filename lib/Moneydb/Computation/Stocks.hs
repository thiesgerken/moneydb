module Moneydb.Computation.Stocks(module Moneydb.Computation.Stocks.HistoricalStats,
                                  module Moneydb.Computation.Stocks.RealtimeStats
                                 ) where

import           Moneydb.Computation.Stocks.HistoricalStats (HistoricalPortfolioStatistics, HistoricalStockStatistics,
                                                             computeHistoricalPortfolioStat)
import           Moneydb.Computation.Stocks.RealtimeStats   (PortfolioStatistics, StockStatistics, computePortfolioStat)

-- way too easy calculations.
-- all I really need exposed via REST is

-- should write a js client for the query api.

-- input: StockId, Maybe Date
-- output: List of Transactions for that StockId until the given day
--          -> easy, just query transactions (using query api).

-- -> better querying api for HistoricalPrice (add Query instance with QueryOptions ?)
-- grabbing of plotting data (given interval, start and end date) for a specific exchange

-- also nice: grab latest intra-day data for a exchange (no caching, just proxy request to onvista)
-- plotting data for the whole portfolio value / value for units holding of a specific stock (+ invested money at that time)

-- statistics for a stock (and stockexchange) for a given day (or realtime), invested money, units, currentValue (+ date of this), internal rate of return
-- how to return this for all stocks? choose first whitelisted available exchange for each stock?
-- IRR for the whole portfolio (and invested money and current value)
