{-# LANGUAGE CPP             #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Moneydb.Computation.Stocks.HistoricalStats(computeHistoricalPortfolioStat, HistoricalPortfolioStatistics(..), HistoricalStockStatistics(..)) where

import           Control.Monad.IO.Class          (MonadIO)
import           Control.Monad.Trans.Reader      (ReaderT)
import           Data.Aeson                      (ToJSON)
import           Data.Decimal                    (Decimal, (*.))
import           Data.List                       (group, sort, sortOn)
import           Data.Maybe                      (catMaybes, isJust, listToMaybe)
import           Data.Swagger                    (ToSchema)
import           Data.Time                       (Day, UTCTime (..), addDays, secondsToDiffTime)
import           Database.Esqueleto              hiding (groupBy, (*.))
import           GHC.Generics                    (Generic)

import           Moneydb.Computation.Stocks.Math
import           Moneydb.Database
import           Moneydb.Types

-- TODO: API: specify preferred exchange (code)?
-- TODO: broker is not taken into account -> separate by broker? makes sense because of expected withdrawal fees
-- TODO: add from- and to- dates? and valueStart?
-- NOTE: units and invested may include transactions that are younger than the data
data HistoricalStockStatistics = HistoricalStockStatistics { stockId :: StockId, exchangeId :: Maybe StockExchangeId, units :: Double,  invested :: Decimal, transactions :: Int, value :: Maybe Double, date :: Maybe Day, previousValue :: Maybe Double, previousDate :: Maybe Day, stockIrr :: Maybe Double } deriving (Show, Generic, ToJSON, ToSchema)

data HistoricalPortfolioStatistics = HistoricalPortfolioStatistics { stocks :: [HistoricalStockStatistics], portfolioIrr :: Maybe Double } deriving (Show, Generic, ToJSON, ToSchema)

computeHistoricalPortfolioStat :: MonadIO m => UserId -> Day -> ReaderT SqlBackend m HistoricalPortfolioStatistics
computeHistoricalPortfolioStat u d = do
          -- Entity Stock, Entity StockInfo
          ss <- select $ from $ \(stock, info) -> do
                        where_ $ (stock ^. StockIsin) ==. (info ^. StockInfoIsin)
                                    &&. (stock ^. StockOwnerId) ==. val u
                        return (stock, info)

          exs <- select $ from $ \ex -> return ex

          ts <- select $ from $ \(trans, stock) -> do
            where_ $ ((trans ^. StockTransactionStockId) ==. (stock ^. StockId)
                       &&. (stock ^. StockOwnerId) ==. (val u))
                       &&. (trans ^. StockTransactionDate) <. (val $ UTCTime (addDays 1 d) (secondsToDiffTime 0) ) -- TODO: timezone error
            return trans

          ps <- select $ from $ \price -> do
            where_ $ (price ^. StockHistoricalPriceDay) >=. (val $ UTCTime (addDays (-7) d) (secondsToDiffTime 0) )
                      &&. (price ^. StockHistoricalPriceDay) <=. (val $ UTCTime d (secondsToDiffTime 0) )
            return price

          let stats = (\(s, si) -> computeHistoricalStockStat s si exs (entityVal <$> ps) (entityVal <$> ts)) <$> ss
          let stats' = filter ((>0) . transactions . fst) stats

          let pirr = if all (isJust . snd) stats then
                       irr 0.1 $ concat . catMaybes $ fmap snd stats'
                     else Nothing

          return $ HistoricalPortfolioStatistics (fst <$> stats') pirr

-- Exchanges and Prices: collection of known stuff (gets filtered for this Stock, so can include data for other Stocks)
-- Transactions: are filtered for this stock, highest and second highest dates are uses.
-- output: stat, and transactions that can be used for IRR (includes simulated withdrawal of everything at last possible time)
computeHistoricalStockStat :: Entity Stock -> Entity StockInfo -> [Entity StockExchange] -> [StockHistoricalPrice] -> [StockTransaction] -> (HistoricalStockStatistics, Maybe [(Day, Double)])
computeHistoricalStockStat es si exs ps ts = (HistoricalStockStatistics (entityKey es) exId us costs (length ts'') curVal d prevVal prevd sirr, ts''')
   where ps' = filter ((`elem` (entityKey <$> exs')) . stockHistoricalPriceExchangeId) ps
         exs' = filter ((==) (entityKey si) . stockExchangeStockId . entityVal) exs
         ts' = filter ((==) (entityKey es) . stockTransactionStockId) ts
         us = sum $ stockTransactionUnits <$> ts'
         ts'' = (\x -> (utctDay $ stockTransactionDate x, stockTransactionAmount x + stockTransactionFees x)) <$> ts'
         costs = sum $ snd <$> ts''
         ts''' = d >>= (\dd -> fmap (\x -> (\(a, b) -> (a, fromRational $ toRational b)) <$> (ts'' ++ [(dd, (-1) *. x)])) curVal)
         sirr = ts''' >>= irr 0.1
         curVal = fmap (\(p, _) -> us * stockHistoricalPriceClosing p) curData
         prevVal = fmap (\(p, _) -> us * stockHistoricalPriceClosing p) prevData
         days = reverse . fmap head . group . sort $ fmap (utctDay . stockHistoricalPriceDay . fst) pe
         d = listToMaybe days
         prevd = listToMaybe (drop 1 days)
         pe = reverse . sortOn (exchangePriority . snd) . catMaybes $ findExchange exs' <$> ps'
         curData = d >>= (\dd -> listToMaybe $ filter ((==) (UTCTime dd (secondsToDiffTime 0)) . stockHistoricalPriceDay . fst) pe)
         exId = fmap (stockHistoricalPriceExchangeId . fst) curData
         prevData = do
                      (_, ex) <- curData
                      prevdd <- prevd
                      listToMaybe . filter ((==) ex . snd) $ filter ((==) (UTCTime prevdd (secondsToDiffTime 0)) . stockHistoricalPriceDay . fst) pe

findExchange :: [Entity StockExchange] -> StockHistoricalPrice -> Maybe (StockHistoricalPrice, StockExchange)
findExchange ses p = if null ses' then Nothing
                     else Just (p, entityVal $ head ses')
   where ses' = filter ((==) (stockHistoricalPriceExchangeId p) . entityKey) ses
