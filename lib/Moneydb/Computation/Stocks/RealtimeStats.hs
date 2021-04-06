{-# LANGUAGE CPP             #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Moneydb.Computation.Stocks.RealtimeStats(computePortfolioStat, PortfolioStatistics(..), StockStatistics(..)) where

import           Control.Monad.IO.Class          (MonadIO, liftIO)
import           Control.Monad.Trans.Reader      (ReaderT)
import           Data.Aeson                      (ToJSON)
import           Data.Decimal                    (Decimal, (*.))
import           Data.List                       (sortOn)
import           Data.Maybe                      (catMaybes, isJust, listToMaybe)
import           Data.Ord                        (Down (..))
import           Data.Swagger                    (ToSchema)
import           Data.Time                       (Day, UTCTime (..), addUTCTime, getCurrentTime, nominalDay,
                                                  secondsToDiffTime)
import           Data.Tuple.Utils                (fst3, snd3, thd3)
import           Database.Esqueleto              hiding (groupBy, (*.))
import           GHC.Generics                    (Generic)

import           Moneydb.Computation.Stocks.Math
import           Moneydb.Database
import           Moneydb.Types

-- TODO: API: specify preferred exchange (code)?
-- TODO: broker is not taken into account -> separate by broker? makes sense because of expected withdrawal fees
-- TODO: add from- and to- dates? and valueStart?

data StockStatistics = StockStatistics { stockId :: StockId, units :: Double,  invested :: Decimal, transactions :: Int, realtimeValue :: Maybe (Double, UTCTime, StockExchangeId), historicValue :: Maybe (Double, Day, StockExchangeId), realtimeIrr :: Maybe Double, historicIrr :: Maybe Double} deriving (Show, Generic, ToJSON, ToSchema)

-- irr makes only sense over the whole data range here (otherwise it would have to include multiple data)
data PortfolioStatistics = PortfolioStatistics { stocks :: [StockStatistics], portfolioRealtimeIrr :: Maybe Double, portfolioHistoricIrr :: Maybe Double} deriving (Show, Generic, ToJSON, ToSchema)

computePortfolioStat :: MonadIO m => UserId -> ReaderT SqlBackend m PortfolioStatistics
computePortfolioStat u = do
          now <- liftIO getCurrentTime

          -- Entity Stock, Entity StockInfo
          ss <- select $ from $ \(stock, info) -> do
                        where_ $ (stock ^. StockIsin) ==. (info ^. StockInfoIsin)
                                    &&. (stock ^. StockOwnerId) ==. val u
                        return (stock, info)

          ts <- select $ from $ \(trans, stock) -> do
            where_ $ ((trans ^. StockTransactionStockId) ==. (stock ^. StockId)
                       &&. (stock ^. StockOwnerId) ==. (val u))
                       &&. (trans ^. StockTransactionDate) <. val now
            return trans

          ps' <- select $ from $ \(ex,p) -> do
            where_ $ (p ^. StockHistoricalPriceExchangeId) ==. (ex ^. StockExchangeId)
                    &&. (p ^. StockHistoricalPriceDay) >=. (val $ addUTCTime (-7*nominalDay) now)

            orderBy [desc $ p ^. StockHistoricalPriceDay]
            return (ex, p)

          rps' <- select $ from $ \(ex,p) -> do
              where_ $ (p ^. StockCurrentPriceExchangeId) ==. (ex ^. StockExchangeId)
                      &&. (p ^. StockCurrentPriceDate) >=. (val $ addUTCTime (-7*nominalDay) now)

              orderBy [desc $ p ^. StockCurrentPriceDate]
              return (ex, p)

          let ps = sortOn (Down . exchangePriority . fst) . sortOn (Down . stockHistoricalPriceDay . snd) $ (\(a, b) -> (entityVal a, entityVal b)) <$> ps'
          -- let ps = ps1++ps2 where (ps1,ps2) = partition ((>0) . exchangePriority . fst) ps''

          let rps = sortOn (Down . exchangePriority . fst) . sortOn (Down . stockCurrentPriceDate . snd) $ (\(a, b) -> (entityVal a, entityVal b)) <$> rps'
          -- let rps = rps1++rps2 where (rps1,rps2) = partition ((>0) . exchangePriority . fst) rps''

          -- ps <- select $ from $ \price -> do
          --   where_ $ (price ^. StockHistoricalPriceDay) >=. (val $ UTCTime (addDays (-7) d) (secondsToDiffTime 0) )
          --             &&. (price ^. StockHistoricalPriceDay) <=. (val $ UTCTime d (secondsToDiffTime 0) )
          --   return price

          let stats = (\(s, si) -> computeStockStat s si ps rps (entityVal <$> ts)) <$> ss
          let stats' = filter ((>0) . transactions . fst3) stats

          let pirrR = if all (isJust . snd3) stats then
                       irr' 0.02 $ concat . catMaybes $ fmap snd3 stats'
                       else Nothing

          let pirrH = if all (isJust . thd3) stats then
                        irr 0.02 $ concat . catMaybes $ fmap thd3 stats'
                        else Nothing

          return $ PortfolioStatistics (fst3 <$> stats') pirrR pirrH

-- TODO: API endpoint to update realtime data for all stocks
-- TODO: rewrite / amend web tab (incl. refresh button to get new data)
-- TODO: filtering of desired exchanges? activate them in web dropdown menu?

-- Exchanges and Prices: collection of known stuff (gets filtered for this Stock, so can include data for other Stocks)
-- Transactions: are filtered for this stock, highest and second highest dates are used.
-- output: stat, and transactions that can be used for IRR (includes simulated withdrawal of everything at last possible time)
computeStockStat :: Entity Stock -> Entity StockInfo -> [(StockExchange, StockHistoricalPrice)] -> [(StockExchange, StockCurrentPrice)] -> [StockTransaction] -> (StockStatistics, Maybe [(UTCTime, Double)], Maybe [(Day, Double)])
computeStockStat es si ps rps ts = (StockStatistics (entityKey es) curUnits costs (length ts'') rVal hVal rIrr hIrr, tsRealtime, tsHistoric)
   where hDatum = listToMaybe $ filter ((==) (entityKey si) . stockExchangeStockId . fst) ps
        --  hDatum = listToMaybe $ case rDatum of
        --                 Nothing -> hDatum'
        --                 Just (_, scp) -> filter ((>) (utctDay $ stockCurrentPriceDate scp) . utctDay . stockHistoricalPriceDay . snd) hDatum'
         rDatum = listToMaybe $ filter ((==) (entityKey si) . stockExchangeStockId . fst) rps
         rVal = fmap (\(_, p) -> (curUnits * stockCurrentPricePrice p, stockCurrentPriceDate p, stockCurrentPriceExchangeId p)) rDatum
         hVal = fmap (\(_, p) -> (curUnits * stockHistoricalPriceClosing p, utctDay (stockHistoricalPriceDay p), stockHistoricalPriceExchangeId p)) hDatum
         ts' = filter ((==) (entityKey es) . stockTransactionStockId) ts
         ts'' = (\x -> (stockTransactionDate x, stockTransactionAmount x + stockTransactionFees x)) <$> ts'
         tsHistoric = fmap (\(x, d, _) -> (\(a, b) -> (utctDay a, fromRational $ toRational b)) <$> (ts'' ++ [(UTCTime d (secondsToDiffTime 0), (-1) *. x)])) hVal
         tsRealtime = fmap (\(x, d, _) -> (\(a, b) -> (a, fromRational $ toRational b)) <$> (ts'' ++ [(d, (-1) *. x)])) rVal
         curUnits = sum $ stockTransactionUnits <$> ts'
         costs = sum $ snd <$> ts''
         hIrr = tsHistoric >>= irr 0.1
         rIrr = tsRealtime >>= irr' 0.1

-- findExchange :: [Entity StockExchange] -> StockHistoricalPrice -> Maybe (StockHistoricalPrice, StockExchange)
-- findExchange ses p = if null ses' then Nothing
--                      else Just (p, entityVal $ head ses')
--    where ses' = filter ((==) (stockHistoricalPriceExchangeId p) . entityKey) ses
