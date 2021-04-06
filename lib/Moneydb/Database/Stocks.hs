{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module Moneydb.Database.Stocks (checkStocks, exchangePriority, updateAllCurrent) where

import           Control.Monad                   (unless)
import           Control.Monad.IO.Unlift         (MonadUnliftIO, liftIO)
import           Control.Monad.Logger
import           Control.Monad.Trans.Reader      (ReaderT)
import           Control.Monad.Trans.Resource    (ResourceT)
import           Data.List                       (elemIndex, partition, sortOn)
import           Data.List.Extra                 (groupOn)
import           Data.Maybe                      (catMaybes, fromMaybe, isNothing, listToMaybe)
import qualified Data.Text                       as T
import           Data.Time                       (Day, LocalTime (..), UTCTime (..), ZonedTime (..),
                                                  addGregorianYearsClip, diffUTCTime, fromGregorian, getCurrentTime,
                                                  getZonedTime, nominalDay, secondsToDiffTime)
import           Database.Esqueleto              hiding (isNothing)
import           Moneydb.Database.OnvistaGrabber
import           Moneydb.Types

checkStocks :: (MonadLogger m, MonadUnliftIO m) => ReaderT SqlBackend (ResourceT m) ()
checkStocks = do
    infos <- select $ from $ \(s `LeftOuterJoin` si) -> do
        on (just (s ^. StockIsin) ==. si ?. StockInfoIsin)
        return (s ^. StockIsin, si)
    now <- liftIO getCurrentTime

    let noInfos = filter (isNothing . snd) infos
    unless (null noInfos) . logInfoN $ mconcat ["Querying Infos for ", T.pack . show $ length noInfos, " stocks"]
    mapM_ (addStock . unValue . fst) noInfos

    let oldHistorical = filter ((> (6.0/24.0) * nominalDay) . diffUTCTime now . stockInfoLastHistoricalUpdate . entityVal) . catMaybes $ snd <$> infos
    exsH <- select $ from $ \ex -> do
        where_ $ ex ^. StockExchangeStockId `in_` (valList $ entityKey <$> oldHistorical)
        return ex
    unless (null oldHistorical) . logInfoN $ mconcat ["Updating Historical Data for ", T.pack . show $ length oldHistorical, " stocks"]
    mapM_ (updateStockHistorical exsH) oldHistorical

    let oldCurrent = filter ((> (2.0/24.0) * nominalDay) . diffUTCTime now . stockInfoLastCurrentUpdate . entityVal) . catMaybes $ snd <$> infos
    unless (null oldCurrent) . logInfoN $ mconcat ["Updating Current Data for ", T.pack . show $ length oldCurrent, " stocks"]
    exsC <- select $ from $ \ex -> do
        where_ $ ex ^. StockExchangeStockId `in_` (valList $ entityKey <$> oldCurrent)
        return ex
    mapM_ (updateStockCurrent exsC) oldCurrent

updateAllCurrent :: (MonadLogger m, MonadUnliftIO m) => UserId -> ReaderT SqlBackend (ResourceT m) ()
updateAllCurrent u = do
    infos <- select $ from $ \(s, si) -> do
        where_ $ ((s ^. StockIsin) ==. si ^. StockInfoIsin)
                 &&. ((s ^. StockOwnerId) ==. val u)
        return si
    now <- liftIO getCurrentTime

    let oldCurrent = filter ((> 0.00 * nominalDay) . diffUTCTime now . stockInfoLastCurrentUpdate . entityVal) infos
    unless (null oldCurrent) . logInfoN $ mconcat ["Updating Current Data for ", T.pack . show $ length oldCurrent, " stocks"]
    exsC <- select $ from $ \ex -> do
        where_ $ ex ^. StockExchangeStockId `in_` (valList $ entityKey <$> oldCurrent)
        return ex
    mapM_ (updateStockCurrent exsC) oldCurrent

exchangeWhitelist :: [String]
exchangeWhitelist = ["GER", "QUO", "GAT", "FRA", "LUSG", "STU", "HAM", "MUN", "BER", "DUS"]

exchangePriority :: StockExchange -> Int
exchangePriority se = fromMaybe 0 $ elemIndex (stockExchangeCode se) (reverse exchangeWhitelist)

-- | returns 6 exchanges; prefers elements of the whitelist.
exchangesToDownload :: [Entity StockExchange] -> [Entity StockExchange]
exchangesToDownload xs = take 5 $ ws ++ os
    where (ws, os) = partition ((`elem` exchangeWhitelist) . stockExchangeCode . entityVal) $ sortOn entityKey xs

addStock :: (MonadLogger m, MonadUnliftIO m) => String -> ReaderT SqlBackend (ResourceT m) ()
addStock isin = do
    logInfoN $ "Trying to add info and data for " <> T.pack isin
    info' <- liftIO $ getInfo isin

    case info' of
        Nothing -> do
            logErrorN $ "onvista lookup failed for " <> T.pack isin <> ", adding dummy entry"
            now <- liftIO getCurrentTime
            insert_ $ StockInfo isin "" "" "" "" "" "[Unknown ISIN]" "" "" now now
        Just (info, exchanges') -> do
            infoId <- insert info
            let exchanges'' = (\ex -> ex { stockExchangeStockId = infoId }) <$> exchanges'
            exis <- insertMany exchanges''
            let exchanges = zipWith Entity exis exchanges''
            logInfoN $ "Added info for isin, and " <> (T.pack . show $ length exchanges) <> " of its trading places"

            mapM_ (addExchangeData Nothing) $ exchangesToDownload exchanges
            updateStockCurrent exchanges (Entity infoId info)

addExchangeData :: (MonadLogger m, MonadUnliftIO m) => Maybe Day -> Entity StockExchange -> ReaderT SqlBackend (ResourceT m) ()
addExchangeData day se = do
    let se' = entityVal se
    logInfoN $ "Querying info for exchange record " <> (T.pack . show $ stockExchangeOnvistaRecordId se') <> " (at exchange '" <> T.pack (stockExchangeName se') <> "')"

    let startDay = fromMaybe (fromGregorian 1900 1 1) day
    ZonedTime (LocalTime today _) _ <- liftIO getZonedTime
    let starts' = tail $ iterate (addGregorianYearsClip (-4)) today
    let starts = takeWhile (\x -> x > startDay) starts' ++ [startDay]

    rxs <- whileValues $ flip fmap starts $ \t -> do
            logInfoN $ "Trying to query 5 years of data starting from " <> T.pack (show t)
            xs <- liftIO $ getHistorialData se (Just t)
            logInfoN $ "Got " <> T.pack (show $ length xs) <> " value(s)"
            -- unless (null xs) $ liftIO (threadDelay 500000) -- wait a second or so
            return xs

    let rxs' = filter (\x -> stockHistoricalPriceDay x > UTCTime startDay (secondsToDiffTime 0)) $ rmdups stockHistoricalPriceDay rxs
    logInfoN $ "Adding " <> T.pack (show $ length rxs') <> " record(s)"
    insertMany_ rxs'

-- safe to call instead of addExchangeData; calls it after determining last datum in database.
updateExchangeData :: (MonadLogger m, MonadUnliftIO m) => Entity StockExchange -> ReaderT SqlBackend (ResourceT m) ()
updateExchangeData se = do
    lastR' <- select $ from $ \x -> do
        where_ $ (x ^. StockHistoricalPriceExchangeId) ==. val (entityKey se)
        orderBy [ desc $ x ^. StockHistoricalPriceDay ]
        limit 1
        return $ x ^. StockHistoricalPriceDay

    -- logInfoN $ "Last record in database: " <> (T.pack . show . utctDay . unValue . head $ lastR')
    let lastR = fmap (utctDay . unValue) . listToMaybe $ lastR'
    addExchangeData lastR se

updateStockHistorical :: (MonadLogger m, MonadUnliftIO m) => [Entity StockExchange] -> Entity StockInfo -> ReaderT SqlBackend (ResourceT m) ()
updateStockHistorical exs (Entity si se) = do
    logInfoN $ "Asking for historical data for " <> (T.pack . show $ stockInfoIsin se)
    mapM_ updateExchangeData . exchangesToDownload $ filter ((==) si . stockExchangeStockId . entityVal) exs

    now <- liftIO getCurrentTime
    update $ \x -> do
        set x [ StockInfoLastHistoricalUpdate =. val now ]
        where_ $ (x ^. StockInfoId) ==. val si

updateStockCurrent :: (MonadLogger m, MonadUnliftIO m) => [Entity StockExchange] -> Entity StockInfo -> ReaderT SqlBackend (ResourceT m) ()
updateStockCurrent exs (Entity si se) = do
    logInfoN $ "Asking for realtime data for " <> (T.pack . show $ stockInfoIsin se)

    dta <- liftIO $ getCurrentData exs se
    logInfoN $ "Adding data for " <> T.pack (show $ length dta) <> " exchanges"
    insertMany_ dta

    now <- liftIO getCurrentTime
    update $ \x -> do
        set x [ StockInfoLastCurrentUpdate =. val now ]
        where_ $ (x ^. StockInfoId) ==. val si

rmdups :: (Ord b) => (a->b) -> [a] -> [a]
rmdups f = map head . groupOn f . sortOn f

whileValues :: Monad m => [m [a]] -> m [a]
whileValues [] = return []
whileValues (x:xs) = do
    vx <- x

    if null vx then return []
    else do
        vxs <- whileValues xs
        return $ vx ++ vxs
