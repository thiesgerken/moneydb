{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module Moneydb.Database.OnvistaGrabber (getInfo, getHistorialData, getCurrentData) where

import           Control.Lens
import           Control.Monad              (when)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Maybe  (runMaybeT)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Lazy.UTF8  as B
import           Data.List                  (intercalate)
import           Data.List.Split            (splitOn)
import           Data.List.Utils            (replace)
import           Data.Maybe                 (catMaybes, fromMaybe, listToMaybe)
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as TL
import           Data.Text.Lazy.Builder     (toLazyText)
import           Data.Time                  (TimeZone, UTCTime (..), defaultTimeLocale, getCurrentTime,
                                             getCurrentTimeZone, parseTimeM, secondsToDiffTime, zonedTimeToUTC)
import           Data.Time.Calendar         (Day, addGregorianYearsClip, fromGregorian, toGregorian)
import           Data.Time.LocalTime        (LocalTime (..), ZonedTime (..), getZonedTime)
import           Database.Persist.Sql       (Entity (..), toSqlKey)
import           HTMLEntities.Decoder       (htmlEncodedText)
import           Moneydb.Types.Stocks
import qualified Network.Wreq               as Wreq
import           Text.Regex.PCRE            ((=~))

-- needle: isin, wkn
-- id on StockExchanges is invalid.
getInfo :: String -> IO (Maybe (StockInfo, [StockExchange]))
getInfo needle = runMaybeT $ do
    let opts = Wreq.defaults & Wreq.param "searchValue" .~ [ T.pack needle ]
    r <- liftIO $ Wreq.getWith opts "https://www.onvista.de/suche"
    let body = bReplace '\n' ' ' $ r ^. Wreq.responseBody

    -- permalink & title
    let r1 = "<a href=\"([^\"]+)\" title=\"([^\"]+)\"[^<>]*>\\s*<h1[^<>]*>[^<>]*</h1>\\s*</a>"
            :: B.ByteString
    let c1 = (body =~ r1) :: [[B.ByteString]]
    when (null c1) $ fail ""

    -- liftIO . print $ fmap tail c1
    let c1' = decodeHtml <$> head c1
    let url = c1' !! 1
    let title = c1' !! 2

    --  type
    let r2 = "<div class=\"item\">\\s*<div class=\"ui mini (\\w+) label\">([^<>]+)</div>\\s*</div>"
            :: B.ByteString
    let c2 = (body =~ r2) :: [[B.ByteString]]
    when (null c2) $ fail ""
    -- liftIO . print $ fmap tail c2
    when ((head c2 !! 1) /= "etf") $ fail ""

    -- WKN, Land (code and name), Fondstyp
    let r3 = "<tr>\\s*<td><b>WKN:</b></td>\\s*<td>\\s*(\\w+)\\s*<i class=\"(\\w+)\" title=\"([\\w ]+) flag\"[^<>]*></i>\\s*</td>\\s*<td><b>Fondstyp:</b></td>\\s*<td[^<>]*>\\s*<span title=\"([^<>]+)\">"
            :: B.ByteString
    let c3 = (body =~ r3) :: [[B.ByteString]]
    when (null c3) $ fail ""
    -- liftIO . print $ fmap tail c3
    let c3' = decodeHtml <$> head c3
    let wkn = c3' !! 1
    let countryCode = c3' !! 2
    let country = c3' !! 3
    let kind = c3' !! 4

    -- ISIN, Schwerpunkt
    let r4 = "<tr>\\s*<td><b>ISIN:</b></td>\\s*<td>(\\w+)</td>\\s*<td><b>Schwerpunkt:</b></td>\\s*<td[^<>]*>\\s*<span title=\"([^<>]+)\">"
            :: B.ByteString
    let c4 = (body =~ r4) :: [[B.ByteString]]
    when (null c4) $ fail ""
    -- liftIO . print $ fmap tail c4
    let c4' = decodeHtml <$> head c4
    let isin = c4' !! 1
    let focus = c4' !! 2

    -- KAG
    let r5 = "<tr>\\s*<td><b>KAG:</b></td>\\s*<td[^<>]*>\\s*<a[^<>]+title=\"([^\"]+)\""
            :: B.ByteString
    let c5 = (body =~ r5) :: [[B.ByteString]]
    when (null c5) $ fail ""
    -- liftIO . print $ fmap tail c5
    let c5' = decodeHtml <$> head c5
    let managingCompany = c5' !! 1

    -- exchanges (id, title, code)
    -- first match on first select-exchange div, then all item matches inside of that.
    let r6 = "<div id=\"select-exchange\".*?</div>\\s*</div>" :: B.ByteString
    let c6 = (body =~ r6) :: B.ByteString
    let r7 = "data-value=\"(\\d+)[^<>]*data-contributor=\"([^\"]+)\"[^<>]*data-exchange=\"([\\w\\d]+)\"[^<>]*data-id-exchange=\"(\\d+)\""
            :: B.ByteString
    let c7 = (c6 =~ r7) :: [[B.ByteString]]
    when (null c7) $ fail ""
    -- liftIO . print $ fmap tail c7
    let c7' = fmap decodeHtml <$> c7
    let exs = fmap (\xs -> StockExchange (toSqlKey 0)
                                         (xs !! 2)
                                         (xs !! 3)
                                         (read $ xs !! 1)
                                         (read $ xs !! 4))
                   c7'

    now <- liftIO getCurrentTime
    return ( StockInfo isin
                       wkn
                       url
                       kind
                       focus
                       managingCompany
                       title
                       countryCode
                       country
                       now
                       now
           , exs
           )

bReplace :: Char -> Char -> B.ByteString -> B.ByteString
bReplace o r = BL.map f
  where
    f c
        | c == o = r
    f c = c

show2 :: Int -> String
show2 i = reverse . take 2 $ reverse ("0" ++ show i)

pn :: Read a => String -> a
pn = read . replace "," "." . filter (/= '.')

parseHistorical :: StockExchangeId -> [String] -> StockHistoricalPrice
parseHistorical seid [ dd, mm, yyyy, opening, high, low, closing, volume ] =
    StockHistoricalPrice seid
                         (UTCTime (fromGregorian (pn yyyy) (pn mm) (pn dd))
                                  (secondsToDiffTime 0))
                         (pn opening)
                         (pn closing)
                         (pn high)
                         (pn low)
                         (nonneg $ pn volume)
  where
    nonneg x
        | x >= 0 = Just x
    nonneg _ = Nothing
parseHistorical _ _ = error "parseHistorical :: wrong number of args"

-- very slow, only use on small snippets!
decodeHtml :: B.ByteString -> String
decodeHtml = TL.unpack . toLazyText . htmlEncodedText . T.pack . B.toString

-- returns at most five years. run multiple times to get data that is older.
getHistorialData :: Entity StockExchange -> Maybe Day
                 -> IO [StockHistoricalPrice]
getHistorialData se day = do
    ZonedTime (LocalTime today _) _ <- getZonedTime
    let startDay = fromMaybe (addGregorianYearsClip (-5) today) day
    let startDay' = concat [ show2 d, ".", show2 m, ".", show y ]
          where
            (y, m, d) = toGregorian startDay
    let opts = Wreq.defaults & Wreq.param "idNotation"
            .~ [ T.pack . show . stockExchangeOnvistaRecordId $ entityVal se ]
            & Wreq.param "kag" .~ [ "false" ]
            & Wreq.param "datetimeTzStartRange" .~ [ T.pack startDay' ]
            & Wreq.param "timeSpan" .~ [ "5Y" ]
            & Wreq.param "codeResolution" .~ [ "1D" ]
    r <- liftIO $
        Wreq.getWith opts "https://www.onvista.de/etf/snapshotHistoryCSV" -- snapshotHistory[Print|CSV]
    let body = bReplace '\r' ' ' $ r ^. Wreq.responseBody

    -- Datum, Eröffnung, Hoch, Tief, Schluss, Volumen
    -- let regex = "<tr>\\s*<td[^<>]*>(\\d{1,2})\\.(\\d{1,2})\\.(\\d{4})</td>\\s*<td>([\\d,.]+)</td>\\s*<td>([\\d,.]+)</td>\\s*<td>([\\d,.]+)</td>\\s*<td>([\\d,.]+)</td>\\s*<td>([\\d,.]+)</td>\\s*</tr>" :: B.ByteString
    let regex = "\\n(\\d{1,2})\\.(\\d{1,2})\\.(\\d{4});([\\d,.]+);([\\d,.]+);([\\d,.]+);([\\d,.]+);([\\d,.]+)" :: B.ByteString
    let c = (body =~ regex) :: [[B.ByteString]]
    return $ parseHistorical (entityKey se) . fmap B.toString . tail <$> c

parseCurrentDataRow :: TimeZone -> [Entity StockExchange] -> B.ByteString -> Maybe StockCurrentPrice
parseCurrentDataRow tz exs row = do
        let mm x = decodeHtml . (!! 1) <$> listToMaybe (row =~ (x :: B.ByteString))

        i <- mm "data-id-notation=\"(\\d+)\""
        p <- mm "<span class=\"price [^<>]+>\\s*(.+?)\\s*</span>"
        -- c <- mm "<span class=\"price-currency[^<>]+>\\s*(.+?)\\s*</span>"
        t <- mm "<span class=\"price-datetime[^<>]+>\\s*(.+?)\\s*</span>"

        t' <- parseTimeM True defaultTimeLocale "%d.%m.%Y, %T" t
        let t'' = zonedTimeToUTC $ ZonedTime t' tz

        key <- listToMaybe $ filter ((==) (read i) . stockExchangeOnvistaRecordId . entityVal) exs

        return $ StockCurrentPrice (entityKey key) t'' (pn p)

getCurrentData :: [Entity StockExchange] -> StockInfo -> IO [StockCurrentPrice]
getCurrentData exs si = do
    let spls = splitOn "/" . tail $ stockInfoOnvistaUrl si
    let url = "https://www.onvista.de/" ++ head spls ++ "/handelsplaetze/" ++ (intercalate "/" (tail spls))

    r <- liftIO $ Wreq.get url
    let body = bReplace '\n' ' ' . bReplace '\r' ' ' $ r ^. Wreq.responseBody

    let regexRow = "<tr class=\"stock-exchange-row\"[^<>]+>.*?</tr>" :: B.ByteString
    let rows = (body =~ regexRow) :: [[B.ByteString]]

    tz <- getCurrentTimeZone
    return . catMaybes $ parseCurrentDataRow tz exs . head <$> rows
