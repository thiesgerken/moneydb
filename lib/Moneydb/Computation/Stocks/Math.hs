{-# LANGUAGE CPP             #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Moneydb.Computation.Stocks.Math(irr, irr') where

import           Data.List   (sortOn)
import           Data.Time   (Day, UTCTime (..))

#if MIN_VERSION_time(1,9,0)
import           Data.Time   (CalendarDiffDays (..), diffGregorianDurationClip)
#else
import           Data.Time   (diffDays)
#endif

import           Debug.Trace

-- newton method with projection of x to >= -0.999
newton :: (Double -> Double) -> (Double -> Double) -> Int -> Double -> Maybe Double
newton f df n x
    | x < (-0.999) = newton f df n (-0.999)
    | n >= 100 = trace "n>=100" Nothing
    | fx /= 0.0 && dfx == 0.0 = trace "f(x) /= 0 && f'(x) == 0" Nothing
    | abs fx < 1e-2 = Just x
    -- | abs (x'-x) <= 1e-4 = Just x'
    | otherwise = newton f df (n+1) x'
                    where x' = x - fx / dfx
                          fx = f x
                          dfx = df x
                          -- x' = trace ("x = " ++ show x) $ x - fx / dfx
                          -- fx = trace ("f(x) = " ++ show (f x)) $ f x
                          -- dfx = trace ("f'(x) = " ++ show (df x)) $ df x

irrF :: [(Double, Double)] -> Double -> Double
irrF [] _          = 0.0
irrF ((t, y):ts) x = (irrF ts x) + y * ((1+x) ** (-t))

irrdF :: [(Double, Double)] -> Double -> Double
irrdF [] _          = 0.0
irrdF ((t, y):ts) x = (irrdF ts x) - t * y * ((1+x) ** (-t-1))

-- with initial guess for rate
irr :: Double -> [(Day, Double)] -> Maybe Double
irr _ [] = Nothing
irr x0 ts' = newton (irrF xs) (irrdF xs) 0 x0
      where times = fracYearDiff (fst $ head ts) . fst <$> ts
            ts = sortOn fst ts'
            xs = zipWith (\a (_, b) -> (a,b)) times ts

-- with initial guess for rate
irr' :: Double -> [(UTCTime, Double)] -> Maybe Double
irr' _ [] = Nothing
irr' x0 ts' = newton (irrF xs) (irrdF xs) 0 x0
      where times = fracYearDiff' (fst $ head ts) . fst <$> ts
            ts = sortOn fst ts'
            xs = zipWith (\a (_, b) -> (a,b)) times ts

fracYearDiff' :: UTCTime -> UTCTime -> Double
fracYearDiff' (UTCTime a1 a2) (UTCTime b1 b2) = fracYearDiff a1 b1 + rest / 365.25
   where rest = (fromRational . toRational $ b2 - a2) / (24*3600)

fracYearDiff :: Day -> Day -> Double
#if MIN_VERSION_time(1,9,0)
fracYearDiff a b = fromIntegral ms / 12.0 + fromIntegral ds / 365.25 -- requires time >= 1.9
  where CalendarDiffDays ms ds = diffGregorianDurationClip b a
#else
fracYearDiff a b = (fromIntegral $ diffDays b a) / 365.25
#endif
