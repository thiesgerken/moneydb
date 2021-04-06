{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Moneydb.Computation.Statistic (getStatistic, Statistic(..)) where

import           Data.Aeson.TH
import           Data.Decimal                    (Decimal)
import           Data.Int                        (Int64)
import           Data.List                       (sortOn)
import qualified Data.Map                        as Map
import           Data.Maybe                      (fromJust)
import           Data.Swagger                    (ToSchema(..)
                                                , defaultSchemaOptions
                                                , genericDeclareNamedSchema)
import qualified Data.Swagger                    as Swagger
import           Data.Time                       (UTCTime)
import           Database.Persist                (Entity(..))
import           Database.Persist.Sql            (fromSqlKey)
import           GHC.Exts                        (Down(..))
import           GHC.Generics                    (Generic)
import           Moneydb.Types
import qualified Moneydb.Types.Rendering.Expense as RE

data Statistic = Statistic { statId       :: Maybe Int64
                           , statTitle    :: String
                           , statAmount   :: Decimal
                           , statDate     :: Maybe UTCTime
                           , statColor    :: Maybe String
                           , statType     :: String
                           , statChildren :: [Statistic]
                           }
    deriving Generic

instance ToSchema Statistic where
    declareNamedSchema = genericDeclareNamedSchema $
        defaultSchemaOptions { Swagger.fieldLabelModifier = prefixModifier }

$(deriveToJSON defaultOptions { fieldLabelModifier = prefixModifier }
               ''Statistic)

getStatistic :: (Decimal -> Bool)
             -> Int
             -> Int
             -> [Entity Category]
             -> [RenderedExpense]
             -> Statistic
getStatistic op ecount ccount cs rexps = mkCategoryStats ecount ccount es cs
  where
    es = (\e -> e { RE.effectiveAmount = abs (RE.effectiveAmount e) })
        <$> filter (op . RE.effectiveAmount) rexps

partitionExpenses :: [RenderedExpense]
                  -> [Entity Category]
                  -> [(Entity Category, [RenderedExpense])]
partitionExpenses es cs = (\(i, xs) -> (fromJust $ lookup i cs', xs))
    <$> Map.toList grps
  where
    grps = Map.fromListWith (++) [ (RE.categoryId e, [ e ]) | e <- es ]

    cs' = [ (entityKey c, c) | c <- cs ]

truncStats :: Int -> [Statistic] -> [Statistic]
truncStats c xs = take c (rest <> xs')
  where
    total = sum (statAmount <$> xs)

    xs' = sortOn (Down . statAmount) . filter ((/=) 0 . statAmount) $ xs

    rest
        | length xs' > c =
            [ Statistic Nothing
                        ("Rest (+" <> show (length xs' - c + 1) <> ")")
                        (total - sum (statAmount <$> take (c - 1) xs'))
                        Nothing
                        Nothing
                        "rest"
                        []
            ]
        | otherwise = []

mkCategoryStats :: Int
                -> Int
                -> [RenderedExpense]
                -> [Entity Category]
                -> Statistic
mkCategoryStats ec cc es cs =
    Statistic Nothing "" total Nothing Nothing "root" (truncStats cc children)
  where
    children = uncurry (mkCategoryStat ec) <$> partitionExpenses es cs

    total = sum (statAmount <$> children)

mkCategoryStat :: Int -> Entity Category -> [RenderedExpense] -> Statistic
mkCategoryStat ecount (Entity cid c) es =
    Statistic (Just $ fromSqlKey cid)
              (categoryTitle c)
              total
              Nothing
              (Just $ categoryColor c)
              "category"
              (truncStats ecount children)
  where
    children = mkExpenseStat c <$> es

    total = sum (RE.effectiveAmount <$> es)

mkExpenseStat :: Category -> RenderedExpense -> Statistic
mkExpenseStat c x =
    Statistic Nothing
              (RE.title x)
              (RE.effectiveAmount x)
              (Just $ RE.valueDate x)
              (Just $ categoryColor c)
              "expense"
              []
