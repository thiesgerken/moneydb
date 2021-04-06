{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Moneydb.Computation.Prepayment(computePrepayments, PrepaymentInfo(..)) where

import           Control.Monad.IO.Class          (MonadIO)
import           Control.Monad.Trans.Reader      (ReaderT)
import           Data.Aeson.TH
import           Data.Decimal                    (Decimal)
import           Data.List                       (groupBy, sortOn)
import           Data.Swagger                    (ToSchema (..), defaultSchemaOptions, genericDeclareNamedSchema)
import qualified Data.Swagger                    as Swagger
import           Data.Time                       (UTCTime)
import           Database.Esqueleto              hiding (groupBy)
import           GHC.Generics                    (Generic)

import           Moneydb.Database
import           Moneydb.Types
import qualified Moneydb.Types.Rendering.Expense as RE

data PrepaymentInfo = PrepaymentInfo {prepaymentTitle :: String, prepaymentAmount :: Decimal, prepaymentExpenses :: [ExpenseId], prepaymentDate :: UTCTime} deriving Generic

instance ToSchema PrepaymentInfo where declareNamedSchema = genericDeclareNamedSchema $ defaultSchemaOptions { Swagger.fieldLabelModifier = prefixModifier }
$(deriveToJSON defaultOptions{fieldLabelModifier = prefixModifier} ''PrepaymentInfo)

computePrepayments :: MonadIO m => Entity User -> UTCTime -> ReaderT SqlBackend m [PrepaymentInfo]
computePrepayments (Entity uid _) t = do
  as <- select $ from $ \a -> do
            where_ $ ( ((a ^. AccountOwnerId) ==. val uid) &&. ((a ^. AccountKind) ==. val Prepayment) )
                 ||. exists (from $ \(a', as) -> do
                    where_ $ ((((as ^. AccountSyncingAccount1) ==. a' ^. AccountId) &&. ((as ^. AccountSyncingAccount2) ==. (a ^. AccountId))) ||. (((as ^. AccountSyncingAccount2) ==. (a' ^. AccountId)) &&. ((as ^. AccountSyncingAccount1) ==. (a ^. AccountId))))
                       &&. ( (  ((a' ^. AccountKind) ==. val Prepayment) &&. (a ^. AccountOwnerId) ==. val uid)
                          ||. ( ((a ^. AccountKind) ==. val Prepayment)  &&. (a' ^. AccountOwnerId) ==. val uid)
                       )
                    return ()
                   )
            return a
  let aids = entityKey <$> as

  es' <- select $ from $ \e -> do
        where_ $ ((e ^. ExpenseAccountId `in_` valList aids)
                ||. exists (from $ \es -> do
                    where_ $ (es ^. ExpenseSharingAccountId `in_` valList aids)
                          &&. ((es ^. ExpenseSharingExpenseId) ==. (e ^. ExpenseId))
                    return ()))
                &&. notExists (from $ \ef -> do
                    where_ $ (ef ^. ExpenseFlagFlagging ==. val Compensated)
                          &&. ((ef ^. ExpenseFlagExpenseId) ==. (e ^. ExpenseId))
                    return ())
                &&. (e ^. ExpenseValueDate) <=. val t
        orderBy [asc $ e ^. ExpenseTitle]
        return (e ^. ExpenseId, e ^. ExpenseTitle, e ^. ExpenseAmount, e ^. ExpenseValueDate, e ^. ExpenseAccountId)

  let es = lightExpense uid <$> es'
  rexps <- zip (entityKey <$> es) <$> getRenderedExpenses' True uid es

  let gs = truncGroup aids . sortOn (RE.valueDate . snd) <$> groupOn (RE.title . snd) rexps
  let gs' = (\xs@(x:_) -> (PrepaymentInfo (RE.title $ snd x) (sum (impact aids . snd <$> xs)) (fst <$> xs) (minimum $ fmap (RE.valueDate . snd) xs))) <$> filter (not . null) gs

  return . sortOn prepaymentDate . filter (\g -> prepaymentAmount g /= 0) $ gs'

lightExpense :: UserId -> (Value ExpenseId, Value String, Value Decimal, Value UTCTime, Value AccountId) -> Entity Expense
lightExpense uid (Value eid, Value title, Value amount, Value valueDate, Value aid) = Entity eid $ Expense uid amount valueDate Nothing aid (toSqlKey 0) title "" "" "" "" valueDate valueDate "" uid

impact :: [AccountId] -> RenderedExpense -> Decimal
impact aids e = i1 + sum (i2 <$> RE.sharing e)
   where i1 = if RE.accountId e `elem` aids then RE.amount e else 0
         i2 s = if RE.sharingAccountId s `elem` aids then RE.calculatedAmount s else 0

truncGroup :: [AccountId] -> [(ExpenseId, RenderedExpense)] -> [(ExpenseId, RenderedExpense)]
truncGroup aids es = drop i es
  where i = snd . last . filter ((==) 0 . fst) $ zip (scanl (+) 0 (impact aids . snd <$> es)) [0..]

groupOn :: Ord b => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy (\w v -> f w == f v) -- . sortOn f -- sorting now done in sql
