{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Moneydb.Api.Computation (ComputeAPI, computeHandler) where

import           Control.Monad                  (when)
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Logger
import           Data.Maybe                     (fromMaybe)
import           Data.Time                      (Day, LocalTime (..), UTCTime, ZonedTime (..), getZonedTime)
import           Database.Esqueleto
import           Moneydb.Computation.Overview
import           Moneydb.Computation.Prepayment
import           Moneydb.Computation.Simulation
import           Moneydb.Computation.Stocks
import           Moneydb.Database
import           Moneydb.Types
import           Servant.API
import           Servant.Server

-- | a type to wrap our private api
type ComputeAPI = "compute"
    :> ("simulation" :> QueryParam "direction" SearchDirection
        :> Capture "time" UTCTime :> Get '[JSON] Simulation :<|> "simulations"
        :> QueryParam "direction" SearchDirection :> QueryParam "begin" UTCTime
        :> QueryParam "end" UTCTime :> QueryParam "pointspermonth" Int
        :> Get '[JSON] Simulations :<|> "prepayments"
        :> Capture "time" UTCTime :> Get '[JSON] [PrepaymentInfo]
        :<|> "overview" :> QueryParam "detail" Int
        :> Capture "year" Integer :> Capture "month" Int :> Get '[JSON] Overview
        :<|> "overviews" :> QueryParam "detail" Int :> Get '[JSON] Overviews
        :<|> "stocks" :> "portfolio" :> "historic" :> QueryParam "day" Day :> Get '[JSON] HistoricalPortfolioStatistics
        :<|> "stocks" :> "portfolio" :> QueryParam "refresh" Bool :> Get '[JSON] PortfolioStatistics)

computeHandler :: MoneyDBConn -> Entity User -> Server ComputeAPI
computeHandler mc u = simHandler :<|> simsHandler :<|> prepaymentsHandler
    :<|> ovHandler :<|> ovsHandler :<|> historicPortfolioHandler :<|> portfolioHandler
  where
    simHandler d t = liftIO $ runTransaction mc $
        simulate u (fromMaybe Both d) t

    simsHandler d ta tb ppm = liftIO $ runTransaction mc $
        simulates u (fromMaybe Both d) ta tb (fromMaybe 1 ppm)

    ovHandler d y m = liftIO $ runTransaction mc $
        computeOverview' u (fromMaybe 10 d) (fromMaybe 10 d) y m

    ovsHandler d = liftIO $ runTransaction mc $
        computeOverviews u (fromMaybe 10 d) (fromMaybe 10 d)

    prepaymentsHandler t = liftIO $ runTransaction mc $ computePrepayments u t

    historicPortfolioHandler d = liftIO $ do
         d' <- case d of
                    Just dd -> return dd
                    Nothing -> do
                        ZonedTime (LocalTime today _) _ <- getZonedTime
                        return today

         runTransaction mc $ computeHistoricalPortfolioStat (entityKey u) d'

    portfolioHandler refresh = liftIO $ do
         when (fromMaybe False refresh) . (`runLoggingT` (moneyLogger mc)) . runTransaction mc $ updateAllCurrent (entityKey u)

         runTransaction mc $ computePortfolioStat (entityKey u)
