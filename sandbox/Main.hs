{-# LANGUAGE OverloadedStrings #-}

module Main (main, defaultConnect) where

import           Control.Monad           (when)
import           Control.Monad.IO.Unlift (MonadUnliftIO, liftIO)
import           Control.Monad.Logger
import qualified Data.ByteString.Char8   as B8
import qualified Data.Text               as T
import           System.Console.GetOpt
import           System.Environment      (getArgs, getProgName)
import           System.Exit             (exitSuccess)
import           System.IO
-- import           Criterion.Main
-- import           Data.Aeson                     (encode)
-- import           Data.Maybe                     (fromJust)
-- import           Data.Time                      (fromGregorian, getCurrentTime)
-- import           Database.Esqueleto             hiding (loadConfig)

import           Moneydb.Config
import           Moneydb.Database
-- import           Moneydb.Computation.Overview
-- import           Moneydb.Computation.Prepayment
-- import           Moneydb.Computation.Simulation
-- import           Moneydb.Computation.Stocks
-- import           Moneydb.Database.OnvistaGrabber
-- import           Moneydb.Types

data Options = Options  { optConfigPath :: String,
                          optVerbose    :: Bool,
                          optAction     :: Action
                        }

data Action = Help | Sandbox deriving (Show, Eq)

startOptions :: Options
startOptions = Options  { optConfigPath = "/etc/moneydb/money.cfg",
                          optAction = Sandbox,
                          optVerbose = False
                        }

options :: [ OptDescr (Options -> Options) ]
options =
    [
      Option "h" ["help"] (NoArg (\opt -> opt {optAction = Help}))
        "Show this help"
    , Option "v" ["verbose"] (NoArg (\opt -> opt {optVerbose = True}))
      "More verbose output"
    , Option "c" ["cfg"] (ReqArg (\s opt -> opt {optConfigPath = s}) "[path]")
        "path to config file,\ndefaults to /etc/moneydb/money.cfg"
    ]

showHelp :: String -> String
showHelp prg  = usageInfo (concat ["Usage: ", prg, " [options]"]) options

logger :: LogLevel -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
logger minlvl loc src lvl msg
  | lvl == LevelError = logger' stderr loc src lvl msg
  | lvl >= minlvl = logger' stdout loc src lvl msg
  | otherwise = return ()

logger' :: Handle -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
logger' h loc src level msg = B8.hPutStr h (fromLogStr $ defaultLogStr loc src level msg)

-- test code goes here
sandbox :: MonadUnliftIO m => MoneyDBConn -> m ()
sandbox _ = liftIO $ do

    undefined

    -- let ts = [(fromGregorian 2018 1 1, 1000)]
    -- let current = [(fromGregorian 2019 1 1, -1100)]
    -- let ts' = ts ++ current

    -- u' <- runTransaction mc $ getBy $ UserNameKey "thies"
    -- let u = fromJust u'
    -- pstat <- runTransaction mc $ computePortfolioStat (entityKey u) (fromGregorian 2019 5 10)

    -- liftIO $ print pstat
    -- print $ irr' 0.1 ts'

    -- Just (info, exs) <- getInfo "IE00B4L5Y983"

    -- print info
    -- print (head exs)
    -- -- x <- getHistorialData (Entity (toSqlKey 0) (head exs)) Nothing
    -- -- print (length x)

    -- ds <- getCurrentData info (zipWith (\i -> Entity (toSqlKey i)) [1..] exs)
    -- print ds

    -- u' <- runTransaction mc $ getBy $ UserNameKey "thies"
    -- let u = fromJust u'
    -- t <- liftIO getCurrentTime
    -- liftIO $ defaultMain [
    --   bench "computeOverviews" . nfIO . runTransaction mc $ encode <$> computeOverviews u 10 10,
    --   bench "computePrepayments" . nfIO . runTransaction mc $ encode <$> computePrepayments u t,
    --   bench "simulate" . nfIO . runTransaction mc $ encode <$> simulate u Both t,
    --   bench "simulate'" . nfIO . runTransaction mc $ encode <$> simulate' u Both t,
    --   bench "simulates" . nfIO . runTransaction mc $ encode <$> simulates u Both Nothing Nothing 2
    --   ]

defaultConnect :: IO MoneyDBConn
defaultConnect = do
  cfg <- loadConfig ""
  connect cfg (logger LevelInfo)

-- | Run the server
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  args <- getArgs
  prg <- getProgName

  -- Parse options, getting a list of option actions
  let (actions, _, errors) = getOpt RequireOrder options args

  when (errors /= []) $
    hPutStrLn stderr (concat [concat errors, "\n", showHelp prg]) -- >> exitWith (ExitFailure 1)

  -- thread startOptions through all supplied options
  let opts = foldl (\o f -> f o) startOptions actions

  case optAction opts of
    Help -> putStrLn (showHelp prg) >> exitSuccess
    _    -> return ()

  cfg <- loadConfig (optConfigPath opts)
  let minlvl = if optVerbose opts then LevelDebug else LevelInfo

  (`runLoggingT` logger minlvl) $ do
      mc <- connect cfg (logger minlvl)

      logDebugN (T.pack $ show cfg)
      sandbox mc

      commit mc
