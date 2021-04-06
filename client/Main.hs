{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main(main) where

import           Control.Monad              (when)
import           Data.Aeson                 (FromJSON (..), ToJSON (..), eitherDecode, object, withObject, (.:), (.=))
import qualified Data.ByteString.Char8      as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL8
import           Data.Decimal
import           Data.Int                   (Int64)
import           Data.Proxy                 (Proxy (..))
import           Data.Time
import           Database.Persist.Sql       (toSqlKey)
import           Network.HostName
import           Network.HTTP.Client        (defaultManagerSettings, newManager)
import           Network.HTTP.Client.TLS    (tlsManagerSettings)
import           Servant.API
import           Servant.Client
import           System.Console.GetOpt
import           System.Environment         (getArgs, getProgName)
import           System.Exit                (ExitCode (..), exitSuccess, exitWith)
import           System.IO                  (hPutStrLn, stderr)

import           Moneydb.Api.Automation
import           Moneydb.Api.Element
import           Moneydb.Types

-- Int and not real type to avoid imports.
type ProtectedAutomationAPI = (BasicAuth "MoneyDB" Int :> "api" :> AutomationAPI)
type ProtectedAccountAPI = (BasicAuth "MoneyDB" Int :> "api" :> "accounts" :> ElementAPI Account)
type ProtectedBalanceAPI = (BasicAuth "MoneyDB" Int :> "api" :> "balances" :> ElementAPI Balance)

automationAPI :: Proxy ProtectedAutomationAPI
automationAPI = Proxy

accountAPI :: Proxy ProtectedAccountAPI
accountAPI = Proxy

balanceAPI :: Proxy ProtectedBalanceAPI
balanceAPI = Proxy

data Options = Options  { optVerbose   :: Bool,
                          optAccountId :: Int64,
                          optAccount   :: String,
                          optAddress   :: String,
                          optSSL       :: Bool,
                          optUser      :: String,
                          optPassword  :: String,
                          optAction    :: Action,
                          optPort      :: Int
                        } deriving Show

data Action = Help | ImportTransactions | ImportBalance deriving (Eq, Show)

startOptions :: Options
startOptions = Options  { optVerbose = False,
                          optAccountId = -1,
                          optAccount = "",
                          optAction = Help,
                          optAddress = "localhost",
                          optUser = "",
                          optPassword = "",
                          optPort = 443,
                          optSSL = True
                        }

options :: [ OptDescr (Options -> Options) ]
options =
    [ Option "" ["import"] (NoArg (\opt -> opt {optAction = ImportTransactions }))
        "Add expenses from stdin, in the form\n\
        \'yyyy-mm-dd\\n[TEXT]\\n[AMOUNT]'. Additional expenses\n\
        \may be appended using '----------\\n' as a separator."
    , Option "" ["balance"] (NoArg (\opt -> opt {optAction = ImportBalance }))
        "create a balance (with the current date). The amount is read from stdin."
    , Option "" ["accountid"] (ReqArg (\s opt -> opt {optAccountId = read s}) "[id]")
        "Account id for imported expenses (has precedence over --account)"
    , Option "" ["account"] (ReqArg (\s opt -> opt {optAccount = s}) "[name]")
        "Account name (if --acountid not given)"
    , Option "v" ["verbose"] (NoArg (\opt -> opt { optVerbose = True }))
        "Enable verbose messages"
    , Option "h" ["help"] (NoArg (\opt -> opt {optAction = Help}))
        "Show this help message and exit"
    , Option "" ["address"] (ReqArg (\s opt -> opt {optAddress = s}) "[address]")
        "Adress (without protocol) of api, defaults to 'localhost'"
    , Option "" ["user"] (ReqArg (\s opt -> opt {optUser = s}) "[user]")
        "Username for api basic auth, defaults to ''"
    , Option "" ["password"] (ReqArg (\s opt -> opt {optPassword = s}) "[pw]")
        "Password for api basic auth, defaults to ''"
    , Option "" ["insecure"] (NoArg (\opt -> opt {optSSL = False}))
        "Use insecure http instead of https request (not recommended)"
    , Option "" ["port"] (ReqArg (\s opt -> opt {optPort = read s}) "[port]")
        "Port of api, defaults to 443"
    ]

showHelp :: String -> String
showHelp prg  = usageInfo (concat ["Usage: ", prg, " [options]"]) options

data ImportedExpense = ImportedExpense {iexpValueDate :: Day, iexpBookingDate :: Maybe Day, iexpAmount :: Decimal, iexpText :: String, iexpPreliminary :: Bool}

instance FromJSON ImportedExpense where
  parseJSON = withObject "expense" $ \o ->
    ImportedExpense <$> o .: "valueDate" <*> o .: "bookingDate" <*> o .: "amount" <*> o .: "text" <*> o .: "preliminary"

instance ToJSON ImportedExpense where
  toJSON e = object [
    "valueDate" .= iexpValueDate e,
    "bookingDate" .= iexpBookingDate e,
    "amount"  .= iexpAmount e,
    "text"  .= iexpText e,
    "preliminary"  .= iexpPreliminary e ]

dayToUTCTime :: Day -> IO UTCTime
dayToUTCTime day = do
  nowTZ <- getCurrentTimeZone
  (LocalTime today now) <- utcToLocalTime nowTZ <$> getCurrentTime
  let tod = if today == day then now else midday

  tz <- getTimeZone . UTCTime day $ 0
  return . localTimeToUTC tz $ LocalTime day tod

processImportedExpense :: Int64 -> String -> ImportedExpense -> IO ExpenseDelivery
processImportedExpense aid host e = do
  vd <- dayToUTCTime $ iexpValueDate e
  bd <- case iexpBookingDate e of
          Just x  -> Just <$> dayToUTCTime x
          Nothing -> return Nothing

  return $ ExpenseDelivery vd bd (roundTo 2 $ iexpAmount e) aid (iexpText e) ("moneydb-client on " <> host) (iexpPreliminary e)

main :: IO ()
main =  do
  args <- getArgs
  prg <- getProgName

  -- Parse options, getting a list of option actions
  let (actions, _, errors) = getOpt RequireOrder options args

  when (errors /= []) $
    hPutStrLn stderr (concat errors <> "\n" <> showHelp prg) >> exitWith (ExitFailure 1)

  -- Here we thread startOptions through all supplied option actions
  let opts = foldl (\o f -> f o) startOptions actions
  let verbose = optVerbose opts

  when verbose $ print opts

  when (optAction opts == Help) $ do
     putStrLn $ showHelp prg
     exitSuccess

  host <- getHostName

  let auth = BasicAuthData (BS8.pack $ optUser opts) (BS8.pack $ optPassword opts)
  man <- newManager (if optSSL opts then tlsManagerSettings else defaultManagerSettings)
  let runR f = runClientM f (ClientEnv man (BaseUrl (if optSSL opts then Https else Http) (optAddress opts) (optPort opts) "") Nothing)

  aid <- if optAccountId opts >= 0 then return (optAccountId opts)
    else do
     let (accountListClient :<|> _) = client accountAPI auth
     res <- runR (accountListClient Nothing Nothing)

     case res of
       Left s -> error $ "Server error: " <> show s
       Right xs -> case filter ((==) (optAccount opts) . accountTitle . recordData) xs of
         []           -> error "No account found with given title"
         [Record k _] -> do
           putStrLn $ "Asked server for accountId, id=" ++ show k
           return k
         _            -> error "Multiple accounts found with given title"

  case optAction opts of
    ImportTransactions -> do
      input <- eitherDecode <$> BSL8.getContents
      let (deliverClient :<|> _) = client automationAPI auth

      case input of
        Left s -> putStrLn $ "Parser error: " <> s
        Right input' -> do
          exps <- mapM (processImportedExpense aid host) input'

          putStrLn $ concat ["Parsed ", show . length $ exps, " Expenses"]
          when verbose $ print exps

          res <- runR (deliverClient exps)
          case res of
            Left s -> putStrLn $ "Server error: " <> show s
            Right Nothing -> putStrLn "Server error (maybe no rule matched?)"
            Right (Just ids) -> do
              putStrLn $ concat ["Server added  ", show $ length ids, " of them"]
              when verbose $ print ids
    ImportBalance -> do
      input <- eitherDecode <$> BSL8.getContents
      let (_ :<|> _ :<|> _ :<|> _ :<|> balanceInsertClient) = client balanceAPI auth

      case input of
        Left s -> putStrLn $ "Parser error: " <> s
        Right input' -> do

          now <- getCurrentTime
          let bal = Balance (toSqlKey aid) now input'
          when verbose $ print bal

          res <- runR (balanceInsertClient bal)
          case res of
            Left s    -> putStrLn $ "Server error: " <> show s
            Right id' -> when verbose $ putStrLn ("Created balance with id " ++ show id')
    Help -> return () -- already handled
