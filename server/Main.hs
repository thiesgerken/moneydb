{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

-- import           Control.Concurrent.MVar    (tryTakeMVar)
import           Control.Concurrent         (forkIO, myThreadId, threadDelay, throwTo)
import           Control.Monad              (unless, void, when)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.IO.Unlift    (MonadUnliftIO, withRunInIO)
import           Control.Monad.Logger
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Maybe  (MaybeT, runMaybeT)
import           Data.Aeson                 (encode)
import qualified Data.ByteString.Char8      as B8
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Int                   (Int64)
import           Data.List                  (transpose)
import           Data.Maybe                 (isNothing)
import qualified Data.Text                  as T
import           Database.Persist           (Entity (..), Filter, get, getBy, getEntity, selectList, (==.))
import           Database.Persist.Sql       (fromSqlKey, toSqlKey)
import           Network.Wai.Handler.Warp   (HostPreference)
import           System.Console.GetOpt
import           System.Environment         (getArgs, getProgName)
import           System.Exit                (ExitCode (..), exitSuccess, exitWith)
import           System.IO
import           System.Posix.Signals       (Handler (..), installHandler, keyboardSignal, softwareTermination)
import           Text.PrettyPrint.Boxes     (hsep, left, printBox, text, vcat)

import           Moneydb.Api                (apiJS, apiSwagger, runApi)
import           Moneydb.Automation         (checkPreliminaryExpenses)
import           Moneydb.Config
import           Moneydb.Database
import           Moneydb.Demo               (insertDemoData)
import           Moneydb.Notification       (notify)
import           Moneydb.Periodic           (checkPeriodicExpenses)
import           Moneydb.Transfer           (exportJSON, importJSON)
import           Moneydb.Types
import           Moneydb.Util               (hashPassword')

data Options = Options  { optConfigPath :: String,
                          optPort       :: Int,
                          optVerbose    :: Bool,
                          optHost       :: HostPreference,
                          optAction     :: Action,
                          optPingData   :: [Int64]
                        }

data Action = Help | Serve | Export | Import | Spec | JS | Demo | ListUsers | AddUser String | RemoveUser String | ChangePassword String | ChangeGroup String | AddGroup | RemoveGroup Int | ModifyGroup Int | ListGroups | Clear | SwitchAdmin String | PingDevice Int64 deriving (Show, Eq)

startOptions :: Options
startOptions = Options  { optConfigPath = "/etc/moneydb/moneydb.cfg",
                          optPort = 8282,
                          optHost = "127.0.0.1",
                          optAction = Serve,
                          optVerbose = False,
                          optPingData = []
                        }

options :: [ OptDescr (Options -> Options) ]
options =
    [ Option "h" ["help"] (NoArg (\opt -> opt {optAction = Help}))
        "Show this help"
    , Option "" ["spec"] (NoArg (\opt -> opt {optAction = Spec}))
                "Generate swagger spec (output is stdout) and exit"
    , Option "" ["js"] (NoArg (\opt -> opt {optAction = JS}))
                "Generate javascript client (output is stdout) and exit"
    , Option "v" ["verbose"] (NoArg (\opt -> opt {optVerbose = True}))
      "More verbose output"
    , Option "p" ["port"] (ReqArg (\s opt -> opt {optPort = read s}) "[port]")
        "Port on which to serve api, defaults to 8282"
    , Option "" ["demo"] (NoArg (\opt -> opt {optAction = Demo}))
        "Populate database with demo entries and exit"
    , Option "" ["userlist"] (NoArg (\opt -> opt {optAction = ListUsers}))
        "list users and exit"
    , Option "" ["useradd"] (ReqArg (\s opt -> opt {optAction = AddUser s}) "[username]")
        "Add a user to the database and exit"
    , Option "" ["userdel"] (ReqArg (\s opt -> opt {optAction = RemoveUser s}) "[username]")
        "Remove the given user and exit\n(Only possible if this user is not owner of anything)"
    , Option "" ["userpw"] (ReqArg (\s opt -> opt {optAction = ChangePassword s}) "[username]")
        "Change the password of the given user and exit"
    , Option "" ["usergroup"] (ReqArg (\s opt -> opt {optAction = ChangeGroup s}) "[username]")
        "Change the group of the given user and exit"
    , Option "" ["useradmin"] (ReqArg (\s opt -> opt {optAction = SwitchAdmin s}) "[username]")
        "Switch privileges of the given user and exit"
    , Option "" ["grouplist"] (NoArg (\opt -> opt {optAction = ListGroups}))
        "list groups and exit"
    , Option "" ["ping"] (ReqArg (\i opt -> opt {optAction = PingDevice (read i)}) "[device]")
        "ping the device with the given id (use --data to set an optional payload)"
    , Option "" ["data"] (ReqArg (\x opt -> opt {optPingData = read x}) "[expenses]")
        "set an optional payload (list of expense ids) for --ping"
    , Option "" ["groupadd"] (NoArg (\opt -> opt {optAction = AddGroup}))
        "Add a group to the database and exit"
    , Option "" ["groupdel"] (ReqArg (\s opt -> opt {optAction = RemoveGroup (read s)}) "[groupid]")
        "Remove a group given by id and exit\n(Only possible if this group does not have members)"
    , Option "" ["groupmod"] (ReqArg (\s opt -> opt {optAction = ModifyGroup (read s)}) "[groupid]")
        "Change a groups name"
    , Option "" ["serve"] (NoArg (\opt -> opt {optAction = Serve}))
        "Start a http server with the api\nand its documentation (the default behaviour)"
    , Option "" ["clear"] (NoArg (\opt -> opt {optAction = Clear}))
        "Delete all expenses, balances, categories and accounts from database"
    , Option "" ["export"] (NoArg (\opt -> opt {optAction = Export}))
        "dump all database contents to stdout"
    , Option "" ["import"] (NoArg (\opt -> opt {optAction = Import}))
        "clear database and import data from stdin"
    , Option "c" ["cfg"] (ReqArg (\s opt -> opt {optConfigPath = s}) "[path]")
        "path to config file,\ndefaults to /etc/moneydb/money.cfg"
    , Option "" ["listen"] (ReqArg (\s opt -> opt {optHost = read s}) "[host]")
        "address to bind to, defaults to 127.0.0.1. Change to HostAny to allow public access"
    ]

showHelp :: String -> String
showHelp prg  = usageInfo (concat ["Usage: ", prg, " [options]"]) options

failMaybe :: MonadIO m => String -> MaybeT m a
failMaybe s = liftIO (putStrLn s) >> fail s

getUserMaybe :: MonadUnliftIO m => MoneyDBConn -> String -> MaybeT m (Entity User)
getUserMaybe mc s = do
    u <- lift . runTransaction mc $ getBy (UserNameKey s)
    case u of
      Just x  -> return x
      Nothing -> failMaybe $ concat ["Error: user '", s, "' does not exist!"]

getGroupMaybe :: MonadUnliftIO m => MoneyDBConn -> Int -> MaybeT m (Entity Group)
getGroupMaybe mc i = do
    u <- lift . runTransaction mc $ get (toSqlKey . fromIntegral $ i)
    case u of
      Just x  -> return (Entity (toSqlKey . fromIntegral $ i) x)
      Nothing -> failMaybe $ concat ["Error: group '", show i, "' does not exist!"]

askHashMaybe :: MonadIO m => String -> MaybeT m String
askHashMaybe s = do
    liftIO $ putStrLn s
    pw <- liftIO getLine
    when (length pw < 6) $ failMaybe "Error: password must have at least 6 characters!"
    liftIO $ hashPassword' pw

checkPeriodicTasks :: (MonadUnliftIO m, MonadLogger m) => MoneyDBConn -> m ()
checkPeriodicTasks mc = f (0 :: Int)
  where f n = do
            liftIO $ threadDelay 1000000

            -- TODO: disabled as workaround
            -- msg <- liftIO . tryTakeMVar $ moneyMessage mc
            -- case msg of
            --   Just RefreshStocks -> do
            --     logInfoN "Refreshing stock infos due to inter-thread message"
            --     runTransaction mc checkStocks
            --   Nothing -> return ()

            if (n <= 0) then do
              -- TODO: disabled as workaround
              -- runTransaction mc checkStocks
              checkPeriodicExpenses mc
              checkPreliminaryExpenses mc
              f 60
            else f (n-1)

  -- logDebugN "Destroying database connection pool to force database commit"
  -- commit mc -- committing WAL to file every 10 minutes seems like a good idea

logger :: LogLevel -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
logger minlvl loc src lvl msg
  | lvl == LevelError = logger' stderr loc src lvl msg
  | lvl >= minlvl = logger' stdout loc src lvl msg
  | otherwise = return ()

logger' :: Handle -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
logger' h loc src level msg = B8.hPutStr h (fromLogStr $ defaultLogStr loc src level msg)

-- | Run the server
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  args <- getArgs
  prg <- getProgName

  -- Parse options, getting a list of option actions
  let (actions, _, errors) = getOpt RequireOrder options args

  when (errors /= []) $
    hPutStrLn stderr (concat [concat errors, "\n", showHelp prg]) >> exitWith (ExitFailure 1)

  -- thread startOptions through all supplied options
  let opts = foldl (\o f -> f o) startOptions actions

  case optAction opts of
    Help -> putStrLn (showHelp prg) >> exitSuccess
    Spec -> BL8.putStrLn (encode apiSwagger) >> exitSuccess
    JS   -> BL8.putStrLn apiJS >> exitSuccess
    _    -> return ()

  cfg <- loadConfig (optConfigPath opts)
  let minlvl = if optVerbose opts then LevelDebug else LevelInfo
  let logger'' = logger minlvl defaultLoc "" LevelInfo

  (`runLoggingT` logger minlvl) $ do
      mc <- connect cfg (logger minlvl)

      tid <- liftIO myThreadId
      liftIO $ mapM_ (\(x, l) -> installHandler x (Catch (logger'' l >> commit mc >> throwTo tid ExitSuccess)) Nothing) [(softwareTermination, "Received SIGTERM"), (keyboardSignal, "Received Ctrl+C")]

      case optAction opts of
        Serve -> do
          logInfoN (T.pack $ show cfg)
          logInfoN $ "Serving on " <> (T.pack . show . optHost $ opts) <> " at port " <> (T.pack . show . optPort $ opts)
          _ <- withRunInIO $ \runInIO -> forkIO (runInIO (checkPeriodicTasks mc))
          liftIO $ runApi mc (optHost opts) (optPort opts)
        Export ->
          exportJSON mc >>= liftIO . BL8.putStrLn
        Import -> do
          json <- liftIO $ BL8.hGetContents stdin
          importJSON mc json
        PingDevice i -> do
          dev' <- lift . runTransaction mc $ getEntity (toSqlKey i)
          case dev' of
            Just dev -> do
              let token = firebaseToken $ moneyConfig mc
              void $ notify token dev (optPingData opts)
            Nothing -> logErrorN "device not found!"
        ListUsers -> do
          users <- fmap (\u -> [show . entityKey $ u, userName (entityVal u), userFullName (entityVal u), show $ userIsAdmin (entityVal u)]) <$> runTransaction mc (selectList ([] :: [Filter User]) [])
          liftIO $ printBox $ hsep 2 left (fmap (vcat left . fmap text) (transpose $ ["id", "name", "full name", "admin"] : users))
        AddUser s -> void . runMaybeT $ do
          when (null s) $ failMaybe "Error: user name must not be empty!"

          u' <- liftIO $ runTransaction mc (getBy (UserNameKey s))
          unless (isNothing u') $ failMaybe "Error: username exists"

          liftIO $ putStrLn "Enter the full name of the new user"
          fname <- liftIO getLine
          when (null fname) $ failMaybe "Error: full name must not be empty!"

          hash <- askHashMaybe "Enter the password (>=6 chars) of the new user"

          u <- lift $ runTransaction mc (insertElement (User s hash fname Nothing False))
          liftIO . putStrLn $ concat ["Added user with id ", show u, " to the database"]
        RemoveUser s -> void . runMaybeT $ do
          u <- getUserMaybe mc s
          liftIO . putStrLn $ concat ["If you really want to remove user '", show s, "' type 'rm", userName $ entityVal u, "'"]
          conf <- liftIO getLine
          unless (conf == "rm" <> userName (entityVal u)) $ failMaybe "Aborting."

          res <- liftIO $ runTransaction mc (deleteElement (entityKey u))
          if res then
            liftIO . putStrLn $ concat ["Removed user ", show (entityKey u), " from the database"]
          else
            failMaybe "Error: The user still has items belonging to him!"
        SwitchAdmin s -> void . runMaybeT $ do
          u <- getUserMaybe mc s

          liftIO $ runTransaction mc (replaceElement (entityKey u) ((entityVal u) {userIsAdmin = not $ userIsAdmin (entityVal u)}))
          liftIO . putStrLn . concat $ ["User ",  (show . userName . entityVal) u, if userIsAdmin (entityVal u) then " is not an admin anymore" else " is now an admin"]
        ChangePassword s -> void . runMaybeT $ do
          u <- getUserMaybe mc s
          hash <- askHashMaybe "Enter the new password (>=6 chars)"

          liftIO $ runTransaction mc (replaceElement (entityKey u) ((entityVal u) {userPasswordHash = hash}))
          liftIO . putStrLn $ "Modified user " <> (show . userName . entityVal) u
        ChangeGroup s -> void . runMaybeT $ do
          u <- getUserMaybe mc s
          grps <- lift $ runTransaction mc (selectList ([] :: [Filter Group]) [])

          liftIO $ do
             putStrLn "Available Groups:"
             mapM_ (\g -> putStrLn $ concat ["id = ", show . entityKey $ g, ": name = '", groupName (entityVal g) , "'"]) grps
             putStrLn "Please enter one of these ids or '-1' to assign no group to the user."

          i <- read <$> liftIO getLine
          unless (i < 0 || i `elem` fmap (fromSqlKey . entityKey) grps) $
            failMaybe "No such group id!"

          lift $ runTransaction mc (replaceElement (entityKey u) ((entityVal u) {userGroupId = if i<0 then Nothing else Just (toSqlKey i)}))
          liftIO . putStrLn $ "Modified user " <> (show . userName . entityVal) u
        ListGroups -> do
          grps <- runTransaction mc (selectList ([] :: [Filter Group]) [])
          members <- fmap (show . fmap (userName . entityVal)) <$> mapM (\k -> runTransaction mc (selectList [UserGroupId ==. Just (entityKey k)] [])) grps
          let lst = zipWith (\a b -> [show (entityKey a), groupName $ entityVal a, b]) grps members

          liftIO $ printBox $ hsep 2 left (fmap (vcat left . fmap text) (transpose $ ["id", "name", "members"] : lst))
        AddGroup -> void . runMaybeT $ do
          liftIO $ putStrLn "Enter the name of the new group"
          gname <- liftIO getLine
          when (null gname) $ failMaybe "Error: name must not be empty!"

          u <- liftIO $ runTransaction mc (insertElement (Group gname))
          liftIO . putStrLn $ ("Added group to the database, id=" ++ show u)
        RemoveGroup i -> void . runMaybeT $ do
          g <- getGroupMaybe mc i
          liftIO . putStrLn $ concat ["If you really want to remove group '", show i, "' type 'rm", groupName $ entityVal g, "'"]
          conf <- liftIO getLine
          unless (conf == "rm" <> groupName (entityVal g)) $ failMaybe "Aborting."

          res <- lift $ runTransaction mc (deleteElement (entityKey g))
          if res then
            liftIO . putStrLn $ concat ["Removed group ", show (entityKey g), " from the database"]
          else
            failMaybe "Error: The group still has members!"
        ModifyGroup i -> void . runMaybeT $ do
          g <- getGroupMaybe mc i

          liftIO $ putStrLn "Enter the new name of the group"
          gname <- liftIO getLine
          when (null gname) $ failMaybe "Error: name must not be empty!"

          lift $ runTransaction mc (replaceElement (entityKey g) ((entityVal g) {groupName = gname}))
          liftIO . putStrLn $ "Modified group " <> show (entityKey g)
        Demo -> insertDemoData mc >> liftIO (putStrLn "Added demo data to database")
        Clear -> void . runMaybeT $ do
          liftIO $ putStrLn "If you really want to clear everything but users and groups type 'cleareverything'"
          conf <- liftIO getLine
          unless (conf == "cleareverything") $ failMaybe "Aborting"
          lift $ runTransaction mc clear
        _ -> return ()

      commit mc
