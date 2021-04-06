{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Moneydb.Api.Element(ElementAPIs, ElementAPI, elementHandlers) where

import           Control.Concurrent.MVar         (tryPutMVar)
import           Control.Monad                   (join, void, when)
import           Control.Monad.Except            (unless)
import           Control.Monad.IO.Class          (MonadIO (..))
import           Control.Monad.Trans.Reader      (ReaderT)
import           Control.Monad.Trans.Resource    (ResourceT)
import           Data.Int                        (Int64)
import           Data.Maybe                      (fromJust, isNothing, maybeToList)
import           Data.Time.Clock                 (getCurrentTime)
import           Database.Esqueleto              hiding (isNothing)
import qualified Database.Persist                as P
import           Servant.API
import           Servant.Server

import           Moneydb.Api.Util
import           Moneydb.Database
import           Moneydb.Notification
import           Moneydb.Types
import qualified Moneydb.Types.Rendering.Expense as RE

type ElementAPI a = QueryParam "offset" Int64 :> QueryParam "count" Int64 :> Get '[JSON] [Record a]
            :<|> Capture "id" Int64 :> Get '[JSON] a
            :<|> Capture "id" Int64 :> DeleteNoContent '[JSON] NoContent
            :<|> Capture "id" Int64 :> ReqBody '[JSON] a :> PutNoContent '[JSON] NoContent
            :<|> ReqBody '[JSON] a :> Post '[JSON] Int64

type RenderedUserAPI = QueryParam "offset" Int64 :> QueryParam "count" Int64 :> Get '[JSON] [Record RenderedUser]
            :<|> "me" :> Get '[JSON] (Record RenderedUser)
            :<|> Capture "id" Int64 :> Get '[JSON] RenderedUser

type RenderedStockAPI = QueryParam "offset" Int64 :> QueryParam "count" Int64 :> Get '[JSON] [Record RenderedStock]
            :<|> Capture "id" Int64 :> Get '[JSON] RenderedStock

renderedExpenseHandler :: MoneyDBConn -> Entity User -> Server (ElementAPI RenderedExpense)
renderedExpenseHandler mc u = listHandler :<|> getHandler :<|> deleteHandler :<|> replaceHandler :<|> insertHandler
  where listHandler off cnt = liftIO . runTransaction mc $ do
          es <- listReadableElements u off cnt
          rexps <- getRenderedExpenses (entityKey u) es
          return $ zipWith (\ent x -> Record (fromSqlKey $ entityKey ent) x) es rexps
        getHandler i = join . liftIO . runTransaction mc $ do
                          e <- get (toSqlKey i :: Key Expense)
                          case e of
                            Just e' -> do
                              x <- getRenderedExpense (entityKey u) (Entity (toSqlKey i) e')
                              case x of
                                Just e'' -> return $ return e''
                                Nothing  -> return $ throw401 i
                            Nothing -> return $ throw404 i
        deleteHandler i = do
          _ <- getHandler i -- permission check
          let eid = toSqlKey i

          liftIO . runTransaction mc $ do
            deleteWhere [ExpenseSharingExpenseId P.==. eid]
            deleteWhere [ExpenseFlagExpenseId P.==. eid]

          e <- liftIO . runTransaction mc $ deleteElement (toSqlKey i :: Key Expense)
          unless e $ throw400 "Cannot delete from database, other items depend on this one!"

          return NoContent
        replaceHandler i x = do
          _ <- getHandler i -- permission check
          let eid = toSqlKey i

          r <- liftIO . runTransaction mc $ validateRenderedExpense u x
          case r of
            Nothing -> throw401' "No permission for category or account(s)!"
            Just x' -> liftIO . runTransaction mc $ do
              deleteWhere [ExpenseSharingExpenseId P.==. eid]
              deleteWhere [ExpenseFlagExpenseId P.==. eid]

              let x'' = unrenderExpense (entityKey u) x'
              old <- fromJust <$> P.get eid
              t <- liftIO getCurrentTime

              replaceElement eid (x'' {expenseCreationDate = expenseCreationDate old, expenseLastModifiedBy = entityKey u, expenseLastModified = t})
              mapM_ insertElement (unrenderSharing eid <$> RE.sharing x')
              mapM_ insertElement (ExpenseFlag eid <$> RE.flags x')
              checkNotifications mc [eid]

          return NoContent
        insertHandler x = do
          r <- liftIO . runTransaction mc $ validateRenderedExpense u x
          case r of
            Nothing -> throw401' "No permission for category or account(s)!"
            Just y -> liftIO . runTransaction mc $ do
                let x'' = unrenderExpense (entityKey u) y
                t <- liftIO getCurrentTime
                eid <- insertElement (x'' {expenseCreationDate = t, expenseLastModifiedBy = entityKey u, expenseLastModified = t})
                mapM_ insertElement (unrenderSharing eid <$> RE.sharing y)
                mapM_ insertElement (ExpenseFlag eid <$> RE.flags y)

                checkNotifications mc [eid]
                return $ fromSqlKey eid

renderedCategoryHandler :: MoneyDBConn -> Entity User -> Server (ElementAPI RenderedCategory)
renderedCategoryHandler mc u = listHandler :<|> getHandler :<|> deleteHandler :<|> replaceHandler :<|> insertHandler
  where listHandler off cnt = liftIO . runTransaction mc $ do
          xs <- listReadableElements u off cnt
          rs <- mapM (getRenderedCategory (entityKey u)) xs
          return $ zipWith (\ent x -> Record (fromSqlKey $ entityKey ent) x) xs rs
        getHandler i = join . liftIO . runTransaction mc $ do
                          e <- get (toSqlKey i :: Key Category)
                          case e of
                            Just e' -> do
                              x <- getRenderedCategory (entityKey u) (Entity (toSqlKey i) e')
                              return $ return x
                            Nothing -> return $ throw404 i
        deleteHandler i = do
          _ <- getHandler i -- permission check
          let cid = toSqlKey i

          liftIO . runTransaction mc $ do
            deleteWhere [CategoryReplacementOriginal P.==. cid]
            deleteWhere [CategoryReplacementReplacement P.==. cid]

          e <- liftIO . runTransaction mc $ deleteElement (toSqlKey i :: Key Category)
          unless e $ throw400 "Cannot delete from database, other items depend on this one!"

          return NoContent
        replaceHandler i x = do
          _ <- getHandler i -- permission check
          let cid = toSqlKey i
          let (c,cxs') = unrenderCategory (entityKey u) x
          let cxs = fmap (\a -> CategoryReplacement {categoryReplacementReplacement = cid, categoryReplacementOwnerId = entityKey u, categoryReplacementOriginal = a}) cxs'

          rcxs <- liftIO . runTransaction mc $ mapM (validateElement u) cxs
          r <- liftIO . runTransaction mc $ validateElement u c

          when (any isNothing rcxs || isNothing r) $
            throw401' "No permission on categories to be replaced!"

          liftIO . runTransaction mc $ do
              deleteWhere [CategoryReplacementReplacement P.==. cid]

              replaceElement cid (fromJust r)
              mapM_ (insertElement . fromJust) rcxs

          return NoContent
        insertHandler x = do
          let (c,cxs') = unrenderCategory (entityKey u) x

          r <- liftIO . runTransaction mc $ validateElement u c
          join $ case r of
            Nothing -> throw401' "No permission!"
            Just x' -> liftIO . runTransaction mc $ do
              cid <- insertElement x'

              let cxs = fmap (\a -> CategoryReplacement {categoryReplacementReplacement = cid, categoryReplacementOwnerId = entityKey u, categoryReplacementOriginal = a}) cxs'

              rcxs <- mapM (validateElement u) cxs

              if any isNothing rcxs then do
                  P.delete cid
                  return $ throw401' "No permission!"
              else do
                  mapM_ (insertElement . fromJust) rcxs
                  return $ return (fromSqlKey cid)

renderedDeviceHandler :: MoneyDBConn -> Entity User -> Server (ElementAPI RenderedDevice)
renderedDeviceHandler mc u = listHandler :<|> getHandler :<|> deleteHandler :<|> replaceHandler :<|> insertHandler
  where listHandler off cnt = liftIO . runTransaction mc $ do
          xs <- listReadableElements u off cnt
          rs <- mapM getRenderedDevice xs
          return $ zipWith (\ent x -> Record (fromSqlKey $ entityKey ent) x) xs rs
        getHandler i = join . liftIO . runTransaction mc $ do
                          e <- get (toSqlKey i :: Key Device)
                          case e of
                            Just e' -> do
                              x <- getRenderedDevice (Entity (toSqlKey i) e')
                              return $ return x
                            Nothing -> return $ throw404 i
        deleteHandler i = do
          _ <- getHandler i -- permission check
          let cid = toSqlKey i

          liftIO . runTransaction mc $ deleteWhere [ExpenseFilterDeviceId P.==. cid]

          e <- liftIO . runTransaction mc $ deleteElement (toSqlKey i :: Key Device)
          unless e $ throw400 "Cannot delete from database, other items depend on this one!"

          return NoContent
        replaceHandler i x = do
          old <- getHandler i -- permission check
          let (cOld, _) = unrenderDevice (entityKey u) old
          let cid = toSqlKey i
          let (c,cxs') = unrenderDevice (entityKey u) x
          let cxs = fmap (\a -> a {expenseFilterDeviceId = cid}) cxs'

          rcxs <- liftIO . runTransaction mc $ mapM (validateElement u) cxs
          r <- liftIO . runTransaction mc $ validateElement u c

          when (any isNothing rcxs || isNothing r) $
            throw401' "No permission!"

          liftIO . runTransaction mc $ do
              deleteWhere [ExpenseFilterDeviceId P.==. cid]
              t <- liftIO getCurrentTime

              replaceElement cid $ (fromJust r) {deviceLastContact = t, deviceFirstContact = deviceFirstContact cOld, deviceNotificationCount = deviceNotificationCount cOld, deviceLastNotification = deviceLastNotification cOld}

          -- weird errors if this happens in the same sql transaction as the above code (due to uniqueness constraints)
          liftIO . runTransaction mc $
              mapM_ (insertElement . fromJust) rcxs

          return NoContent
        insertHandler x = do
          let (c,cxs') = unrenderDevice (entityKey u) x

          y <- liftIO . runTransaction mc $ getBy . DeviceTokenKey $ deviceToken c
          case y of
            Just y' -> do
              -- only for devices: repeated insertion -> updating
              _ <- replaceHandler (fromSqlKey $ entityKey y') x
              return . fromSqlKey $ entityKey y'
            Nothing -> do
              r <- liftIO . runTransaction mc $ validateElement u c
              join $ case r of
                Nothing -> throw401' "No permission!"
                Just x' -> liftIO . runTransaction mc $ do
                  t <- liftIO getCurrentTime
                  cid <- insertElement $ x' {deviceLastContact = t, deviceFirstContact = t, deviceNotificationCount = 0, deviceLastNotification = Nothing}

                  let cxs = fmap (\a -> a {expenseFilterDeviceId = cid}) cxs'
                  rcxs <- mapM (validateElement u) cxs

                  if any isNothing rcxs then do
                      P.delete cid
                      return $ throw401' "No permission!"
                  else do
                      mapM_ (insertElement . fromJust) rcxs
                      return $ return (fromSqlKey cid)

renderedAccountHandler :: MoneyDBConn -> Entity User -> Server (ElementAPI RenderedAccount)
renderedAccountHandler mc u = listHandler :<|> getHandler :<|> deleteHandler :<|> replaceHandler :<|> insertHandler
  where listHandler off cnt = liftIO . runTransaction mc $ do
          xs <- listReadableElements u off cnt
          rs <- mapM getRenderedAccount xs
          return $ zipWith (\ent x -> Record (fromSqlKey $ entityKey ent) x) xs rs
        getHandler i = join . liftIO . runTransaction mc $ do
                          e <- get (toSqlKey i :: Key Account)
                          case e of
                            Just e' -> do
                              x <- getRenderedAccount (Entity (toSqlKey i) e')
                              return $ return x
                            Nothing -> return $ throw404 i
        deleteHandler i = do
          _ <- getHandler i -- permission check
          let cid = toSqlKey i

          liftIO . runTransaction mc $ do
            deleteWhere [AccountSyncingAccount1 P.==. cid]
            deleteWhere [AccountSyncingAccount2 P.==. cid]

          e <- liftIO . runTransaction mc $ deleteElement (toSqlKey i :: Key Account)
          unless e $ throw400 "Cannot delete from database, other items depend on this one!"

          return NoContent
        replaceHandler i x = do
          _ <- getHandler i -- permission check
          let aid = toSqlKey i
          let (a,_) = unrenderAccount (entityKey u) x

          r <- liftIO . runTransaction mc $ validateElement u a
          when (isNothing r) $ throw401' "No permission!"

          liftIO . runTransaction mc $ replaceElement aid (fromJust r)
          return NoContent
        insertHandler x = do
          let (c,_) = unrenderAccount (entityKey u) x

          r <- liftIO . runTransaction mc $ validateElement u c
          case r of
            Nothing -> throw401' "No permission!"
            Just x' -> liftIO . runTransaction mc $ do
              cid <- insertElement x'
              return (fromSqlKey cid)

renderedUserHandler :: MoneyDBConn -> Entity User -> Server RenderedUserAPI
renderedUserHandler mc u = listHandler :<|> meHandler :<|> getHandler
  where listHandler off cnt = do
          ents <- liftIO . runTransaction mc $ listReadableElements u off cnt
          return $ (\x -> Record (fromSqlKey $ entityKey x) (convertUser $ entityVal x)) <$> ents
        meHandler = return $ Record (fromSqlKey $ entityKey u) (convertUser $ entityVal u)
        getHandler i = join . liftIO . runTransaction mc $ do
                          e <- get (toSqlKey i)
                          case e of
                            Just e' -> do
                                perm <- checkPermissions u (Entity (toSqlKey i) e')
                                if perm >= ReadPermissions then return . return $ convertUser e'
                                else return $ throw401 i
                            Nothing -> return $ throw404 i

renderedStockHandler :: MoneyDBConn -> Entity User -> Server RenderedStockAPI
renderedStockHandler mc u = listHandler :<|> getHandler
  where listHandler off cnt = do
          (ents,rents) <- liftIO . runTransaction mc $ do
            x <- listReadableElements u off cnt
            y <- mapM getRenderedStock x
            return (x,y)

          return $ zipWith (\x y -> Record (fromSqlKey $ entityKey x) y) ents rents
        getHandler i = join . liftIO . runTransaction mc $ do
                          e <- get (toSqlKey i)
                          case e of
                            Just e' -> do
                                perm <- checkPermissions u (Entity (toSqlKey i) e')
                                if perm >= ReadPermissions then do
                                            z <- getRenderedStock (Entity (toSqlKey i) e')
                                            return $ return z
                                else return $ throw401 i
                            Nothing -> return $ throw404 i

type ElementAPIs =
         ("users" :> RenderedUserAPI)
    :<|> ("expenses" :> "rendered" :> ElementAPI RenderedExpense)
    :<|> ("expenses" :> "flags" :> ElementAPI ExpenseFlag)
    :<|> ("expenses" :> "sharings" :> ElementAPI ExpenseSharing)
    :<|> ("expenses" :> "filters" :> ElementAPI ExpenseFilter)
    :<|> ("expenses" :> "seen" :> ElementAPI SeenExpense)
    :<|> ("expenses" :> ElementAPI Expense)
    :<|> ("accounts" :> ElementAPI Account)
    :<|> ("accounts" :> "rendered" :> ElementAPI RenderedAccount)
    :<|> ("rules" :> "automation" :> ElementAPI AutomationRule)
    :<|> ("rules" :> "periodic" :> ElementAPI PeriodicRule)
    :<|> ("balances" :> ElementAPI Balance)
    :<|> ("categories" :> ElementAPI Category)
    :<|> ("categories" :> "rendered" :> ElementAPI RenderedCategory)
    :<|> ("categories" :> "replacements" :> ElementAPI CategoryReplacement)
    :<|> ("devices" :> ElementAPI Device)
    :<|> ("devices" :> "rendered" :> ElementAPI RenderedDevice)
    :<|> ("stocks" :> ElementAPI Stock)
    :<|> ("stocks" :> "rendered" :> RenderedStockAPI)
    :<|> ("stocks" :> "accounts" :> ElementAPI SecuritiesAccount)
    :<|> ("stocks" :> "transactions" :> ElementAPI StockTransaction)
    :<|> ("stocks" :> "infos" :> ElementAPI StockInfo)
    :<|> ("stocks" :> "exchanges" :> ElementAPI StockExchange)
    :<|> ("stocks" :> "prices" :> ElementAPI StockHistoricalPrice)
    :<|> ("groups" :> ElementAPI Group)

elementHandlers :: MoneyDBConn -> Entity User -> Server ElementAPIs
elementHandlers mc u = renderedUserHandler mc u :<|> renderedExpenseHandler mc u :<|> e :<|> e :<|> e :<|> e :<|> e :<|> e :<|> renderedAccountHandler mc u :<|> e :<|> e :<|> e :<|> e :<|> renderedCategoryHandler mc u :<|> e :<|> e :<|> renderedDeviceHandler mc u :<|> elementHandler' stockAction mc u :<|> renderedStockHandler mc u :<|> e :<|> e :<|> e :<|> e :<|> e :<|> e
  where e :: DBElement a => Server (ElementAPI a)
        e = elementHandler mc u
        stockAction = void $ tryPutMVar (moneyMessage mc) RefreshStocks

elementHandler :: forall a. DBElement a => MoneyDBConn -> Entity User -> Server (ElementAPI a)
elementHandler = elementHandler' mempty

-- f : something to call after delete / replace / insert operations [used for stocks]
elementHandler' :: forall a. DBElement a => IO () -> MoneyDBConn -> Entity User -> Server (ElementAPI a)
elementHandler' action mc u = listHandler :<|> getHandler :<|> deleteHandler :<|> replaceHandler :<|> insertHandler
  where listHandler off cnt =  do
          ents <- liftIO . runTransaction mc $ listReadableElements u off cnt
          return $ (\x -> Record (fromSqlKey $ entityKey x) (entityVal x)) <$> ents
        getHandler i = join . liftIO . runTransaction mc $ do
                          f <- getEntity (toSqlKey i :: Key a)
                          e <- renderElements u (maybeToList f)
                          case e of
                            [e'] -> do
                                perm <- checkPermissions u e'
                                if perm >= ReadPermissions then return . return $ entityVal e'
                                else return $ throw401 i
                            _ -> return $ throw404 i
        deleteHandler i = join . liftIO $ do
                                (z, s) <- runTransaction mc $ do
                                    e <- get (toSqlKey i :: Key a) :: ReaderT SqlBackend (ResourceT IO) (Maybe a)
                                    case e of
                                      Just e'-> do
                                          perm <- checkPermissions u (Entity (toSqlKey i) e')
                                          if perm >= ReadWritePermissions then do
                                            ok <- deleteElement (toSqlKey i :: Key a)
                                            if ok then return (return NoContent, True)
                                            else return (throw400 "Cannot delete from database, other items depend on it!", False)
                                          else return (throw401 i, False)
                                      Nothing -> return (throw404 i, False)
                                when s action -- want action to be called outside of runTransaction
                                return z
        replaceHandler i x = join . liftIO $ do
                              (z, s) <- runTransaction mc $ do
                                  e <- get (toSqlKey i) :: ReaderT SqlBackend (ResourceT IO) (Maybe a)
                                  case e of
                                    Just e' -> do
                                        perm <- checkPermissions u (Entity (toSqlKey i) e')
                                        if perm >= ReadWritePermissions then do
                                            r <- validateElement u x
                                            case r of
                                              Just x' -> do
                                                replaceElement (toSqlKey i) x'
                                                return (return NoContent, True)
                                              Nothing -> return (throw401' "No permissions on some ids!", False)
                                        else return (throw401 i, False)
                                    Nothing -> return (throw404 i, False)
                              when s action
                              return z
        insertHandler x = fmap fromSqlKey $ join . liftIO $ do
                              (z, s) <- runTransaction mc $ do
                                  r <- validateElement u x
                                  case r of
                                    Just x' -> do
                                      g <- insertElement x'
                                      return (return g, True)
                                    Nothing -> return (throw401' "No permissions on some ids!", False)
                              when s action
                              return z

