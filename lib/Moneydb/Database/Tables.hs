{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

-- | database bindings for moneydb.
--   Note that this module may abort execution on malformed queries or process them, leading to a corrupt database.
--   (example: Adding an expense with different ownerid and owner of account & account is not shared)
module Moneydb.Database.Tables (clear, DBElement(..)) where

import           Control.Monad                (unless, when)
import           Control.Monad.IO.Class       (MonadIO (..))
import           Control.Monad.IO.Unlift      (MonadUnliftIO)
import           Control.Monad.Trans.Class    (lift)
import           Control.Monad.Trans.Maybe    (MaybeT (..))
import           Control.Monad.Trans.Reader   (ReaderT)
import           Control.Monad.Trans.Resource (ResourceT)

import           Data.Maybe                   (fromMaybe, isNothing)

import           Database.Esqueleto           hiding (isNothing)
import           Database.Persist             (Filter)
import qualified Database.Persist             as P

import           Moneydb.Database.Permissions
import           Moneydb.Database.Rendering
import           Moneydb.Types

clear :: MonadIO m => ReaderT SqlBackend (ResourceT m) ()
clear = do
    deleteWhere ([] :: [Filter AutomationRule])
    deleteWhere ([] :: [Filter PeriodicRule])
    deleteWhere ([] :: [Filter ExpenseFlag])
    deleteWhere ([] :: [Filter ExpenseSharing])
    deleteWhere ([] :: [Filter Expense])

    deleteWhere ([] :: [Filter CategoryReplacement])
    deleteWhere ([] :: [Filter Category])

    deleteWhere ([] :: [Filter Balance])
    deleteWhere ([] :: [Filter AccountSyncing])
    deleteWhere ([] :: [Filter Account])

    deleteWhere ([] :: [Filter ExpenseFilter])
    deleteWhere ([] :: [Filter Device])
    deleteWhere ([] :: [Filter SeenExpense])

    deleteWhere ([] :: [Filter StockTransaction])
    deleteWhere ([] :: [Filter Stock])
    deleteWhere ([] :: [Filter SecuritiesAccount])
    deleteWhere ([] :: [Filter StockHistoricalPrice])
    deleteWhere ([] :: [Filter StockCurrentPrice])
    deleteWhere ([] :: [Filter StockExchange])
    deleteWhere ([] :: [Filter StockInfo])

    deleteWhere ([] :: [Filter User])
    deleteWhere ([] :: [Filter Group])

-- write operations need more logic than DB handling, e.g. because
-- - they could introduce inconsistencies in the database (e.g. deleting a group with members)
-- - they cause additional updates (e.g. creation of a shared expense)
-- this typeclass wraps the db ops from persistent with these checks and additional operations.
-- also on reading: might need to change the perspective on some items (balances)
class ( PersistEntity a
      , PersistRecordBackend a SqlBackend
      , ToBackendKey SqlBackend a
      ) => DBElement a where
    insertElement :: MonadUnliftIO m
                  => a
                  -> ReaderT SqlBackend (ResourceT m) (Key a)
    insertElement = P.insert

    replaceElement :: MonadUnliftIO m
                   => Key a
                   -> a
                   -> ReaderT SqlBackend (ResourceT m) ()
    replaceElement = P.replace

    deleteElement :: MonadUnliftIO m
                  => Key a
                  -> ReaderT SqlBackend (ResourceT m) Bool
    deleteElement k = P.delete k >> return True

    validateElement :: MonadUnliftIO m
                    => Entity User
                    -> a
                    -> ReaderT SqlBackend (ResourceT m) (Maybe a)

    checkPermissions :: MonadUnliftIO m
                     => Entity User
                     -> Entity a
                     -> ReaderT SqlBackend (ResourceT m) Permissions

    readable :: Entity User -> SqlExpr (Entity a) -> SqlExpr (Value Bool)

    -- "simple" rendering of items, i.e. the type does not change. used for balances. Expenses and Categories need enhanced rendering where the type does change.
    renderElements :: MonadUnliftIO m
                   => Entity User
                   -> [Entity a]
                   -> ReaderT SqlBackend (ResourceT m) [Entity a]
    renderElements _ = return

    nativeOrdering :: SqlExpr (Entity a) -> SqlExpr OrderBy

instance DBElement Group where
    deleteElement k = do
        member <- P.selectFirst [ UserGroupId P.==. Just k ] []
        if isNothing member then P.delete k >> return True else return False

    nativeOrdering x = asc (x ^. GroupId)

    readable u x
        | userIsAdmin $ entityVal u = val True
        | Just i <- userGroupId $ entityVal u = x ^. GroupId ==. val i
        | otherwise = val False

    checkPermissions u g
        | userIsAdmin $ entityVal u = return ReadWritePermissions
        | Just (entityKey g) == userGroupId (entityVal u) =
            return ReadPermissions
        | otherwise = return NoPermissions

    validateElement u x = return $
        if userIsAdmin (entityVal u) then Just x else Nothing

instance DBElement User where
    deleteElement k = do
        nexp <- isNothing <$> P.selectFirst [ ExpenseOwnerId P.==. k ] []
        nacc <- isNothing <$> P.selectFirst [ AccountOwnerId P.==. k ] []
        ncat <- isNothing <$> P.selectFirst [ CategoryOwnerId P.==. k ] []
        nper <- isNothing <$> P.selectFirst [ PeriodicRuleOwnerId P.==. k ] []
        naur
            <- isNothing <$> P.selectFirst [ AutomationRuleOwnerId P.==. k ] []
        ndev <- isNothing <$> P.selectFirst [ DeviceOwnerId P.==. k ] []
        if nexp && nacc && ncat && nper && naur && ndev
            then P.delete k >> return True
            else return False

    nativeOrdering x = asc (x ^. UserId)

    readable u x
        | userIsAdmin $ entityVal u = val True
        | Just i <- userGroupId $ entityVal u = (x ^. UserGroupId)
            ==. val (Just i)
        | otherwise = val False

    checkPermissions u x
        | userIsAdmin $ entityVal u = return ReadWritePermissions
        | Just g <- userGroupId (entityVal u), Just g
            == userGroupId (entityVal x) = return ReadPermissions
        | otherwise = return NoPermissions

    validateElement u x = return $
        if userIsAdmin (entityVal u) then Just x else Nothing

instance DBElement Expense where
    deleteElement k = do
        nauto <- isNothing
            <$> P.selectFirst [ AutomationRuleTemplateId P.==. k ] []
        nper <- isNothing
            <$> P.selectFirst [ PeriodicRuleTemplateId P.==. k ] []
        if nauto && nper
            then do
                deleteWhere [ ExpenseFlagExpenseId P.==. k ]
                deleteWhere [ ExpenseSharingExpenseId P.==. k ]

                P.delete k
                return True
            else return False

    nativeOrdering x = asc (x ^. ExpenseId)

    readable (Entity uid _) = expenseVisible uid

    checkPermissions u x = do
        e <- getRenderedExpense (entityKey u) x
        case e of
            Just _  -> return ReadWritePermissions -- only enables to delete expenses (see validateElement)
            Nothing -> return NoPermissions

    validateElement _ _ = return Nothing -- editing and inserting of expenses is done through rendered expenses. validating a single expense is hard, since one of the (rendered) expenses accounts has to at least be synced to the user. validity of an expense can therefore be determined by sharing settings that are not inserted yet.

instance DBElement Account where
    deleteElement k = do
        nexp <- isNothing <$> P.selectFirst [ ExpenseAccountId P.==. k ] []
        nbal <- isNothing <$> P.selectFirst [ BalanceAccountId P.==. k ] []
        nsxp <- isNothing <$> P.selectFirst [ SeenExpenseAccountId P.==. k ] []
        nshare <- isNothing
            <$> P.selectFirst [ ExpenseSharingAccountId P.==. k ] []
        nsync <- isNothing
            <$> P.selectFirst ([ AccountSyncingAccount1 P.==. k
                               ] P.||. [ AccountSyncingAccount2 P.==. k ])
                              []
        if nexp && nbal && nshare && nsync && nsxp
            then P.delete k >> return True
            else return False

    nativeOrdering x = asc (x ^. AccountId)

    readable u a
        | Just gid <- userGroupId (entityVal u) = exists $ from $ \u' ->
            where_ $ (a ^. AccountOwnerId ==. (u' ^. UserId))
            &&. ((u' ^. UserGroupId) ==. just (val gid))
        | otherwise = a ^. AccountOwnerId ==. val (entityKey u)

    checkPermissions u x
        | entityKey u == accountOwnerId (entityVal x) =
            return ReadWritePermissions
        | Just g <- userGroupId (entityVal u) = do
            u' <- get . accountOwnerId $ entityVal x
            case u' of
                Nothing -> return NoPermissions
                Just u'' -> return $ if Just g == userGroupId u''
                                     then ReadPermissions
                                     else NoPermissions
        | otherwise = return NoPermissions

    validateElement u x = return $ Just x { accountOwnerId = entityKey u }

instance DBElement AccountSyncing where
    insertElement x@(AccountSyncing a1 a2 f)
        | a1 < a2 = P.insert x
        | a1 > a2 = P.insert (AccountSyncing a2 a1 f)
        | otherwise = error "Cannot sync account with itself!"

    replaceElement k x@(AccountSyncing a1 a2 f)
        | a1 < a2 = P.replace k x
        | a1 > a2 = P.replace k (AccountSyncing a2 a1 f)
        | otherwise = error "Cannot sync account with itself!"

    deleteElement k = P.delete k >> return True

    nativeOrdering x = asc (x ^. AccountSyncingId)

    readable u as = exists $ from $ \a -> where_ $
        (a ^. AccountOwnerId ==. val (entityKey u))
        &&. (((as ^. AccountSyncingAccount1) ==. a ^. AccountId)
             ||. ((as ^. AccountSyncingAccount2) ==. a ^. AccountId))

    checkPermissions u x = fmap (fromMaybe NoPermissions) . runMaybeT $ do
        a1 <- MaybeT . get . accountSyncingAccount1 $ entityVal x
        a2 <- MaybeT . get . accountSyncingAccount2 $ entityVal x

        if accountOwnerId a1 == entityKey u || accountOwnerId a2 == entityKey u
            then return ReadWritePermissions -- only enables to delete expenses (see validateElement)
            else return NoPermissions

    validateElement u x = runMaybeT $ do
        -- only admins can create and edit account syncings
        -- (otherwise every group member could get access to other people's expenses by syncing)
        unless (userIsAdmin $ entityVal u) $ fail ""

        a1 <- MaybeT . get $ accountSyncingAccount1 x
        u1 <- MaybeT . get $ accountOwnerId a1

        a2 <- MaybeT . get $ accountSyncingAccount2 x
        u2 <- MaybeT . get $ accountOwnerId a2

        g1 <- lift . return $ userGroupId u1
        g2 <- lift . return $ userGroupId u2

        if g1 == g2 then return x else fail ""

instance DBElement Category where
    deleteElement k = do
        first_exp <- P.selectFirst [ ExpenseCategoryId P.==. k ] []
        if isNothing first_exp
            then do
                deleteWhere [ CategoryReplacementOriginal P.==. k ]
                deleteWhere [ CategoryReplacementReplacement P.==. k ]

                P.delete k
                return True
            else return False

    nativeOrdering x = asc (x ^. CategoryId)

    readable u cat
        | Just gid <- userGroupId (entityVal u) = exists $ from $ \u' ->
            where_ $ (cat ^. CategoryOwnerId ==. (u' ^. UserId))
            &&. ((u' ^. UserGroupId) ==. just (val gid))
        | otherwise = cat ^. CategoryOwnerId ==. val (entityKey u)

    checkPermissions u x
        | entityKey u == categoryOwnerId (entityVal x) =
            return ReadWritePermissions
        | Just g <- userGroupId (entityVal u) = do
            u' <- get . categoryOwnerId $ entityVal x
            case u' of
                Nothing -> return NoPermissions
                Just u'' -> return $ if Just g == userGroupId u''
                                     then ReadPermissions
                                     else NoPermissions
        | otherwise = return NoPermissions

    validateElement u x = return . Just $ x { categoryOwnerId = entityKey u }

instance DBElement CategoryReplacement where
    nativeOrdering x = asc (x ^. CategoryReplacementId)

    readable u catr = catr ^. CategoryReplacementOwnerId ==. val (entityKey u)

    checkPermissions u x
        | entityKey u == categoryReplacementOwnerId (entityVal x) =
            return ReadWritePermissions
        | otherwise = return NoPermissions

    validateElement u x = runMaybeT $ do
        -- only edit/create replacements for yourself
        unless (categoryReplacementOwnerId x == entityKey u) $ fail ""

        -- replacement must belong to you
        cr <- MaybeT . get $ categoryReplacementReplacement x
        unless (categoryOwnerId cr == entityKey u) $ fail ""

        -- ... but the original may not.
        co <- MaybeT . get $ categoryReplacementOriginal x
        uo <- MaybeT . get $ categoryOwnerId co
        when (categoryOwnerId co == entityKey u) $ fail ""

        -- still, the other category must belong to a user of the same group
        g <- lift . return . userGroupId $ entityVal u
        go <- lift . return $ userGroupId uo

        if g == go then return x else fail ""

instance DBElement Balance where
    nativeOrdering x = asc (x ^. BalanceId)

    readable (Entity uid _) balance =
        (balance ^. BalanceAccountId `in_` ownAccounts)
        ||. (balance ^. BalanceAccountId `in_` syncedAccounts1)
        ||. (balance ^. BalanceAccountId `in_` syncedAccounts2)
      where
        ownAccounts = subList_select $ from $ \account -> do
            where_ $ (account ^. AccountOwnerId) ==. val uid
            return (account ^. AccountId)

        syncedAccounts1 = subList_select $ from $ \accountsync -> do
            where_ $ (accountsync ^. AccountSyncingAccount1) `in_` ownAccounts
            return (accountsync ^. AccountSyncingAccount2)

        syncedAccounts2 = subList_select $ from $ \accountsync -> do
            where_ $ (accountsync ^. AccountSyncingAccount2) `in_` ownAccounts
            return (accountsync ^. AccountSyncingAccount1)

    renderElements (Entity uid _) bals = do
        bs <- changeBalancePerspectives uid (entityVal <$> bals)
        return $ zipWith Entity (entityKey <$> bals) bs

    checkPermissions (Entity uid _) x = do
        -- easiest: check if rendering changes balance account to own account
        x' <- changeBalancePerspective uid (entityVal x)
        ownAccounts <- select $ from $ \account -> do
            where_ $ account ^. AccountOwnerId ==. val uid
            return account

        return $ if balanceAccountId x' `elem` (entityKey <$> ownAccounts)
                 then ReadWritePermissions
                 else NoPermissions

    validateElement (Entity uid _) x = do
        ownAccounts <- select $ from $ \account -> do
            where_ $ account ^. AccountOwnerId ==. val uid
            return account

        if balanceAccountId x `elem` (entityKey <$> ownAccounts)
            then return (Just x)
            else return Nothing

instance DBElement PeriodicRule where
    readable u x = x ^. PeriodicRuleOwnerId ==. val (entityKey u)

    nativeOrdering x = asc (x ^. PeriodicRuleId)

    checkPermissions u x
        | entityKey u == periodicRuleOwnerId (entityVal x) =
            return ReadWritePermissions
        | otherwise = return NoPermissions

    validateElement u x = runMaybeT $ do
        -- template must exist and be visible
        let ek = periodicRuleTemplateId x
        template <- MaybeT $ get ek
        perm <- lift $ checkPermissions u (Entity ek template)
        unless (perm >= ReadPermissions) $ fail ""

        return $ x { periodicRuleOwnerId = entityKey u }

instance DBElement AutomationRule where
    nativeOrdering x = asc (x ^. AutomationRuleId)

    readable u x = x ^. AutomationRuleOwnerId ==. val (entityKey u)

    checkPermissions u x
        | entityKey u == automationRuleOwnerId (entityVal x) =
            return ReadWritePermissions
        | otherwise = return NoPermissions

    validateElement u x = runMaybeT $ do
        -- template must exist and be visible
        let ek = automationRuleTemplateId x
        atemplate <- MaybeT $ get ek
        perm <- lift $ checkPermissions u (Entity ek atemplate)
        unless (perm >= ReadPermissions) $ fail ""

        return $ x { automationRuleOwnerId = entityKey u }

instance DBElement SeenExpense where
    nativeOrdering x = asc (x ^. SeenExpenseId)

    readable u se = exists $ from $ \a -> where_ $
        ((se ^. SeenExpenseAccountId) ==. a ^. AccountId)
        &&. (a ^. AccountOwnerId ==. val (entityKey u))

    checkPermissions u x = fmap (fromMaybe NoPermissions) . runMaybeT $ do
        a <- MaybeT . get . seenExpenseAccountId $ entityVal x
        return $ if accountOwnerId a == entityKey u
                 then ReadWritePermissions
                 else NoPermissions

    validateElement u x = runMaybeT $ do
        a <- MaybeT . get $ seenExpenseAccountId x
        unless (accountOwnerId a == entityKey u) $ fail ""
        return x

instance DBElement Device where
    nativeOrdering x = asc (x ^. DeviceId)

    readable u x = x ^. DeviceOwnerId ==. val (entityKey u)

    checkPermissions u x
        | entityKey u == deviceOwnerId (entityVal x) =
            return ReadWritePermissions
        | otherwise = return NoPermissions

    validateElement u x = return . Just $ x { deviceOwnerId = entityKey u }

instance DBElement ExpenseFilter where
    nativeOrdering x = asc (x ^. ExpenseFilterId)

    readable u f = exists $ from $ \d -> where_ $
        ((f ^. ExpenseFilterDeviceId) ==. d ^. DeviceId)
        &&. (d ^. DeviceOwnerId ==. val (entityKey u))

    checkPermissions u x = fmap (fromMaybe NoPermissions) . runMaybeT $ do
        e <- MaybeT . get . expenseFilterDeviceId $ entityVal x
        return $ if deviceOwnerId e == entityKey u
                 then ReadWritePermissions
                 else NoPermissions

    validateElement u x = runMaybeT $ do
        e <- MaybeT . get $ expenseFilterDeviceId x
        unless (deviceOwnerId e == entityKey u) $ fail ""
        return x

instance DBElement ExpenseFlag where
    nativeOrdering x = asc (x ^. ExpenseFlagId)

    readable (Entity uid _) x =
        exists $ from $ \expense -> where_ $ expenseVisible uid expense
        &&. ((x ^. ExpenseFlagExpenseId) ==. expense ^. ExpenseId)

    checkPermissions u x = fmap (fromMaybe NoPermissions) . runMaybeT $ do
        e <- MaybeT . get . expenseFlagExpenseId $ entityVal x
        lift $
            checkPermissions u (Entity (expenseFlagExpenseId $ entityVal x) e)

    validateElement _ _ = return Nothing -- cf. instance for Expense

instance DBElement ExpenseSharing where
    nativeOrdering x = asc (x ^. ExpenseSharingId)

    readable (Entity uid _) x =
        exists $ from $ \expense -> where_ $ expenseVisible uid expense
        &&. ((x ^. ExpenseSharingExpenseId) ==. expense ^. ExpenseId)

    checkPermissions u x = fmap (fromMaybe NoPermissions) . runMaybeT $ do
        e <- MaybeT . get . expenseSharingExpenseId $ entityVal x
        lift $
            checkPermissions u
                             (Entity (expenseSharingExpenseId $ entityVal x) e)

    validateElement _ _ = return Nothing -- cf. instance for Expense

instance DBElement Stock where
    nativeOrdering x = asc (x ^. StockId)

    readable u x = x ^. StockOwnerId ==. val (entityKey u)

    checkPermissions u x
        | entityKey u == stockOwnerId (entityVal x) =
            return ReadWritePermissions
        | otherwise = return NoPermissions

    validateElement u x = return . Just $ x { stockOwnerId = entityKey u }

instance DBElement SecuritiesAccount where
    nativeOrdering x = asc (x ^. SecuritiesAccountId)

    readable u x = x ^. SecuritiesAccountOwnerId ==. val (entityKey u)

    checkPermissions u x
        | entityKey u == securitiesAccountOwnerId (entityVal x) =
            return ReadWritePermissions
        | otherwise = return NoPermissions

    validateElement u x =
        return . Just $ x { securitiesAccountOwnerId = entityKey u }

instance DBElement StockTransaction where
    nativeOrdering x = asc (x ^. StockTransactionId)

    validateElement u x = runMaybeT $ do
        e <- MaybeT . get $ stockTransactionAccountId x
        unless (securitiesAccountOwnerId e == entityKey u) $ fail ""
        return x

    readable (Entity uid _) x = exists $ from $
        \secacc -> where_ $ secacc ^. SecuritiesAccountOwnerId ==. val uid
        &&. ((x ^. StockTransactionAccountId) ==. secacc ^. SecuritiesAccountId)

    checkPermissions (Entity uid _) x = do
        sacc <- get . stockTransactionAccountId $ entityVal x

        case sacc of
            Just sacc' -> if securitiesAccountOwnerId sacc' == uid
                          then return ReadWritePermissions
                          else return NoPermissions
            Nothing -> return NoPermissions

instance DBElement StockInfo where
    nativeOrdering x = asc (x ^. StockInfoId)

    readable _ _ = val True

    checkPermissions _ _ = return ReadPermissions

    validateElement _ _ = return Nothing

instance DBElement StockExchange where
    nativeOrdering x = asc (x ^. StockExchangeId)

    readable _ _ = val True

    checkPermissions _ _ = return ReadPermissions

    validateElement _ _ = return Nothing

instance DBElement StockHistoricalPrice where
    nativeOrdering x = asc (x ^. StockHistoricalPriceId)

    readable _ _ = val True

    checkPermissions _ _ = return ReadPermissions

    validateElement _ _ = return Nothing

instance DBElement StockCurrentPrice where
    nativeOrdering x = asc (x ^. StockCurrentPriceId)

    readable _ _ = val True

    checkPermissions _ _ = return ReadPermissions

    validateElement _ _ = return Nothing
