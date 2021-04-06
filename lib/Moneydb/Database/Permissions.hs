{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- | database bindings for moneydb.
--   Note that this module may abort execution on malformed queries or process them, leading to a corrupt database.
--   (example: Adding an expense with different ownerid and owner of account & account is not shared)
module Moneydb.Database.Permissions
    ( Permissions(..)
    , expenseVisible
    , expenseVisible'
    ) where

import           Database.Esqueleto hiding (isNothing)
import           Moneydb.Types

data Permissions = NoPermissions | ReadPermissions | ReadWritePermissions
    deriving (Ord, Eq)

expenseVisible :: UserId -> SqlExpr (Entity Expense) -> SqlExpr (Value Bool)
expenseVisible uid = expenseVisible' (val uid)

expenseVisible' :: SqlExpr (Value UserId)
                -> SqlExpr (Entity Expense)
                -> SqlExpr (Value Bool)
expenseVisible' uid expense = (expense ^. ExpenseAccountId `in_` ownAccounts)
    ||. (expense ^. ExpenseAccountId `in_` syncedAccounts1)
    ||. (expense ^. ExpenseAccountId `in_` syncedAccounts2)
    ||. (expense ^. ExpenseId `in_` sharedExps)
  where
    ownAccounts = subList_select $ from $ \a -> do
        where_ $ (a ^. AccountOwnerId) ==. uid
        return (a ^. AccountId)

    syncedAccounts1 = subList_select $ from $ \accountsync -> do
        where_ $ (accountsync ^. AccountSyncingAccount1) `in_` ownAccounts
        return (accountsync ^. AccountSyncingAccount2)

    syncedAccounts2 = subList_select $ from $ \accountsync -> do
        where_ $ (accountsync ^. AccountSyncingAccount2) `in_` ownAccounts
        return (accountsync ^. AccountSyncingAccount1)

    sharedExps = subList_select (from $ \es -> do
                                     where_ ((es ^. ExpenseSharingAccountId
                                              `in_` ownAccounts)
                                             ||. (es ^. ExpenseSharingAccountId
                                                  `in_` syncedAccounts1)
                                             ||. (es ^. ExpenseSharingAccountId
                                                  `in_` syncedAccounts2))
                                     return (es ^. ExpenseSharingExpenseId))
