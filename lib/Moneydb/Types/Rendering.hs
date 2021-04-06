{-# LANGUAGE FlexibleInstances #-}

module Moneydb.Types.Rendering
    ( RenderedAccount
    , RenderedExpense
    , RenderedCategory
    , RenderedUser
    , RenderedSharing
    , RenderedDevice
    , RenderedStock
    , RenderedStockExchange
    , convertUser
    , convertExpense
    , convertSharing
    , unrenderSharing
    , unrenderExpense
    , convertAccount
    , convertCategory
    , convertDevice
    , unrenderAccount
    , unrenderCategory
    , unrenderDevice
    ) where

import           Data.Decimal                     (Decimal)
import           Moneydb.Types.Basic.Schema       (AccountSyncing, CategoryId
                                                 , ExpenseFilter, UserId)
import qualified Moneydb.Types.Basic.Schema       as BS
import qualified Moneydb.Types.Expenses.Schema    as ES
import           Moneydb.Types.Expenses.Types     (ExpenseFlagging)
import           Moneydb.Types.Rendering.Account
import           Moneydb.Types.Rendering.Category
import           Moneydb.Types.Rendering.Device
import           Moneydb.Types.Rendering.Expense
import           Moneydb.Types.Rendering.Stock
import           Moneydb.Types.Rendering.User

convertUser :: BS.User -> RenderedUser
convertUser (BS.User n _ fn g a) = RenderedUser n fn g a

convertExpense :: ES.Expense
               -> Decimal
               -> [RenderedSharing]
               -> [ExpenseFlagging]
               -> RenderedExpense
convertExpense (ES.Expense _ a b c d e f g h i j k l m n) x =
    RenderedExpense a x b c d e f g h i j k l m n

convertSharing :: ES.ExpenseSharing -> Decimal -> RenderedSharing
convertSharing (ES.ExpenseSharing _ aid typ para) =
    RenderedSharing aid typ para

unrenderSharing :: ES.ExpenseId -> RenderedSharing -> ES.ExpenseSharing
unrenderSharing eid (RenderedSharing aid typ para _) =
    ES.ExpenseSharing eid aid typ para

unrenderExpense :: UserId -> RenderedExpense -> ES.Expense
unrenderExpense uid (RenderedExpense a _ b c d e f g h i j k l m n _ _) =
    ES.Expense uid a b c d e f g h i j k l m n

convertAccount :: BS.Account -> Maybe AccountSyncing -> RenderedAccount
convertAccount (BS.Account _ a b c d e f g) = RenderedAccount a b c d e f g

convertCategory :: BS.Category -> [CategoryId] -> RenderedCategory
convertCategory (BS.Category _ a b c) = RenderedCategory a b c

convertDevice :: BS.Device -> [ExpenseFilter] -> RenderedDevice
convertDevice (BS.Device _ a b c d e f) = RenderedDevice a b c d e f

unrenderCategory :: UserId -> RenderedCategory -> (BS.Category, [CategoryId])
unrenderCategory uid (RenderedCategory a b c d) = (BS.Category uid a b c, d)

unrenderAccount :: UserId
                -> RenderedAccount
                -> (BS.Account, Maybe AccountSyncing)
unrenderAccount uid (RenderedAccount a b c d e f g h) =
    (BS.Account uid a b c d e f g, h)

unrenderDevice :: UserId -> RenderedDevice -> (BS.Device, [ExpenseFilter])
unrenderDevice uid (RenderedDevice a b c d e f g) =
    (BS.Device uid a b c d e f, g)
