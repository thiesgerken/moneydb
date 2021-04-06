{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Moneydb.Util(hashPassword, hashPassword', verifyPassword) where

import qualified Crypto.PasswordStore  as PWStore
import           Data.ByteString.Char8 (ByteString, pack, unpack)
import           Moneydb.Types

hashPassword :: ByteString -> IO ByteString
hashPassword p = PWStore.makePassword p 10

hashPassword' :: String -> IO String
hashPassword' p = unpack <$> hashPassword (pack p)

verifyPassword :: User -> ByteString -> Bool
verifyPassword u pw = PWStore.verifyPassword pw (pack $ userPasswordHash u)
