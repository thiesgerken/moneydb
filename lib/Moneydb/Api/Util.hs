{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Moneydb.Api.Util(throw404, throw401, throw400, throw401') where

import           Control.Monad.Error.Class (MonadError)
import           Control.Monad.Except      (throwError)
import qualified Data.Text.Lazy            as TL
import qualified Data.Text.Lazy.Encoding   as TLE
import           Prelude                   as P
import           Servant.Server

throw404 :: (MonadError ServerError m, Show a1) => a1 -> m a2
throw404 i = throwError err404 { errBody = TLE.encodeUtf8 $ "Item with id " <> TL.pack (show i) <> " not found" }

throw401' :: MonadError ServerError m => TL.Text -> m a2
throw401' s = throwError err401 { errBody = TLE.encodeUtf8 s }

throw401 :: (MonadError ServerError m, Show a1) => a1 -> m a2
throw401 i = throw401' $ "No permission to view item with id " <> TL.pack (show i)

throw400 :: MonadError ServerError m => TL.Text -> m a
throw400 s = throwError err400 { errBody = TLE.encodeUtf8 s }
