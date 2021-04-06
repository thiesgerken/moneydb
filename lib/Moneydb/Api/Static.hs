{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module Moneydb.Api.Static(serveStatic) where

import           Data.Maybe                      (fromJust)
import           Servant.API.Raw                 (Raw)
import           Servant.Server                  (ServerT, Tagged (..))
import           WaiAppStatic.Types              (MaxAge (..), StaticSettings (..), toPiece)

#ifdef LOCALWWW

import           Network.Wai.Application.Static  (staticApp)
import           WaiAppStatic.Storage.Filesystem (defaultWebAppSettings)

#else

import           Data.ByteString                 (ByteString)
import           Data.FileEmbed                  (embedDir)
import           Language.Haskell.TH.Syntax      (addDependentFile)
import           Network.Wai.Application.Static  (embeddedSettings, staticApp)

#endif

#ifdef LOCALWWW

serveStatic :: ServerT Raw m
serveStatic = Tagged . staticApp $ ((defaultWebAppSettings "./web/dist") {ssMaxAge = NoMaxAge, ssListing = Nothing, ssIndices = [fromJust $ toPiece "index.html"], ssUseHash = True})

#else

serveStatic :: ServerT Raw m
serveStatic = Tagged . staticApp $ (embeddedSettings webDir) {ssMaxAge = NoMaxAge, ssListing = Nothing, ssUseHash = True, ssIndices = [fromJust $ toPiece "index.html"]}

webDir :: [(FilePath, ByteString)]
webDir = $(addDependentFile "./web/dist/index.html" >> embedDir "./web/dist")

#endif
