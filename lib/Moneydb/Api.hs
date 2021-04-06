{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Moneydb.Api(runApi, apiSwagger, api, apiJS) where

import           Control.Lens             ((&), (.~), (?~))
import qualified Data.ByteString          as B
import qualified Data.ByteString.Char8    as B8
import qualified Data.ByteString.Lazy     as BL
import           Data.Proxy               (Proxy (..))
import           Data.Swagger
import qualified Data.Text.Lazy           as TL
import qualified Data.Text.Lazy.Encoding  as TLE
import           Database.Persist         (Entity (..), getBy)
import           Network.HTTP.Types       (ok200)
import           Network.Wai              (responseLBS)
import           Network.Wai.Handler.Warp (HostPreference, defaultSettings, runSettings, setHost, setLogger, setPort)
import           Network.Wai.Logger       (withStdoutLogger)
import           Servant.API
import           Servant.JS               (CommonGeneratorOptions (..), FunctionName (..), camelCase,
                                           defCommonGeneratorOptions, jqueryWith, jsForAPI)
import           Servant.Server
import           Servant.Swagger          (toSwagger)
import           Servant.Swagger.UI       (SwaggerSchemaUI, swaggerSchemaUIServer)

import           Moneydb.Api.Automation
import           Moneydb.Api.Computation
import           Moneydb.Api.Element
import           Moneydb.Api.Query
import           Moneydb.Api.Static
import           Moneydb.Api.Update
import           Moneydb.Database
import           Moneydb.Types
import           Moneydb.Util

type ProtectedAPI = "api" :> (AutomationAPI :<|> ElementAPIs :<|> QueryAPIs :<|> UpdateAPI :<|> ComputeAPI)

type MoneyAPI = (BasicAuth "MoneyDB" (Entity User) :> ProtectedAPI) :<|> ("moneydb.js" :> Raw) :<|> SwaggerSchemaUI "swagger-ui" "swagger.json" :<|> Raw

protectedAPI :: Proxy ProtectedAPI
protectedAPI = Proxy

moneyAPI :: Proxy MoneyAPI
moneyAPI = Proxy

authCheck :: MoneyDBConn -> BasicAuthCheck (Entity User)
authCheck mc = BasicAuthCheck check
  where check (BasicAuthData u p) = if B.null u then return Unauthorized
            else runTransaction mc $ do
              user <- getBy (UserNameKey $ B8.unpack u)
              case user of
                Just user' ->
                  if verifyPassword (entityVal user') p then return (Authorized user')
                  else return BadPassword
                Nothing -> return NoSuchUser

basicAuthServerContext :: MoneyDBConn -> Context (BasicAuthCheck (Entity User) ': '[])
basicAuthServerContext mc = authCheck mc :. EmptyContext

basicAuthServer :: MoneyDBConn -> Server MoneyAPI
basicAuthServer mc = protectedHandler mc :<|> Tagged serveJS :<|> serveSwaggerUI :<|> serveStatic
  where serveJS _ respond = respond $ responseLBS ok200 [("Content-Type", "text/javascript")] apiJS
        serveSwaggerUI = swaggerSchemaUIServer apiSwagger

protectedHandler :: MoneyDBConn -> Entity User -> Server ProtectedAPI
protectedHandler mc u = automationHandler mc u :<|> elementHandlers mc u :<|> queryHandlers mc u :<|> updateHandler mc u :<|> computeHandler mc u

api :: MoneyDBConn -> Application
api mc = serveWithContext moneyAPI (basicAuthServerContext mc) (basicAuthServer mc)

apiJS :: BL.ByteString
apiJS = TLE.encodeUtf8 $ preamble <> defs
  where defs = TL.replace " = function" "" . TL.replace "EXPORTED." "export function " . TL.replace "$.ajax" "return $.ajax" . TL.fromStrict . jsForAPI protectedAPI $ jqueryWith defCommonGeneratorOptions {functionNameBuilder = camelCase . modifier, moduleName = "EXPORTED"}
        modifier (FunctionName xs) = FunctionName $ filter ("api" /=) xs
        preamble = "\'use strict';\n\
                    \import $ from 'jquery';\n"

apiSwagger :: Swagger
apiSwagger = toSwagger protectedAPI
  & info.title        .~ "MoneyDB"
  & info.version      .~ "2.1"
  & info.description  ?~ "API for MoneyDB, version 2.1"
  & info.license      ?~ "BSD3"
  & securityDefinitions .~ [("basicAuth", SecurityScheme SecuritySchemeBasic Nothing)]
  & security .~ [SecurityRequirement [("basicAuth", [])]]

runApi :: MoneyDBConn -> HostPreference -> Int -> IO ()
runApi mc hst i = withStdoutLogger $ \apl -> do
    let settings = setPort i . setHost hst $ setLogger apl defaultSettings
    runSettings settings (api mc)
