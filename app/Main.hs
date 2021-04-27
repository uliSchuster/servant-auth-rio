{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import           RIO

import           Servant                        ( Context(..) )
import qualified Servant                       as SV
import qualified Servant.Auth.Server           as AS
import qualified Servant.Auth.Server.Internal.AddSetCookie
                                               as ASC
import qualified Network.Wai.Handler.Warp      as Warp
import qualified RIO.Time                      as T
import           Control.Monad.Except           ( ExceptT(..) )


import           Api
import           Server

-- Workaround https://github.com/haskell-servant/servant-auth/issues/177
type instance ASC.AddSetCookieApi (SV.NoContentVerb 'SV.DELETE)
  = SV.Verb 'SV.DELETE 204 '[SV.JSON] (ASC.AddSetCookieApiVerb SV.NoContent)

-- Configuration environment to be threaded through the entire application.
-- Placeholder for the present experimental app.
newtype Env = Env
  { config :: Text }

-- Helper function to hoist our RIO handler into a Servant Handler.
hoistAppServer :: AS.CookieSettings -> AS.JWTSettings -> Env -> SV.Server Api
hoistAppServer cookieSettings jwtSettings env = SV.hoistServerWithContext
  apiProxy
  contextProxy
  (nt env)
  (server cookieSettings jwtSettings)
 where
    -- Natural transformation to map the RIO monad stack to Servant's Handler.
    -- We want to use the RIO monad for our application to run in, instead of Servant's 
    -- regular Handler monad that uses the ExceptT antipattern.
    -- https://harporoeder.com/posts/servant-13-reader-io/
  nt :: Env -> RIO Env a -> SV.Handler a
  nt e m = SV.Handler $ ExceptT $ try $ runRIO e m


main :: IO ()
main = do
    -- We generate the key for signing tokens. This would generally be persisted,
    -- and kept safely.
  myKey <- AS.generateKey
  -- Adding some configurations. All authentications require CookieSettings to
  -- be in the context.
  let jwtCfg = AS.defaultJWTSettings myKey
      cfg    = cookieConf :. jwtCfg :. SV.EmptyContext
      env    = Env { config = "Some configuration string" }
  Warp.run 8081 $ SV.serveWithContext apiProxy cfg $ hoistAppServer cookieConf
                                                                    jwtCfg
                                                                    env


cookieConf :: AS.CookieSettings
cookieConf = AS.defaultCookieSettings
  { AS.cookieIsSecure    = SV.NotSecure -- For local testing via HTTP (instead of HTTPS)
  , AS.cookieMaxAge      = Just $ T.secondsToDiffTime $ 60 * 60 * 24 * 365
  , AS.cookieXsrfSetting = Just $ AS.def
                             { AS.xsrfCookieName = encodeUtf8 "X-XSRF"
                             , AS.xsrfHeaderName = encodeUtf8 "X-XSRF"
                             , AS.xsrfExcludeGet = True
                             }
  }
