{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TestConfig
  ( Env(..)
  , cookieConf
  )
where

import qualified Servant                       as SV
import qualified Servant.Auth.Server           as AS
import qualified RIO.Time                      as T
import qualified Network.HTTP.Client           as HC

import           RIO
import           Server

-- Configuration environment to be threaded through the entire application.
-- Placeholder for the present experimental app.
data Env = Env
  { cookieConfig :: AS.CookieSettings
  , jwtConfig :: AS.JWTSettings
  , logger :: LogFunc
  , cookieJar :: TVar HC.CookieJar
  , someConfig :: Text }

class HasCookieJar config where
  cookieJarL :: Lens' config (TVar HC.CookieJar)

instance HasCookieJar Env where
  cookieJarL = lens cookieJar (\x y -> x { cookieJar = y })

instance HasCookieConfig Env where
  cookieConfigL = lens cookieConfig (\x y -> x { cookieConfig = y })

instance HasJwtConfig Env where
  jwtConfigL = lens jwtConfig (\x y -> x { jwtConfig = y })

instance HasLogFunc Env where
  logFuncL = lens logger (\x y -> x { logger = y })

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
