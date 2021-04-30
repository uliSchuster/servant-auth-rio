{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AppConfig
  ( Env(..)
  , cookieConf
  )
where

import qualified Servant                       as SV
import qualified Servant.Auth.Server           as AS
import qualified RIO.Time                      as T

import           RIO
import           Server

-- Configuration environment to be threaded through the entire application.
-- Placeholder for the present experimental app.
data Env = Env
  { cookieConfig :: AS.CookieSettings
  , jwtConfig :: AS.JWTSettings
  , logger :: !LogFunc
  , someConfig :: Text }

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
