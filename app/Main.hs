{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import           RIO

import           Servant                        ( Context(..) )
import qualified Servant                       as SV
import qualified Servant.Auth.Server           as AS
import qualified Network.Wai.Handler.Warp      as Warp
import qualified AppConfig                     as Conf
import qualified Application                   as App
import qualified Logging                       as Log


main :: IO ()
main = do
    -- We generate the key for signing tokens. This would generally be persisted,
    -- and kept safely.
  myKey <- AS.generateKey
  -- Adding some configurations. All authentications require CookieSettings to
  -- be in the context.
  let jwtConf = AS.defaultJWTSettings myKey
      ctx     = Conf.cookieConf :. jwtConf :. SV.EmptyContext
      env     = Conf.Env { Conf.cookieConfig = Conf.cookieConf
                         , Conf.jwtConfig    = jwtConf
                         , Conf.logger       = Log.myLogFunc
                         , Conf.someConfig   = "Some configuration string"
                         }
  Warp.run 8081 $ App.myApp ctx env

