{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TestUtils
  ( withClient
  )
where

import           RIO
import qualified Network.Wai.Handler.Warp      as Warp
import qualified Network.HTTP.Client           as HC
import           Servant                        ( Context(..) )
import qualified Servant                       as SV
import qualified Servant.Auth.Server           as AS
import qualified Servant.Client                as SC
import qualified TestConfig                    as Conf
import qualified Application                   as App
import qualified Logging                       as Log


withClient :: (SC.ClientEnv -> IO ()) -> IO ()
withClient request = do
  -- withClien makes sure the server is running,
  -- a client is available to execute the request, 
  -- and the server is shutdown properly when the test has completed.
  myKey   <- AS.generateKey
  baseUrl <- SC.parseBaseUrl "http://localhost"
  manager <- HC.newManager HC.defaultManagerSettings
  -- Adding some configurations. All authentications require CookieSettings to
  -- be in the context.
  let jwtConf = AS.defaultJWTSettings myKey
      ctx     = Conf.cookieConf :. jwtConf :. SV.EmptyContext
      env     = Conf.Env { Conf.cookieConfig = Conf.cookieConf
                         , Conf.jwtConfig    = jwtConf
                         , Conf.logger       = Log.myLogFunc
                         , Conf.someConfig   = "Some configuration string"
                         }
  -- Start up a server on a random free port, execute a request, 
  -- and cleanly shut down the server.
  -- Pass the server configuration to the client as `clientEnv`.
  Warp.testWithApplication (pure $ App.myApp ctx env) $ \port ->
    let clientEnv = SC.mkClientEnv manager (baseUrl { SC.baseUrlPort = port })
    in  request clientEnv
