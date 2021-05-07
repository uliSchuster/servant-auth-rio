{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TestUtils
  ( withClient
  , withAuthentication
  , tryRequest
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
import qualified Api


withClient :: (SC.ClientEnv -> IO ()) -> IO ()
withClient request = do
  -- Make sure the server is running,
  -- a client is available to execute the request, 
  -- and the server is shutdown properly when the test has completed.
  myKey     <- AS.generateKey
  baseUrl   <- SC.parseBaseUrl "http://localhost"
  manager   <- HC.newManager HC.defaultManagerSettings
  -- Create a TVar to hold the client's cookies between requests.
  -- Without it, authentication and CSRF would not work.
  cookieJar <- newTVarIO (HC.createCookieJar [])
  -- Adding some configurations. All authentications require CookieSettings to
  -- be in the context.
  let jwtConf = AS.defaultJWTSettings myKey
      ctx     = Conf.cookieConf :. jwtConf :. SV.EmptyContext
      env     = Conf.Env { Conf.cookieConfig = Conf.cookieConf
                         , Conf.jwtConfig    = jwtConf
                         , Conf.logger       = Log.myLogFunc
                         , Conf.cookieJar    = cookieJar
                         , Conf.someConfig   = "Some configuration string"
                         }
  -- Start up a server on a random free port, execute a request, 
  -- and cleanly shut down the server.
  -- Pass the server configuration to the client as `clientEnv`.
  -- Provide mutable storage for cookies between successive requests.
  Warp.testWithApplication (pure $ App.myApp ctx env) $ \port ->
    let clientEnv' = SC.mkClientEnv manager (baseUrl { SC.baseUrlPort = port })
        clientEnv  = clientEnv' { SC.cookieJar = Just $ Conf.cookieJar env }
    in  request clientEnv

-- Let Servant generate an HTTP client for the login endpoint.
loginClient :: Api.Login -> SC.ClientM Api.CookieHeader
loginClient = SC.client (Proxy :: Proxy Api.LoginEndpoint)

-- Execute a request within an authenticated session.
withAuthentication :: Text -> Text -> (SC.ClientEnv -> IO ()) -> IO ()
withAuthentication user password request = withClient $ \clientEnv -> do
  -- Perform authentication to obtain a session cookie in client's cookieJar TVar.
  _ <- SC.runClientM (loginClient (Api.Login user password)) clientEnv
  request clientEnv

tryRequest :: SC.ClientEnv -> SC.ClientM a -> IO a
tryRequest clientEnv action =
  either throwIO return =<< SC.runClientM action clientEnv
