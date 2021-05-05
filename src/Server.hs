{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Server
  ( HasCookieConfig(..)
  , HasJwtConfig(..)
  , contextProxy
  , login
  , server
  )
where
import           Servant                        ( (:<|>)(..) )
import qualified Servant                       as SV

import qualified Servant.Auth.Server           as AS

import           RIO
import           RIOThrowAll

import           Api


-- The server requires the cookie configuration to be passed along in the environment
class HasCookieConfig config where
  cookieConfigL :: Lens' config AS.CookieSettings

-- The server requires the JWT configuration to be passed along in the environment
class HasJwtConfig config where
    jwtConfigL :: Lens' config AS.JWTSettings

contextProxy :: Proxy '[AS.CookieSettings, AS.JWTSettings]
contextProxy = Proxy

-- | The server responsible for the `login` endpoint.
loginServer
  :: (HasCookieConfig env, HasJwtConfig env, HasLogFunc env)
  => SV.ServerT LoginEndpoint (RIO env)
loginServer = login

-- | A very much simplified login check that authenticates a single user only.
login
  :: (HasCookieConfig env, HasJwtConfig env, HasLogFunc env)
  => Login
  -> RIO env CookieHeader
login (Login "uliSchuster" "uli") = do
  cc <- view cookieConfigL
  jc <- view jwtConfigL
  logDebug "Authenticating user uliSchuster"
  let usr = User "uliSchuster" "ulrich.schuster@koing.de"
  mApplyCookies <- liftIO $ AS.acceptLogin cc jc usr
  case mApplyCookies of
    Nothing           -> throwIO SV.err401
    Just applyCookies -> return $ applyCookies SV.NoContent
login _ = do
  logDebug "Not authenticated"
  throwIO SV.err401


-- | The server for the `unprotected` endpoint.
unprotectedServer :: SV.ServerT UnprotectedEndpoint (RIO env)
unprotectedServer = return "This is unprotected" :<|> return SV.NoContent

-- | The server for the `protected` endpoint.
-- This server receives the result of the cookie-based authentication combinator.
protectedServer :: AS.AuthResult User -> SV.ServerT ProtectedEndpoint (RIO env)
-- If we get an "Authenticated v", we can trust the information in v, since
-- it was signed by a key we trust.
protectedServer (AS.Authenticated _) =
  return "Authenticated" :<|> return SV.NoContent
-- return (username (user :: User)) 
-- Otherwise, we return a 401.
protectedServer _ = rioThrowAll SV.err401 --throwIO SV.err401 :<|> throwIO SV.err401

-- | The overall server for all endpoints.
server
  :: (HasCookieConfig env, HasJwtConfig env, HasLogFunc env)
  => SV.ServerT Api (RIO env)
server = loginServer :<|> unprotectedServer :<|> protectedServer
