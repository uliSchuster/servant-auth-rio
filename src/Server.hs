{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Server
  ( apiProxy
  , contextProxy
  , server
  )
where
import           Servant                        ( (:<|>)(..) )
import qualified Servant                       as SV

import qualified Servant.Auth.Server           as AS

import           RIO

import           Api

apiProxy :: Proxy Api
apiProxy = Proxy

contextProxy :: Proxy '[AS.CookieSettings, AS.JWTSettings]
contextProxy = Proxy

-- | The server responsible for the `login` endpoint.
loginServer
  :: AS.CookieSettings -> AS.JWTSettings -> SV.ServerT LoginEndpoint (RIO m)
loginServer = login

-- | A very much sinmplified login check that authenticates a single user only.
login :: AS.CookieSettings -> AS.JWTSettings -> Login -> RIO m CookieHeader
login cookieSettings jwtSettings (Login "uliSchuster" "uli") = do
  let usr = User "uliSchuster" "ulrich.schuster@koing.de"
  mApplyCookies <- liftIO $ AS.acceptLogin cookieSettings jwtSettings usr
  case mApplyCookies of
    Nothing           -> throwIO SV.err401
    Just applyCookies -> return $ applyCookies SV.NoContent
login _ _ _ = throwIO SV.err401


-- | The server for the `protected` endpoint.
-- This server receives the result of the cookie-based authentication combinator.
protectedServer :: AS.AuthResult User -> SV.ServerT ProtectedEndpoint (RIO m)
-- If we get an "Authenticated v", we can trust the information in v, since
-- it was signed by a key we trust.
protectedServer (AS.Authenticated user) = return (username (user :: User))
-- Otherwise, we return a 401.
protectedServer _                       = throwIO SV.err401

-- | The overall server for all endpoints.
server :: AS.CookieSettings -> AS.JWTSettings -> SV.ServerT Api (RIO m)
server cs jwt = loginServer cs jwt :<|> protectedServer
