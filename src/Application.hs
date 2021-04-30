{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Application
  ( myApp
  )
where

import           RIO
import qualified Servant                       as SV
import qualified Servant.Auth.Server           as AS
import qualified Servant.Auth.Server.Internal.AddSetCookie
                                               as ASC
import qualified Network.Wai                   as Wai
import           Api
import qualified Server                        as S
import           Control.Monad.Except           ( ExceptT(..) )

-- Workaround https://github.com/haskell-servant/servant-auth/issues/177
type instance ASC.AddSetCookieApi (SV.NoContentVerb 'SV.DELETE)
  = SV.Verb 'SV.DELETE 204 '[SV.JSON] (ASC.AddSetCookieApiVerb SV.NoContent)

type AuthContext = SV.Context '[AS.CookieSettings, AS.JWTSettings]

-- Helper function to hoist our RIO handler into a Servant Handler.
hoistAppServer
  :: (S.HasCookieConfig env, S.HasJwtConfig env, HasLogFunc env)
  => env
  -> SV.Server Api
hoistAppServer env = SV.hoistServerWithContext S.apiProxy
                                               S.contextProxy
                                               (nt env)
                                               S.server
 where
    -- Natural transformation to map the RIO monad stack to Servant's Handler.
    -- We want to use the RIO monad for our application to run in, instead of Servant's 
    -- regular Handler monad that uses the ExceptT antipattern.
    -- https://harporoeder.com/posts/servant-13-reader-io/
  nt :: env -> RIO env a -> SV.Handler a
  nt e m = SV.Handler $ ExceptT $ try $ runRIO e m


-- Create a Wai application from the hoisted Servant server.
myApp
  :: (S.HasCookieConfig env, S.HasJwtConfig env, HasLogFunc env)
  => AuthContext
  -> env
  -> Wai.Application
myApp ctx env = SV.serveWithContext S.apiProxy ctx $ hoistAppServer env
