{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Api
  ( Login(..)
  , User(..)
  , LoginEndpoint
  , ProtectedEndpoint
  , UnprotectedEndpoint
  , Api
  , CookieHeader
  , apiProxy
  )
where

import qualified Data.Aeson                    as J
import           RIO
import           Servant                        ( (:>)
                                                , (:<|>)
                                                )
import qualified Servant                       as SV
import qualified Servant.Auth.Server           as AS

-- Login request 
data Login = Login
  { username :: !Text
  , password :: !Text
  }
  deriving (Eq, Show, Generic)

instance J.FromJSON Login
instance J.ToJSON Login

-- Authentication cookie contents
data User = User
  { username :: !Text
  , email :: !Text
  } deriving (Eq, Show, Generic)

instance J.ToJSON User
instance J.FromJSON User
instance AS.ToJWT User
instance AS.FromJWT User

-- Type for the response cookie header. Includes the auth and XSRF cookies.
type CookieHeader
  = ( SV.Headers
        '[SV.Header "Set-Cookie" AS.SetCookie, SV.Header
          "Set-Cookie"
          AS.SetCookie]
        SV.NoContent
    )

-- A login endpoint that sets authentication and XSRF cookies upon success.
type LoginEndpoint
  = "login" :> SV.ReqBody '[SV.JSON] Login :> SV.Verb 'SV.POST 204 '[SV.JSON] CookieHeader

-- An unprotected endpoint
type UnprotectedEndpoint
  = "unprotected" :> (SV.Get '[SV.JSON] Text :<|> SV.DeleteNoContent)

-- An endpoint that requires authentication.
type ProtectedEndpoint
  = "protected" :> SV.Get '[SV.JSON] Text :<|> SV.DeleteNoContent

-- The overall API, with cookie authentication on the protected endpoint
type Api
  = LoginEndpoint :<|> UnprotectedEndpoint :<|> (AS.Auth '[AS.Cookie, AS.JWT] User :> ProtectedEndpoint)

apiProxy :: Proxy Api
apiProxy = Proxy
