{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
module RIOThrowAll where

import           RIO                            ( RIO ) -- rio
import qualified RIO
import           Data.Tagged                    ( Tagged(..) ) -- package tagged
import           Servant                        ( (:<|>)(..)
                                                , ServerError(..)
                                                )
import           Network.HTTP.Types             ( mkStatus ) -- package http-types
import           Network.Wai                    ( Application
                                                , responseLBS
                                                ) -- package wai
import qualified Data.ByteString.Char8         as BS

class RIOThrowAll a where
    rioThrowAll :: ServerError -> a

-- for a composition of endpoints
instance (RIOThrowAll a, RIOThrowAll b) => RIOThrowAll (a :<|> b) where
  rioThrowAll e = rioThrowAll e :<|> rioThrowAll e

-- if we have a function, we ignore the argument and delegate on the result
instance (RIOThrowAll b) => RIOThrowAll (a -> b) where
  rioThrowAll e = \_ -> rioThrowAll e

-- if we reach a RIO action at the tip of a function
instance RIOThrowAll (RIO.RIO env x) where
  rioThrowAll e = RIO.throwIO e

-- this is only for Raw endpoints which embed a WAI app directly
instance RIOThrowAll (Tagged (RIO.RIO env x) Application) where
  rioThrowAll e = Tagged $ \_req respond -> respond $ responseLBS
    (mkStatus (errHTTPCode e) (BS.pack $ errReasonPhrase e))
    (errHeaders e)
    (errBody e)
