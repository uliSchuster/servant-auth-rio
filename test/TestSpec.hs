{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators #-}

module TestSpec where

import           RIO
import           Test.Hspec
import qualified TestUtils                     as TU
import           Servant                        ( (:<|>)(..) )
import qualified Servant                       as SV
import qualified Servant.Client                as SC
import qualified Servant.Auth.Client           as AC
import qualified Api

-- Let Servant generate HTTP clients.
loginClient :: Api.Login -> SC.ClientM Api.CookieHeader
unprotectedClient :: SC.ClientM Text :<|> SC.ClientM SV.NoContent
protectedClient :: AC.Token -> SC.ClientM Text :<|> SC.ClientM SV.NoContent
loginClient :<|> unprotectedClient :<|> protectedClient =
  SC.client Api.apiProxy

spec_login :: Spec
spec_login = around TU.withClient $ do
  -- `around` starts the Wai Server before the tests and shuts it down afterwards.

  -- Testing scenarios start here
  describe "login" $ do
    it "should login the known user" $ \clientEnv -> do
      result <- SC.runClientM (loginClient (Api.Login "uliSchuster" "uli"))
                              clientEnv
      isRight result `shouldBe` True
    it "should not login an unknown user" $ \clientEnv -> do
      result <- SC.runClientM (loginClient (Api.Login "unknown" "uli"))
                              clientEnv
      isLeft result `shouldBe` True
    it "should not login a user with incorrect password" $ \clientEnv -> do
      result <- SC.runClientM
        (loginClient (Api.Login "uliSchuster" "incorrect"))
        clientEnv
      isLeft result `shouldBe` True

-- Just to make sure that autodiscovery of test cases works.
spec_experiment :: Spec
spec_experiment = describe "read" $ do
  it "can parse integers" $ do
    (10 :: Int) `shouldBe` (10 :: Int)
