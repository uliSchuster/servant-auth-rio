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
import qualified Network.HTTP.Client           as HC
import qualified Network.HTTP.Types.Header     as HT
import qualified Api

-- Let Servant generate HTTP clients.
loginClient :: Api.Login -> SC.ClientM Api.CookieHeader
unprotectedGet :: SC.ClientM Text
unprotectedDelete :: SC.ClientM SV.NoContent
-- Not used but needed to satisfy the type checker (AC.Token)
protectedClient :: AC.Token -> SC.ClientM Text :<|> SC.ClientM SV.NoContent
loginClient :<|> (unprotectedGet :<|> unprotectedDelete) :<|> protectedClient =
  SC.client Api.apiProxy

-- Generate a client for the protected endpoint without the need to supply a JWT token.
protectedGet :: SC.ClientM Text
protectedDelete :: SC.ClientM SV.NoContent
protectedGet :<|> protectedDelete =
  SC.client (Proxy :: Proxy Api.ProtectedEndpoint)


spec_unprotected :: Spec
spec_unprotected = around TU.withClient $ do
  -- `around` starts the Wai Server before the tests and shuts it down afterwards.
  describe "unprotected" $ do
    it "GET the expected string" $ \clientEnv -> do
      TU.tryRequest clientEnv unprotectedGet
        `shouldReturn` "This is unprotected"
    it "DELETE success" $ \clientEnv -> do
      TU.tryRequest clientEnv unprotectedDelete `shouldReturn` SV.NoContent

spec_login :: Spec
spec_login = around TU.withClient $ do
  -- `around` starts the Wai Server before the tests and shuts it down afterwards.
  describe "login" $ do
    it "should login the known user" $ \clientEnv -> do
      SV.Headers a b <- TU.tryRequest
        clientEnv
        (loginClient (Api.Login "uliSchuster" "uli"))
      a `shouldBe` SV.NoContent
      let headers = SV.getHeaders b
      length headers `shouldBe` 2
      let (hName1, _) : (hName2, _) : _ = headers
      hName1 `shouldBe` HT.hSetCookie
      hName2 `shouldBe` HT.hSetCookie
    it "login should set authentication and CSRF-cookies" $ \clientEnv -> do
      _ <- SC.runClientM (loginClient (Api.Login "uliSchuster" "uli")) clientEnv
      let mcj = SC.cookieJar clientEnv
      case mcj of
        Nothing   -> expectationFailure "No cookieJar in clientEnv"
        Just cjtv -> do
          cj <- readTVarIO cjtv
          let cookies = HC.destroyCookieJar cj
          cookies `shouldSatisfy` (not . null)
    it "should not login an unknown user" $ \clientEnv -> do
      result <- SC.runClientM (loginClient (Api.Login "unknown" "uli"))
                              clientEnv
      isLeft result `shouldBe` True
      let mcj = SC.cookieJar clientEnv
      case mcj of
        Nothing   -> expectationFailure "No cookieJar in clientEnv"
        Just cjtv -> do
          cj <- readTVarIO cjtv
          let cookies = HC.destroyCookieJar cj
          cookies `shouldSatisfy` null
    it "should not login a user with incorrect password" $ \clientEnv -> do
      result <- SC.runClientM
        (loginClient (Api.Login "uliSchuster" "incorrect"))
        clientEnv
      isLeft result `shouldBe` True

spec_auth :: Spec
spec_auth = around (TU.withAuthentication "uliSchuster" "uli") $ do
  describe "/protected" $ do
    it "should obtain a success response" $ \clientEnv -> do
      result <- SC.runClientM protectedGet clientEnv
      isRight result `shouldBe` True
    it "should return the expected text" $ \clientEnv -> do
      TU.tryRequest clientEnv protectedGet `shouldReturn` "Authenticated"

-- Just to make sure that autodiscovery of test cases works.
spec_experiment :: Spec
spec_experiment = describe "read" $ do
  it "can parse integers" $ do
    (10 :: Int) `shouldBe` (10 :: Int)
