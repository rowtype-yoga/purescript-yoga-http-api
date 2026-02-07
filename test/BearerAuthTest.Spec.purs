module BearerAuthTest.Spec where

import Prelude

import Data.String as String
import Effect (Effect)
import Yoga.HTTP.API.Path (Path, Lit)
import Yoga.HTTP.API.Route (GET, Route, Request, BearerToken, buildOpenAPISpec)
import Yoga.JSON (writeJSON)
import ViTest (ViTest, describe, test)
import ViTest.Expect (expectToBe)

--------------------------------------------------------------------------------
-- Test API with BearerToken
--------------------------------------------------------------------------------

type AuthAPI =
  { getProtected :: Route GET (Path (Lit "protected")) (Request { headers :: { authorization :: BearerToken } }) (ok :: { body :: { message :: String } })
  }

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

testBearerTokenNotInHeaders :: Effect ViTest
testBearerTokenNotInHeaders = describe "BearerToken in OpenAPI" do
  _ <- test "BearerToken does not appear as a regular header parameter" do
    let spec = buildOpenAPISpec @AuthAPI { title: "Auth API", version: "1.0.0" }
    let json = writeJSON spec
    -- Authorization should NOT appear in parameters array
    let hasAuthParam = String.contains (String.Pattern "\"name\":\"authorization\"") json
    expectToBe false hasAuthParam

  _ <- test "BearerToken appears in security requirements" do
    let spec = buildOpenAPISpec @AuthAPI { title: "Auth API", version: "1.0.0" }
    let json = writeJSON spec
    -- Should contain bearerAuth security reference
    let hasBearerAuth = String.contains (String.Pattern "bearerAuth") json
    expectToBe true hasBearerAuth

  test "OpenAPI spec contains securitySchemes component" do
    let spec = buildOpenAPISpec @AuthAPI { title: "Auth API", version: "1.0.0" }
    let json = writeJSON spec
    -- Should have bearerAuth defined in components.securitySchemes
    let hasSecuritySchemes = String.contains (String.Pattern "securitySchemes") json
    expectToBe true hasSecuritySchemes
