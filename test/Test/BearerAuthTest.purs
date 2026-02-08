module Test.BearerAuthTest where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String as String
import Effect (Effect)
import Yoga.HTTP.API.Route (GET, Route, Request, buildOpenAPISpec)
import Yoga.HTTP.API.Route.Auth (BearerToken, BasicAuth, ApiKeyHeader, DigestAuth)
import Yoga.JSON (writeJSON)
import Test.OpenAPIValidation (validate)
import ViTest (ViTest, describe, test)
import ViTest.Expect (expectToBe)

--------------------------------------------------------------------------------
-- Test APIs with different authentication types
--------------------------------------------------------------------------------

type BearerAuthAPI =
  { getProtected :: Route GET "protected" (Request { headers :: { authorization :: BearerToken } }) (ok :: { body :: { message :: String } })
  }

type BasicAuthAPI =
  { getProtected :: Route GET "protected" (Request { headers :: { authorization :: BasicAuth } }) (ok :: { body :: { message :: String } })
  }

type ApiKeyAPI =
  { getProtected :: Route GET "protected" (Request { headers :: { apiKey :: ApiKeyHeader } }) (ok :: { body :: { message :: String } })
  }

type DigestAuthAPI =
  { getProtected :: Route GET "protected" (Request { headers :: { authorization :: DigestAuth } }) (ok :: { body :: { message :: String } })
  }

type MultiAuthAPI =
  { bearerEndpoint :: Route GET "bearer" (Request { headers :: { authorization :: BearerToken } }) (ok :: { body :: { message :: String } })
  , basicEndpoint :: Route GET "basic" (Request { headers :: { authorization :: BasicAuth } }) (ok :: { body :: { message :: String } })
  , apiKeyEndpoint :: Route GET "apikey" (Request { headers :: { xApiKey :: ApiKeyHeader } }) (ok :: { body :: { message :: String } })
  }

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

testBearerTokenAuth :: Effect ViTest
testBearerTokenAuth = describe "BearerToken Authentication" do
  _ <- test "BearerToken does not appear as a regular header parameter" do
    let spec = buildOpenAPISpec @BearerAuthAPI { title: "Auth API", version: "1.0.0", description: Nothing, contact: Nothing, license: Nothing }
    let json = writeJSON spec
    let hasAuthParam = String.contains (String.Pattern "\"name\":\"authorization\"") json
    expectToBe false hasAuthParam

  _ <- test "BearerToken appears in security requirements" do
    let spec = buildOpenAPISpec @BearerAuthAPI { title: "Auth API", version: "1.0.0", description: Nothing, contact: Nothing, license: Nothing }
    let json = writeJSON spec
    let hasBearerAuth = String.contains (String.Pattern "bearerAuth") json
    expectToBe true hasBearerAuth

  _ <- test "BearerToken security scheme has correct structure" do
    let spec = buildOpenAPISpec @BearerAuthAPI { title: "Auth API", version: "1.0.0", description: Nothing, contact: Nothing, license: Nothing }
    let json = writeJSON spec
    expectToBe true (String.contains (String.Pattern "\"type\":\"http\"") json)
    expectToBe true (String.contains (String.Pattern "\"scheme\":\"bearer\"") json)

  test "BearerToken API generates valid OpenAPI 3.0 schema" do
    let spec = buildOpenAPISpec @BearerAuthAPI { title: "Auth API", version: "1.0.0", description: Nothing, contact: Nothing, license: Nothing }
    let result = validate spec
    expectToBe true (Array.null result.errors)

testBasicAuth :: Effect ViTest
testBasicAuth = describe "BasicAuth Authentication" do
  _ <- test "BasicAuth does not appear as a regular header parameter" do
    let spec = buildOpenAPISpec @BasicAuthAPI { title: "Auth API", version: "1.0.0", description: Nothing, contact: Nothing, license: Nothing }
    let json = writeJSON spec
    let hasAuthParam = String.contains (String.Pattern "\"name\":\"authorization\"") json
    expectToBe false hasAuthParam

  _ <- test "BasicAuth appears in security requirements" do
    let spec = buildOpenAPISpec @BasicAuthAPI { title: "Auth API", version: "1.0.0", description: Nothing, contact: Nothing, license: Nothing }
    let json = writeJSON spec
    let hasBasicAuth = String.contains (String.Pattern "basicAuth") json
    expectToBe true hasBasicAuth

  test "BasicAuth security scheme has correct structure" do
    let spec = buildOpenAPISpec @BasicAuthAPI { title: "Auth API", version: "1.0.0", description: Nothing, contact: Nothing, license: Nothing }
    let json = writeJSON spec
    expectToBe true (String.contains (String.Pattern "\"type\":\"http\"") json)
    expectToBe true (String.contains (String.Pattern "\"scheme\":\"basic\"") json)

testApiKeyAuth :: Effect ViTest
testApiKeyAuth = describe "ApiKey Authentication" do
  _ <- test "ApiKeyHeader does not appear as a regular header parameter" do
    let spec = buildOpenAPISpec @ApiKeyAPI { title: "Auth API", version: "1.0.0", description: Nothing, contact: Nothing, license: Nothing }
    let json = writeJSON spec
    -- Check that parameters array is empty (no header parameters)
    let hasEmptyParams = String.contains (String.Pattern "\"parameters\":[]") json
    expectToBe true hasEmptyParams

  _ <- test "ApiKey appears in security requirements" do
    let spec = buildOpenAPISpec @ApiKeyAPI { title: "Auth API", version: "1.0.0", description: Nothing, contact: Nothing, license: Nothing }
    let json = writeJSON spec
    let hasApiKey = String.contains (String.Pattern "apiKeyApiKey") json
    expectToBe true hasApiKey

  test "ApiKey security scheme has correct structure" do
    let spec = buildOpenAPISpec @ApiKeyAPI { title: "Auth API", version: "1.0.0", description: Nothing, contact: Nothing, license: Nothing }
    let json = writeJSON spec
    expectToBe true (String.contains (String.Pattern "\"type\":\"apiKey\"") json)
    expectToBe true (String.contains (String.Pattern "\"in\":\"header\"") json)

testDigestAuth :: Effect ViTest
testDigestAuth = describe "DigestAuth Authentication" do
  _ <- test "DigestAuth does not appear as a regular header parameter" do
    let spec = buildOpenAPISpec @DigestAuthAPI { title: "Auth API", version: "1.0.0", description: Nothing, contact: Nothing, license: Nothing }
    let json = writeJSON spec
    let hasAuthParam = String.contains (String.Pattern "\"name\":\"authorization\"") json
    expectToBe false hasAuthParam

  _ <- test "DigestAuth appears in security requirements" do
    let spec = buildOpenAPISpec @DigestAuthAPI { title: "Auth API", version: "1.0.0", description: Nothing, contact: Nothing, license: Nothing }
    let json = writeJSON spec
    let hasDigestAuth = String.contains (String.Pattern "digestAuth") json
    expectToBe true hasDigestAuth

  test "DigestAuth security scheme has correct structure" do
    let spec = buildOpenAPISpec @DigestAuthAPI { title: "Auth API", version: "1.0.0", description: Nothing, contact: Nothing, license: Nothing }
    let json = writeJSON spec
    expectToBe true (String.contains (String.Pattern "\"type\":\"http\"") json)
    expectToBe true (String.contains (String.Pattern "\"scheme\":\"digest\"") json)

testMultipleAuthTypes :: Effect ViTest
testMultipleAuthTypes = describe "Multiple Authentication Types" do
  _ <- test "All auth types appear in security schemes" do
    let spec = buildOpenAPISpec @MultiAuthAPI { title: "Auth API", version: "1.0.0", description: Nothing, contact: Nothing, license: Nothing }
    let json = writeJSON spec
    expectToBe true (String.contains (String.Pattern "bearerAuth") json)
    expectToBe true (String.contains (String.Pattern "basicAuth") json)
    expectToBe true (String.contains (String.Pattern "xApiKeyApiKey") json)

  test "None of the auth headers appear as regular parameters" do
    let spec = buildOpenAPISpec @MultiAuthAPI { title: "Auth API", version: "1.0.0", description: Nothing, contact: Nothing, license: Nothing }
    let json = writeJSON spec
    -- Check that all endpoints have empty parameters arrays
    let hasEmptyParams = String.contains (String.Pattern "\"parameters\":[]") json
    expectToBe true hasEmptyParams
