module Test.CookieAuthTest where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String as String
import Effect (Effect)
import Yoga.HTTP.API.Route (GET, Route, buildOpenAPISpec)
import Yoga.HTTP.API.Route.Auth (ApiKeyCookie)
import Yoga.JSON (writeJSON)
import ViTest (ViTest, describe, test)
import ViTest.Expect (expectToBe)

--------------------------------------------------------------------------------
-- Test APIs with cookie authentication and parameters
--------------------------------------------------------------------------------

type ApiKeyCookieAPI =
  { getProtected :: Route GET "protected" { cookies :: { sessionId :: ApiKeyCookie } } (ok :: { body :: { message :: String } })
  }

type RegularCookieParamsAPI =
  { getData :: Route GET "data" { cookies :: { preference :: String, theme :: String } } (ok :: { body :: { data :: String } })
  }

type MixedCookieAPI =
  { getResource :: Route GET "resource" { cookies :: { sessionId :: ApiKeyCookie, userId :: String } } (ok :: { body :: { resource :: String } })
  }

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

testApiKeyCookieAuth :: Effect ViTest
testApiKeyCookieAuth = describe "ApiKeyCookie Authentication" do
  _ <- test "ApiKeyCookie does not appear as a regular cookie parameter" do
    let spec = buildOpenAPISpec @ApiKeyCookieAPI { title: "Cookie Auth API", version: "1.0.0" }
    let json = writeJSON spec
    -- Check that parameters array is empty (no cookie parameters)
    let hasEmptyParams = String.contains (String.Pattern "\"parameters\":[]") json
    expectToBe true hasEmptyParams

  _ <- test "ApiKeyCookie appears in security requirements" do
    let spec = buildOpenAPISpec @ApiKeyCookieAPI { title: "Cookie Auth API", version: "1.0.0" }
    let json = writeJSON spec
    let hasSessionIdCookie = String.contains (String.Pattern "sessionIdCookie") json
    expectToBe true hasSessionIdCookie

  test "ApiKeyCookie security scheme has correct structure" do
    let spec = buildOpenAPISpec @ApiKeyCookieAPI { title: "Cookie Auth API", version: "1.0.0" }
    let json = writeJSON spec
    expectToBe true (String.contains (String.Pattern "\"type\":\"apiKey\"") json)
    expectToBe true (String.contains (String.Pattern "\"in\":\"cookie\"") json)
    expectToBe true (String.contains (String.Pattern "\"name\":\"sessionId\"") json)

testRegularCookieParams :: Effect ViTest
testRegularCookieParams = describe "Regular Cookie Parameters" do
  _ <- test "Regular cookies appear as cookie parameters" do
    let spec = buildOpenAPISpec @RegularCookieParamsAPI { title: "Cookie Params API", version: "1.0.0" }
    let json = writeJSON spec
    let hasPreferenceParam = String.contains (String.Pattern "\"name\":\"preference\"") json && String.contains (String.Pattern "\"in\":\"cookie\"") json
    let hasThemeParam = String.contains (String.Pattern "\"name\":\"theme\"") json && String.contains (String.Pattern "\"in\":\"cookie\"") json
    expectToBe true hasPreferenceParam
    expectToBe true hasThemeParam

  test "Regular cookies do not appear in security schemes" do
    let spec = buildOpenAPISpec @RegularCookieParamsAPI { title: "Cookie Params API", version: "1.0.0" }
    let json = writeJSON spec
    let hasPreferenceCookie = String.contains (String.Pattern "preferenceCookie") json && String.contains (String.Pattern "securitySchemes") json
    let hasThemeCookie = String.contains (String.Pattern "themeCookie") json && String.contains (String.Pattern "securitySchemes") json
    expectToBe false hasPreferenceCookie
    expectToBe false hasThemeCookie

testMixedCookies :: Effect ViTest
testMixedCookies = describe "Mixed Cookie Types (Auth + Regular)" do
  _ <- test "Auth cookie appears in security schemes" do
    let spec = buildOpenAPISpec @MixedCookieAPI { title: "Mixed Cookie API", version: "1.0.0" }
    let json = writeJSON spec
    let hasSessionIdAuth = String.contains (String.Pattern "sessionIdCookie") json
    expectToBe true hasSessionIdAuth

  _ <- test "Auth cookie does not appear as regular parameter" do
    let spec = buildOpenAPISpec @MixedCookieAPI { title: "Mixed Cookie API", version: "1.0.0" }
    let json = writeJSON spec
    -- sessionId should not appear as a parameter (only userId should)
    -- Check that the JSON doesn't contain a parameter object for sessionId
    let hasSessionIdParam = String.contains (String.Pattern "{\"name\":\"sessionId\",\"in\":\"cookie\"") json
    expectToBe false hasSessionIdParam

  test "Regular cookie appears as cookie parameter" do
    let spec = buildOpenAPISpec @MixedCookieAPI { title: "Mixed Cookie API", version: "1.0.0" }
    let json = writeJSON spec
    let hasUserIdParam = String.contains (String.Pattern "\"name\":\"userId\"") json && String.contains (String.Pattern "\"in\":\"cookie\"") json
    expectToBe true hasUserIdParam
