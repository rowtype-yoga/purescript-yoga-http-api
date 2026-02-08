module Test.OpenAPIInfoTest where

import Prelude

import Data.String as String
import Effect (Effect)
import Yoga.HTTP.API.Route (GET, Route, buildOpenAPISpec)
import Yoga.JSON (writeJSON)
import ViTest (ViTest, describe, test)
import ViTest.Expect (expectToBe)

type SimpleAPI =
  { getRoot :: Route GET "" {} (ok :: { body :: { message :: String } })
  }

testInfoDescription :: Effect ViTest
testInfoDescription = describe "OpenAPI Info - Description" do
  _ <- test "includes description when provided" do
    let
      spec = buildOpenAPISpec @SimpleAPI
        { title: "Test API"
        , version: "1.0.0"
        , description: "This is a test API for demonstrating OpenAPI info enhancements"
        }
      json = writeJSON spec
    expectToBe true (String.contains (String.Pattern "\"description\":\"This is a test API for demonstrating OpenAPI info enhancements\"") json)

  test "omits description when Nothing" do
    let
      spec = buildOpenAPISpec @SimpleAPI
        { title: "Test API"
        , version: "1.0.0"
        }
      json = writeJSON spec
    -- Check that description is not in the info object (it will appear in responses, which is required by OpenAPI)
    let hasInfoDescription = String.contains (String.Pattern "\"info\":{\"title\":\"Test API\",\"version\":\"1.0.0\",\"description\"") json
    expectToBe false hasInfoDescription

testInfoContact :: Effect ViTest
testInfoContact = describe "OpenAPI Info - Contact" do
  _ <- test "includes contact with all fields" do
    let
      spec = buildOpenAPISpec @SimpleAPI
        { title: "Test API"
        , version: "1.0.0"
        , contact:
            { name: "API Support"
            , url: "https://api.example.com/support"
            , email: "support@example.com"
            }
        }
      json = writeJSON spec
    expectToBe true (String.contains (String.Pattern "\"contact\"") json)
    expectToBe true (String.contains (String.Pattern "\"name\":\"API Support\"") json)
    expectToBe true (String.contains (String.Pattern "\"url\":\"https://api.example.com/support\"") json)
    expectToBe true (String.contains (String.Pattern "\"email\":\"support@example.com\"") json)

  _ <- test "includes contact with only name" do
    let
      spec = buildOpenAPISpec @SimpleAPI
        { title: "Test API"
        , version: "1.0.0"
        , contact: { name: "API Team" }
        }
      json = writeJSON spec
    expectToBe true (String.contains (String.Pattern "\"contact\"") json)
    expectToBe true (String.contains (String.Pattern "\"name\":\"API Team\"") json)

  _ <- test "includes contact with only email" do
    let
      spec = buildOpenAPISpec @SimpleAPI
        { title: "Test API"
        , version: "1.0.0"
        , contact: { email: "info@example.com" }
        }
      json = writeJSON spec
    expectToBe true (String.contains (String.Pattern "\"contact\"") json)
    expectToBe true (String.contains (String.Pattern "\"email\":\"info@example.com\"") json)

  _ <- test "includes contact with name and url" do
    let
      spec = buildOpenAPISpec @SimpleAPI
        { title: "Test API"
        , version: "1.0.0"
        , contact:
            { name: "Support Team"
            , url: "https://support.example.com"
            }
        }
      json = writeJSON spec
    expectToBe true (String.contains (String.Pattern "\"contact\"") json)
    expectToBe true (String.contains (String.Pattern "\"name\":\"Support Team\"") json)
    expectToBe true (String.contains (String.Pattern "\"url\":\"https://support.example.com\"") json)

  test "omits contact when Nothing" do
    let
      spec = buildOpenAPISpec @SimpleAPI
        { title: "Test API"
        , version: "1.0.0"
        }
      json = writeJSON spec
    expectToBe false (String.contains (String.Pattern "\"contact\"") json)

testInfoLicense :: Effect ViTest
testInfoLicense = describe "OpenAPI Info - License" do
  _ <- test "includes license with name only" do
    let
      spec = buildOpenAPISpec @SimpleAPI
        { title: "Test API"
        , version: "1.0.0"
        , license: { name: "MIT" }
        }
      json = writeJSON spec
    expectToBe true (String.contains (String.Pattern "\"license\"") json)
    expectToBe true (String.contains (String.Pattern "\"name\":\"MIT\"") json)

  _ <- test "includes license with name and url" do
    let
      spec = buildOpenAPISpec @SimpleAPI
        { title: "Test API"
        , version: "1.0.0"
        , license:
            { name: "Apache 2.0"
            , url: "https://www.apache.org/licenses/LICENSE-2.0.html"
            }
        }
      json = writeJSON spec
    expectToBe true (String.contains (String.Pattern "\"license\"") json)
    expectToBe true (String.contains (String.Pattern "\"name\":\"Apache 2.0\"") json)
    expectToBe true (String.contains (String.Pattern "\"url\":\"https://www.apache.org/licenses/LICENSE-2.0.html\"") json)

  test "omits license when Nothing" do
    let
      spec = buildOpenAPISpec @SimpleAPI
        { title: "Test API"
        , version: "1.0.0"
        }
      json = writeJSON spec
    expectToBe false (String.contains (String.Pattern "\"license\"") json)

testInfoAllFields :: Effect ViTest
testInfoAllFields = describe "OpenAPI Info - All Fields Together" do
  test "includes all info fields when provided" do
    let
      spec = buildOpenAPISpec @SimpleAPI
        { title: "Complete API"
        , version: "2.0.0"
        , description: "A comprehensive API with full metadata"
        , contact:
            { name: "API Team"
            , url: "https://example.com/contact"
            , email: "api@example.com"
            }
        , license:
            { name: "MIT"
            , url: "https://opensource.org/licenses/MIT"
            }
        }
      json = writeJSON spec
    expectToBe true (String.contains (String.Pattern "\"title\":\"Complete API\"") json)
    expectToBe true (String.contains (String.Pattern "\"version\":\"2.0.0\"") json)
    expectToBe true (String.contains (String.Pattern "\"description\":\"A comprehensive API with full metadata\"") json)
    expectToBe true (String.contains (String.Pattern "\"contact\"") json)
    expectToBe true (String.contains (String.Pattern "\"license\"") json)
    expectToBe true (String.contains (String.Pattern "\"name\":\"API Team\"") json)
    expectToBe true (String.contains (String.Pattern "\"email\":\"api@example.com\"") json)
    expectToBe true (String.contains (String.Pattern "\"name\":\"MIT\"") json)
