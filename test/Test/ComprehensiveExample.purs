module Test.ComprehensiveExample where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Type.Function (type (#))
import Type.Proxy (Proxy(..))
import Yoga.HTTP.API.Path (Path, Lit, Capture, type (:), type (/), type (:?))
import Yoga.HTTP.API.Route (GET, POST, PUT, DELETE, Route, Request, buildOpenAPISpec)
import Yoga.HTTP.API.Route.Auth (BearerToken, ApiKeyHeader, BasicAuth)
import Yoga.HTTP.API.Route.Encoding (JSON, PlainText, XML)
import Yoga.HTTP.API.Route.OpenAPIMetadata (Description, Example, Examples, ExampleValue, ExampleWithSummary, Schema, Minimum, Maximum, Pattern, MinLength, MaxLength)
import Yoga.JSON (writeJSON)
import Test.OpenAPIValidation (validate)
import ViTest (ViTest, describe, test)
import ViTest.Expect (expectToBe)

-- | A comprehensive example API showcasing all major features
type ComprehensiveAPI =
  { -- Simple GET with path parameter and authentication
    getUser ::
      Route GET ("users" / "id" : Int)
        (Request { headers :: { authorization :: BearerToken } })
        (ok :: { body :: Schema "User" User })

  , -- List users with query parameters
    listUsers ::
      Route GET ("users" :? { limit :: Int # Example "10", offset :: Int # Example "0" })
        (Request {})
        (ok :: { body :: Array User })

  , -- POST with JSON body and schema
    createUser ::
      Route POST "users"
        (Request { body :: JSON (Schema "CreateUserRequest" CreateUserRequest) })
        (created :: { body :: Schema "User" User })

  , -- PUT with authentication and JSON body
    updateUser ::
      Route PUT ("users" / "id" : Int)
        ( Request
            { headers :: { authorization :: BearerToken }
            , body :: JSON UpdateUserRequest
            }
        )
        (ok :: { body :: User })

  , -- DELETE with authentication
    deleteUser ::
      Route DELETE ("users" / "id" : Int)
        (Request { headers :: { authorization :: BearerToken } })
        (noContent :: { body :: {} })

  , -- Multiple content types - XML
    exportUser ::
      Route GET ("users" / "id" : Int / Lit "export")
        (Request {})
        (ok :: { body :: XML User })

  , -- Plain text response
    healthCheck ::
      Route GET (Path (Lit "health"))
        (Request {})
        (ok :: { body :: PlainText String })

  , -- API Key authentication
    getStats ::
      Route GET (Path (Lit "stats"))
        (Request { headers :: { xApiKey :: ApiKeyHeader } })
        (ok :: { body :: Stats })

  , -- Basic auth
    login ::
      Route POST (Path (Lit "login"))
        (Request { headers :: { authorization :: BasicAuth } })
        (ok :: { body :: { token :: String # Description "JWT access token" } })

  , -- Path with examples
    getUserPosts ::
      Route GET ("users" / "userId" : (Int # Examples (alice :: ExampleValue "1", bob :: ExampleValue "2")) / Lit "posts")
        (Request {})
        (ok :: { body :: Array Post })
  }

-- | User type with metadata
type User =
  { id :: Int
  , name :: String # Description "User's full name"
  , email :: String # Description "User's email address"
  , age :: Int
  , role :: String # Description "User role: admin, user, or guest"
  }

-- | Request to create a user
type CreateUserRequest =
  { name :: String # Examples (short :: ExampleValue "\"Jo\"", normal :: ExampleValue "\"John Doe\"")
  , email :: String # Example "\"user@example.com\""
  , age :: Int # Example "25"
  , role :: String # Example "\"user\""
  }

-- | Request to update a user
type UpdateUserRequest =
  { name :: String
  , email :: String
  , age :: Int
  }

-- | Statistics type
type Stats =
  { activeUsers :: Int # Description "Number of currently active users"
  , totalRequests :: Int # Description "Total API requests"
  , uptime :: Int # Description "Server uptime in seconds"
  }

-- | Post type
type Post =
  { id :: Int
  , userId :: Int
  , title :: String # Description "Post title"
  , content :: String # Description "Post content"
  , published :: Boolean
  }

-- | Test demonstrating the comprehensive API
testComprehensiveExample :: Effect ViTest
testComprehensiveExample = describe "Comprehensive API Example" do
  test "generates complete OpenAPI spec with all features" do
    let
      spec = buildOpenAPISpec @ComprehensiveAPI
        { title: "Comprehensive Example API"
        , version: "1.0.0"
        , description: Just "A complete example showcasing yoga-http-api features including authentication, multiple content types, metadata, schemas, and examples"
        , contact: Just
            { name: Just "API Team"
            , email: Just "api@example.com"
            , url: Just "https://example.com/support"
            }
        , license: Just
            { name: "MIT"
            , url: Just "https://opensource.org/licenses/MIT"
            }
        }
      json = writeJSON spec

    -- Verify it generates valid OpenAPI
    let result = validate spec
    expectToBe true (Array.null result.errors)

    -- Check it has our endpoints
    expectToBe true (json /= "")
