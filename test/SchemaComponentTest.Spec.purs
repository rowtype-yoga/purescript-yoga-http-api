module SchemaComponentTest.Spec where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String as String
import Effect (Effect)
import Yoga.HTTP.API.Path (Path, Lit, type (:), type (/))
import Yoga.HTTP.API.Route (GET, POST, Route, Request, JSON, Schema, buildOpenAPISpec)
import Yoga.JSON (writeJSON)
import ViTest (ViTest, describe, test)
import ViTest.Expect (expectToBe)

type User = { id :: Int, name :: String }
type Post = { id :: Int, title :: String, authorId :: Int }

type SchemaAPI =
  { getUser ::
      Route GET ("users" / "id" : Int)
        (Request {})
        (ok :: { body :: Schema "User" User })
  , createUser ::
      Route POST "users"
        (Request { body :: JSON (Schema "User" User) })
        (created :: { body :: Schema "User" User })
  , listPosts :: Route GET (Path (Lit "posts")) (Request {}) (ok :: { body :: Array (Schema "Post" Post) })
  }

testSchemaComponents :: Effect ViTest
testSchemaComponents = describe "Schema Components" do
  _ <- test "generates $ref in response body" do
    let
      spec = buildOpenAPISpec @SchemaAPI { title: "Test", version: "1.0.0", description: Nothing, contact: Nothing, license: Nothing }
      json = writeJSON spec
    expectToBe true (String.contains (String.Pattern "\"$ref\":\"#/components/schemas/User\"") json)

  _ <- test "generates schema definition in components" do
    let
      spec = buildOpenAPISpec @SchemaAPI { title: "Test", version: "1.0.0", description: Nothing, contact: Nothing, license: Nothing }
      json = writeJSON spec
    expectToBe true (String.contains (String.Pattern "components") json)
    expectToBe true (String.contains (String.Pattern "\"schemas\"") json)

  _ <- test "defines User schema with properties" do
    let
      spec = buildOpenAPISpec @SchemaAPI { title: "Test", version: "1.0.0", description: Nothing, contact: Nothing, license: Nothing }
      json = writeJSON spec
    -- Schema should be in components, not inline
    expectToBe true (String.contains (String.Pattern "\"User\"") json)
    expectToBe true (String.contains (String.Pattern "\"id\"") json)
    expectToBe true (String.contains (String.Pattern "\"name\"") json)

  test "collects nested schemas from arrays" do
    let
      spec = buildOpenAPISpec @SchemaAPI { title: "Test", version: "1.0.0", description: Nothing, contact: Nothing, license: Nothing }
      json = writeJSON spec
    expectToBe true (String.contains (String.Pattern "\"Post\"") json)
