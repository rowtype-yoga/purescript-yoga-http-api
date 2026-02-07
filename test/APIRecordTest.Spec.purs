module APIRecordTest.Spec where

import Prelude

import Data.Array as Array
import Data.String as String
import Effect (Effect)
import Effect.Aff (Aff)
import Type.Proxy (Proxy(..))
import Yoga.HTTP.API.Path (Path, Lit, type (/), type (:))
import Yoga.HTTP.API.Route (GET, POST, Route, Request, JSON, respondNoHeaders, buildOpenAPISpec, collectOperations, mkHandler, apiHandlers)
import Yoga.JSON (writeJSON)
import ViTest (ViTest, describe, test)
import ViTest.Expect (expectToBe)

expectToEqual :: forall a. Eq a => a -> a -> Aff Unit
expectToEqual expected actual = expectToBe true (expected == actual)

--------------------------------------------------------------------------------
-- Test API Definition
--------------------------------------------------------------------------------

type User = { id :: Int, name :: String }

type TestAPI =
  { getUser :: Route GET (Path ("users" / "id" : Int)) (Request {}) (ok :: { body :: User })
  , listUsers :: Route GET (Path (Lit "users")) (Request {}) (ok :: { body :: Array User })
  , createUser :: Route POST (Path (Lit "users")) (Request { body :: JSON User }) (created :: { body :: User })
  }

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

testCollectOperations :: Effect ViTest
testCollectOperations = describe "API Record - CollectOperations" do
  _ <- test "collects correct number of operations" do
    let ops = collectOperations (Proxy :: _ TestAPI)
    expectToEqual 3 (Array.length ops)

  _ <- test "contains GET method" do
    let ops = collectOperations (Proxy :: _ TestAPI)
    let methods = ops <#> _.method
    expectToBe true (Array.elem "get" methods)

  test "contains POST method" do
    let ops = collectOperations (Proxy :: _ TestAPI)
    let methods = ops <#> _.method
    expectToBe true (Array.elem "post" methods)

testOperationIds :: Effect ViTest
testOperationIds = describe "API Record - OperationId" do
  _ <- test "injects getUser operationId" do
    let ops = collectOperations (Proxy :: _ TestAPI)
    let
      hasGetUser = ops # Array.any \op ->
        String.contains (String.Pattern "\"operationId\":\"getUser\"") op.operation
    expectToBe true hasGetUser

  _ <- test "injects listUsers operationId" do
    let ops = collectOperations (Proxy :: _ TestAPI)
    let
      hasListUsers = ops # Array.any \op ->
        String.contains (String.Pattern "\"operationId\":\"listUsers\"") op.operation
    expectToBe true hasListUsers

  test "injects createUser operationId" do
    let ops = collectOperations (Proxy :: _ TestAPI)
    let
      hasCreateUser = ops # Array.any \op ->
        String.contains (String.Pattern "\"operationId\":\"createUser\"") op.operation
    expectToBe true hasCreateUser

testBuildOpenAPISpec :: Effect ViTest
testBuildOpenAPISpec = describe "API Record - buildOpenAPISpec" do
  _ <- test "generates valid OpenAPI spec with operationIds" do
    let spec = buildOpenAPISpec @TestAPI { title: "Test API", version: "1.0.0" }
    let json = writeJSON spec
    expectToBe true (String.contains (String.Pattern "\"operationId\":\"getUser\"") json)
    expectToBe true (String.contains (String.Pattern "\"operationId\":\"listUsers\"") json)
    expectToBe true (String.contains (String.Pattern "\"operationId\":\"createUser\"") json)

  _ <- test "contains paths" do
    let spec = buildOpenAPISpec @TestAPI { title: "Test API", version: "1.0.0" }
    let json = writeJSON spec
    expectToBe true (String.contains (String.Pattern "/users/{id}") json)
    expectToBe true (String.contains (String.Pattern "/users") json)

  test "contains API info" do
    let spec = buildOpenAPISpec @TestAPI { title: "Test API", version: "1.0.0" }
    let json = writeJSON spec
    expectToBe true (String.contains (String.Pattern "Test API") json)
    expectToBe true (String.contains (String.Pattern "1.0.0") json)

testAPIHandlers :: Effect ViTest
testAPIHandlers = describe "API Record - apiHandlers" do
  test "compiles with correct handler record" do
    let
      _handlers = apiHandlers @TestAPI
        { getUser: mkHandler \{ path } -> pure $ respondNoHeaders @"ok" { id: path.id, name: "Alice" }
        , listUsers: mkHandler \_ -> pure $ respondNoHeaders @"ok" []
        , createUser: mkHandler \{ body } -> pure $ respondNoHeaders @"created" body
        }
    expectToBe true true
