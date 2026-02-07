-- EXPECT: Could not match type
module Test.CompileFail.SchemaNameCollision where

import Data.Maybe (Maybe(..))
import Yoga.HTTP.API.Path (Path, Lit)
import Yoga.HTTP.API.Route.Encoding (JSON)
import Yoga.HTTP.API.Route.Handler (Request)
import Yoga.HTTP.API.Route.Method (GET, POST)
import Yoga.HTTP.API.Route.OpenAPIMetadata (Schema)
import Yoga.HTTP.API.Route.OpenAPI (buildOpenAPISpec, OpenAPISpec)
import Yoga.HTTP.API.Route.Route (Route)

type UserV1 = { id :: Int, name :: String }
type UserV2 = { id :: Int, email :: String, age :: Int }

type CollisionAPI =
  { getUser :: Route GET (Path (Lit "users")) (Request {}) (ok :: { body :: Schema "User" UserV1 })
  , createUser :: Route POST (Path (Lit "users")) (Request { body :: JSON (Schema "User" UserV2) }) (created :: { body :: Schema "User" UserV2 })
  }

spec :: OpenAPISpec
spec = buildOpenAPISpec @CollisionAPI { title: "Test", version: "1.0.0", description: Nothing, contact: Nothing, license: Nothing }
