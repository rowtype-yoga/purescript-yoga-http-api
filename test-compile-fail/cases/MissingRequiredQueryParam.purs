-- EXPECT: Could not match kind
module Test.CompileFail.MissingRequiredQueryParam where

import Prelude
import Data.Variant (Variant)
import Effect.Aff (Aff)
import Yoga.HTTP.API.Path (Path, Lit, QueryParams, type (:?))
import Yoga.HTTP.API.Route.Handler (Request)
import Yoga.HTTP.API.Route.Method (GET)
import Yoga.HTTP.API.Route.Response (Response, respondNoHeaders)
import Yoga.HTTP.API.Route.Route (Route)
import Yoga.HTTP.API.Route.RouteHandler (Handler, mkHandler)

-- Query param "limit" is optional (no Required wrapper), so handler gets Maybe Int
-- But handler tries to use it as plain Int
type MyRoute = Route GET (Path (Lit "items") :? (limit :: Int)) (Request {}) (ok :: { body :: String })

test :: Handler MyRoute
test = mkHandler \{ query: { limit } } ->
  pure (respondNoHeaders @"ok" (show (limit :: Int)))
