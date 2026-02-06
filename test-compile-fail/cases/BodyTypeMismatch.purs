-- EXPECT: Could not match type
module Test.CompileFail.BodyTypeMismatch where

import Prelude
import Data.Variant (Variant)
import Effect.Aff (Aff)
import Yoga.HTTP.API.Path (Path, Lit)
import Yoga.HTTP.API.Route.Handler (Request)
import Yoga.HTTP.API.Route.Method (GET)
import Yoga.HTTP.API.Route.Response (Response, respondNoHeaders)
import Yoga.HTTP.API.Route.Route (Route)
import Yoga.HTTP.API.Route.RouteHandler (Handler, mkHandler)

-- Route has no body (defaults to NoBody/Unit), but handler uses body as String
type MyRoute = Route GET (Path (Lit "health")) (Request {}) (ok :: { body :: String })

test :: Handler MyRoute
test = mkHandler \{ body } ->
  pure (respondNoHeaders @"ok" (body :: String))
