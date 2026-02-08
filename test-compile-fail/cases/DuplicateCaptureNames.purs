-- EXPECT: Could not match type
module Test.CompileFail.DuplicateCaptureNames where

import Prelude
import Data.Maybe (Maybe)
import Data.Variant (Variant)
import Effect.Aff (Aff)
import Yoga.HTTP.API.Path (Path, Lit, Capture, type (/), type (:))
import Yoga.HTTP.API.Route.Encoding (NoBody)
import Yoga.HTTP.API.Route.Handler
import Yoga.HTTP.API.Route.Method (GET)
import Yoga.HTTP.API.Route.Response (Response, respondNoHeaders)
import Yoga.HTTP.API.Route.Route (Route)
import Yoga.HTTP.API.Route.RouteHandler (Handler, mkHandler)

-- Two captures both named "id" with different types
type MyRoute = Route GET (Path (Capture "id" Int / Capture "id" String)) {} (ok :: { body :: String })

test :: Handler MyRoute
test = mkHandler \{ path: { id } } ->
  pure (respondNoHeaders @"ok" id)
