-- EXPECT: Could not match type
module Test.CompileFail.WrongResponseVariantLabel where

import Prelude
import Data.Variant (Variant)
import Effect.Aff (Aff)
import Yoga.HTTP.API.Path (Path, Lit)
import Yoga.HTTP.API.Route.Handler
import Yoga.HTTP.API.Route.Method (GET)
import Yoga.HTTP.API.Route.Response (Response, respondNoHeaders)
import Yoga.HTTP.API.Route.Route (Route)
import Yoga.HTTP.API.Route.RouteHandler (Handler, mkHandler)

-- Route only has "ok" response, no "notFound"
type MyRoute = Route GET (Path (Lit "health")) {} (ok :: { body :: String })

test :: Handler MyRoute
test = mkHandler \_ ->
  pure (respondNoHeaders @"notFound" "oops")
