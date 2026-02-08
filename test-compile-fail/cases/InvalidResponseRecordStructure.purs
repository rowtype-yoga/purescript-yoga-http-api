-- EXPECT: Invalid response record structure
module Test.CompileFail.InvalidResponseRecordStructure where

import Prelude
import Yoga.HTTP.API.Path (Path, Lit)
import Yoga.HTTP.API.Route.Handler
import Yoga.HTTP.API.Route.Method (GET)
import Yoga.HTTP.API.Route.Route (Route)
import Yoga.HTTP.API.Route.RouteHandler (Handler, mkHandler)
import Yoga.HTTP.API.Route.Response (respondNoHeaders)

-- Response record has "status" field which is not "body" or "headers"
type MyRoute = Route GET (Path (Lit "health")) {} (ok :: { body :: String, status :: Int })

test :: Handler MyRoute
test = mkHandler \_ ->
  pure (respondNoHeaders @"ok" "healthy")
