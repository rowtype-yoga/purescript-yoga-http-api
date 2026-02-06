-- EXPECT: No type class instance was found
module Test.CompileFail.InvalidResponseRecordStructure where

import Prelude
import Yoga.HTTP.API.Path (Path, Lit)
import Yoga.HTTP.API.Route.Handler (Request)
import Yoga.HTTP.API.Route.Method (GET)
import Yoga.HTTP.API.Route.Route (Route)
import Yoga.HTTP.API.Route.RouteHandler (Handler, mkHandler)
import Yoga.HTTP.API.Route.Response (respondNoHeaders)

-- Response record has "status" field which is not "body" or "headers"
type MyRoute = Route GET (Path (Lit "health")) (Request {}) (ok :: { body :: String, status :: Int })

test :: Handler MyRoute
test = mkHandler \_ ->
  pure (respondNoHeaders @"ok" "healthy")
