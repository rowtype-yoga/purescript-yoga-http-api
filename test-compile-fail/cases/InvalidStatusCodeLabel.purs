-- EXPECT: Unknown status code
module Test.CompileFail.InvalidStatusCodeLabel where

import Type.Proxy (Proxy(..))
import Yoga.HTTP.API.Route.StatusCode (class StatusCodeMap, statusCodeFor, StatusCode)

-- "banana" is not a valid HTTP status code label
test :: StatusCode
test = statusCodeFor (Proxy :: Proxy "banana")
