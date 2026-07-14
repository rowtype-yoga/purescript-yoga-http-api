-- EXPECT: StatusCodeToLabel
module Test.CompileFail.InvalidNumericStatusCode where

import Data.Variant (Variant)
import Yoga.HTTP.API.Route.Response (Response, respondStatus)

-- Numeric response constructors accept only registered HTTP status codes.
test :: Variant (ok :: Response () String)
test = respondStatus @299 "not registered"
