-- EXPECT: Could not match type
module Test.CompileFail.WrongCaptureType where

import Data.Maybe (Maybe)
import Type.Proxy (Proxy(..))
import Yoga.HTTP.API.Path (Path, Lit, Capture, type (/), type (:), class ParsePath, parsePath)

-- Boolean has no ParseParam instance, so parsePath should fail
type MyPath = Path (Lit "users" / Capture "valid" Boolean)

test :: Proxy MyPath -> String -> Maybe { valid :: Boolean }
test = parsePath
