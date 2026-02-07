module Test.OpenAPIValidation (validate) where

import Prelude
import Foreign (Foreign)
import Yoga.HTTP.API.Route.OpenAPI (OpenAPISpec)
import Yoga.JSON (write)

foreign import validateImpl :: Foreign -> { errors :: Array { message :: String } }

validate :: OpenAPISpec -> { errors :: Array { message :: String } }
validate = validateImpl <<< write
