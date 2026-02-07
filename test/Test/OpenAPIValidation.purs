module Test.OpenAPIValidation (validate) where

import Foreign (Foreign)
import Unsafe.Coerce (unsafeCoerce)
import Yoga.HTTP.API.Route.OpenAPI (OpenAPISpec)

foreign import validateImpl :: Foreign -> { errors :: Array { message :: String } }

validate :: OpenAPISpec -> { errors :: Array { message :: String } }
validate spec = validateImpl (unsafeCoerce spec)
