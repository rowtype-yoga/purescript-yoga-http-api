module Test.OpenAPIValidation (validate) where

import Foreign (Foreign)
import Yoga.HTTP.API.Route.OpenAPI (OpenAPISpec)
import Yoga.JSON (writeImpl)

foreign import validateImpl :: Foreign -> { errors :: Array { message :: String } }

validate :: OpenAPISpec -> { errors :: Array { message :: String } }
validate spec = validateImpl (writeImpl spec)
