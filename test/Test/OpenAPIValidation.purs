module Test.OpenAPIValidation (validate) where

import Foreign (Foreign)

foreign import validateImpl :: Foreign -> { errors :: Array { message :: String } }

validate :: Foreign -> { errors :: Array { message :: String } }
validate = validateImpl
