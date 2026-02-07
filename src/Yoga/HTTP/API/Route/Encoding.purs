module Yoga.HTTP.API.Route.Encoding
  ( JSON
  , FormData
  , MultipartFormData
  , PlainText
  , XML
  , CustomContentType
  , NoBody
  ) where

-- | JSON-encoded request body (application/json)
-- |
-- | Example:
-- |   Request { body :: JSON User }
data JSON :: Type -> Type
data JSON a

-- | Form data encoded request body (application/x-www-form-urlencoded)
-- |
-- | Example:
-- |   Request { body :: FormData { username :: String, password :: String } }
data FormData :: Type -> Type
data FormData a

-- | Multipart form data encoded request body (multipart/form-data)
-- |
-- | Example:
-- |   Request { body :: MultipartFormData { file :: FileUpload } }
data MultipartFormData :: Type -> Type
data MultipartFormData a

-- | Plain text request/response body (text/plain)
-- |
-- | Example:
-- |   Request { body :: PlainText String }
data PlainText :: Type -> Type
data PlainText a

-- | XML encoded request/response body (application/xml)
-- |
-- | Example:
-- |   Request { body :: XML XmlDocument }
data XML :: Type -> Type
data XML a

-- | Custom content type with explicit MIME type
-- |
-- | Example:
-- |   Request { body :: CustomContentType "application/vnd.api+json" User }
data CustomContentType :: Symbol -> Type -> Type
data CustomContentType mime a

-- | No request body (for GET, DELETE, etc.)
-- |
-- | Example:
-- |   Request {}  -- NoBody is the default when body is omitted
data NoBody
