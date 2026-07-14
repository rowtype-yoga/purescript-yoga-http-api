module Yoga.HTTP.API.Route.Encoding
  ( JSON
  , FormData
  , MultipartFormData
  , MultipartFile
  , multipartFileBytes
  , PlainText
  , XML
  , CustomContentType
  , Streaming
  , StreamChunk
  , textChunk
  , binaryChunk
  , streaming
  , NoBody
  ) where

import Prelude

import Data.ArrayBuffer.Types (Uint8Array)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toNullable)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Foreign (unsafeToForeign, unsafeFromForeign)
import Promise (Promise)
import Promise.Aff as Promise
import Yoga.JSON (class ReadForeign, class WriteForeign)

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

-- | Opaque bytes for a file field produced by a multipart parser.
-- | The type prevents ordinary text fields and uploaded bytes being confused.
foreign import data MultipartFile :: Type

-- | Access uploaded bytes without copying.
foreign import multipartFileBytes :: MultipartFile -> Uint8Array

instance ReadForeign MultipartFile where
  readImpl value = pure (unsafeFromForeign value)

-- | Plain text request/response body (text/plain)
-- |
-- | Example:
-- |   { body :: PlainText }

data PlainText

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

-- | A chunk representation accepted by Node and Web byte streams.
foreign import data StreamChunk :: Type

-- | Construct a textual stream chunk.
foreign import textChunk :: String -> StreamChunk

-- | Construct a binary stream chunk without copying its Uint8Array.
foreign import binaryChunk :: Uint8Array -> StreamChunk

-- | A standards-based Web ReadableStream used as an HTTP response body.
-- | `pull` is called only when the consumer requests another chunk. Returning
-- | `Nothing` closes the stream; disconnecting the consumer runs `cancel`.
foreign import data Streaming :: Type -> Type

foreign import streamingImpl
  :: EffectFn2
       (Effect (Promise (Nullable StreamChunk)))
       (Effect Unit)
       (Streaming StreamChunk)

streaming
  :: { pull :: Aff (Maybe StreamChunk), cancel :: Effect Unit }
  -> Effect (Streaming StreamChunk)
streaming source =
  runEffectFn2 streamingImpl
    (Promise.fromAff (toNullable <$> source.pull))
    source.cancel

instance WriteForeign (Streaming a) where
  writeImpl = unsafeToForeign

-- | No request body (for GET, DELETE, etc.)
-- |
-- | Example:
-- |   Request {}  -- NoBody is the default when body is omitted
data NoBody
