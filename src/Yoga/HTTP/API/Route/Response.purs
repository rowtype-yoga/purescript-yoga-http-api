module Yoga.HTTP.API.Route.Response
  ( Response(..)
  , ResponseData
  , ResponseHeaders
  , ResponseBody
  , EmptyResponse
  , class ToResponse
  , class ToResponseRL
  , respond
  , respondWith
  , respondNoHeaders
  , respondStatus
  , respondStatusWith
  , respondNoBody
  , respondNothing
  , respondNoContent
  , respondNotModified
  -- Status code helpers
  , continue
  , continueWith
  , switchingProtocols
  , switchingProtocolsWith
  , processing
  , processingWith
  , earlyHints
  , earlyHintsWith
  , ok
  , okWith
  , created
  , createdWith
  , accepted
  , acceptedWith
  , nonAuthoritativeInformation
  , nonAuthoritativeInformationWith
  , noContent
  , noContentWith
  , resetContent
  , resetContentWith
  , partialContent
  , partialContentWith
  , multiStatus
  , multiStatusWith
  , alreadyReported
  , alreadyReportedWith
  , imUsed
  , imUsedWith
  , multipleChoices
  , multipleChoicesWith
  , movedPermanently
  , movedPermanentlyWith
  , found
  , foundWith
  , seeOther
  , seeOtherWith
  , notModified
  , notModifiedWith
  , useProxy
  , useProxyWith
  , temporaryRedirect
  , temporaryRedirectWith
  , permanentRedirect
  , permanentRedirectWith
  , badRequest
  , badRequestWith
  , unauthorized
  , unauthorizedWith
  , paymentRequired
  , paymentRequiredWith
  , forbidden
  , forbiddenWith
  , notFound
  , notFoundWith
  , methodNotAllowed
  , methodNotAllowedWith
  , notAcceptable
  , notAcceptableWith
  , proxyAuthenticationRequired
  , proxyAuthenticationRequiredWith
  , requestTimeout
  , requestTimeoutWith
  , conflict
  , conflictWith
  , gone
  , goneWith
  , lengthRequired
  , lengthRequiredWith
  , preconditionFailed
  , preconditionFailedWith
  , payloadTooLarge
  , payloadTooLargeWith
  , uriTooLong
  , uriTooLongWith
  , unsupportedMediaType
  , unsupportedMediaTypeWith
  , rangeNotSatisfiable
  , rangeNotSatisfiableWith
  , expectationFailed
  , expectationFailedWith
  , imATeapot
  , imATeapotWith
  , misdirectedRequest
  , misdirectedRequestWith
  , unprocessableEntity
  , unprocessableEntityWith
  , locked
  , lockedWith
  , failedDependency
  , failedDependencyWith
  , tooEarly
  , tooEarlyWith
  , upgradeRequired
  , upgradeRequiredWith
  , preconditionRequired
  , preconditionRequiredWith
  , tooManyRequests
  , tooManyRequestsWith
  , requestHeaderFieldsTooLarge
  , requestHeaderFieldsTooLargeWith
  , unavailableForLegalReasons
  , unavailableForLegalReasonsWith
  , internalServerError
  , internalServerErrorWith
  , notImplemented
  , notImplementedWith
  , badGateway
  , badGatewayWith
  , serviceUnavailable
  , serviceUnavailableWith
  , gatewayTimeout
  , gatewayTimeoutWith
  , httpVersionNotSupported
  , httpVersionNotSupportedWith
  , variantAlsoNegotiates
  , variantAlsoNegotiatesWith
  , insufficientStorage
  , insufficientStorageWith
  , loopDetected
  , loopDetectedWith
  , notExtended
  , notExtendedWith
  , networkAuthenticationRequired
  , networkAuthenticationRequiredWith
  , module Data.Variant
  ) where

import Data.Symbol (class IsSymbol)
import Data.Unit (Unit, unit)
import Data.Variant (Variant)
import Data.Variant as Variant
import Prim.Row (class Cons)
import Prim.RowList as RL
import Prim.TypeError (class Fail, Text, Above)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Yoga.HTTP.API.Route.StatusCode (class StatusCodeToLabel)

--------------------------------------------------------------------------------
-- Response Data Type
--------------------------------------------------------------------------------

-- | Response data combining headers and body
-- | This is a data type (not type alias) to work with type class instances
data Response headers body = Response
  { headers :: Record headers
  , body :: body
  }

type ResponseBody body = Response () body

type ResponseHeaders headers = Response headers Unit

type EmptyResponse = Response () Unit

-- | Deprecated alias for backwards compatibility
type ResponseData headers body = Response headers body

--------------------------------------------------------------------------------
-- ToResponse: Convert record syntax to Response type
--------------------------------------------------------------------------------

-- | Convert user-friendly record syntax to Response type.
-- |
-- | Supports:
-- |   Response headers body → Response headers body (identity)
-- |   { body :: User } → Response () User
-- |   { headers :: { "Location" :: String }, body :: User } → Response ("Location" :: String) User
class ToResponse (recordType :: Type) (headers :: Row Type) (body :: Type) | recordType -> headers body

-- Identity instance: Response is already Response
instance toResponseIdentity :: ToResponse (Response headers body) headers body

-- Record instance: delegate to RowList-based helper
else instance toResponseRecord ::
  ( RL.RowToList recordRow rl
  , ToResponseRL rl headers body
  ) =>
  ToResponse (Record recordRow) headers body

-- | RowList-based helper to distinguish records with/without headers.
-- | Instance heads are distinguishable by the RowList structure.
class ToResponseRL (rl :: RL.RowList Type) (headers :: Row Type) (body :: Type) | rl -> headers body

-- { body :: b, headers :: Record h } (headers comes before body in sorted RowList)
instance toResponseRLBodyHeaders ::
  ToResponseRL (RL.Cons "body" body (RL.Cons "headers" (Record headers) RL.Nil)) headers body

-- { body :: b } (no headers)
else instance toResponseRLBodyOnly ::
  ToResponseRL (RL.Cons "body" body RL.Nil) () body

-- Invalid record structure: has fields other than "body" and "headers"
else instance toResponseRLInvalid ::
  ( Fail
      ( Above
          (Text "Invalid response record structure.")
          ( Above
              (Text "")
              ( Above
                  (Text "Response records must contain ONLY:")
                  ( Above
                      (Text "  • body :: YourBodyType              (required)")
                      ( Above
                          (Text "  • headers :: Record (...)         (optional)")
                          ( Above
                              (Text "")
                              ( Above
                                  (Text "Valid examples:")
                                  ( Above
                                      (Text "  { body: user }")
                                      ( Above
                                          (Text "  { body: user, headers: { \"Location\": \"/users/123\" } }")
                                          ( Above
                                              (Text "")
                                              (Text "Do not include 'status', 'statusCode', or other fields.")
                                          )
                                      )
                                  )
                              )
                          )
                      )
                  )
              )
          )
      )
  ) =>
  ToResponseRL rl headers body

--------------------------------------------------------------------------------
-- Response Construction Helpers
--------------------------------------------------------------------------------

-- | Construct a variant response with full control over headers and body
-- |
-- | Example:
-- |   respond (Proxy :: _ "created")
-- |     (Response { headers: { "Location": "/users/123" }, body: user })
respond
  :: forall label headers body r1 r2
   . IsSymbol label
  => Cons label (Response headers body) r1 r2
  => Proxy label
  -> Response headers body
  -> Variant r2
respond label rd = Variant.inj label rd

-- | Construct a variant response with separate headers and body arguments
-- |
-- | Example:
-- |   respondWith (Proxy :: _ "created")
-- |     { "Location": "/users/123" }
-- |     user
respondWith
  :: forall label headers body r1 r2
   . IsSymbol label
  => Cons label (Response headers body) r1 r2
  => Proxy label
  -> Record headers
  -> body
  -> Variant r2
respondWith label headers body =
  Variant.inj label (Response { headers, body })

-- | Construct a variant response with no custom headers (most common case)
-- |
-- | Example:
-- |   respondNoHeaders (Proxy :: _ "ok") user
-- |   respondNoHeaders (Proxy :: _ "notFound") { error: "Not found" }
respondNoHeaders
  :: forall @label body r1 r2
   . IsSymbol label
  => Cons label (Response () body) r1 r2
  => body
  -> Variant r2
respondNoHeaders body =
  Variant.inj (Proxy :: Proxy label) (Response { headers: {}, body })

-- | Construct a variant response using numeric status code
-- |
-- | Example:
-- |   respondStatus @200 user
-- |   respondStatus @404 { error: "Not found" }
respondStatus
  :: forall @code label body r1 r2
   . StatusCodeToLabel code label
  => IsSymbol label
  => Cons label (Response () body) r1 r2
  => body
  -> Variant r2
respondStatus body =
  Variant.inj (Proxy :: Proxy label) (Response { headers: {}, body })

-- | Construct a variant response with headers using numeric status code
-- |
-- | Example:
-- |   respondStatusWith @201 { "Location": "/users/123" } user
respondStatusWith
  :: forall @code label headers body r1 r2
   . StatusCodeToLabel code label
  => IsSymbol label
  => Cons label (Response headers body) r1 r2
  => Record headers
  -> body
  -> Variant r2
respondStatusWith headers body =
  Variant.inj (Proxy :: Proxy label) (Response { headers, body })

-- | Construct a variant response with headers but no body
-- |
-- | Example:
-- |   respondNoBody (Proxy :: _ "noContent")
-- |     { "X-Resource-Id": "123" }
respondNoBody
  :: forall label headers r1 r2
   . IsSymbol label
  => Cons label (Response headers Unit) r1 r2
  => Proxy label
  -> Record headers
  -> Variant r2
respondNoBody label headers =
  Variant.inj label (Response { headers, body: unit })

-- | Construct a variant response with neither headers nor body
-- |
-- | Example:
-- |   respondNothing (Proxy :: _ "noContent")
-- |   respondNothing (Proxy :: _ "notModified")
respondNothing
  :: forall @label r1 r2
   . IsSymbol label
  => Cons label (Response () Unit) r1 r2
  => Variant r2
respondNothing =
  Variant.inj (Proxy :: Proxy label) (Response { headers: {}, body: unit })

-- | Construct a 204 No Content response (no headers, no body)
-- |
-- | Example:
-- |   respondNoContent
respondNoContent
  :: forall r1 r2
   . Cons "noContent" (Response () Unit) r1 r2
  => Variant r2
respondNoContent =
  unsafeCoerce (Variant.inj (Proxy :: Proxy "noContent") (Response { headers: {}, body: unit }))

-- | Construct a 304 Not Modified response (no headers, no body)
-- |
-- | Example:
-- |   respondNotModified
respondNotModified
  :: forall r1 r2
   . Cons "notModified" (Response () Unit) r1 r2
  => Variant r2
respondNotModified =
  unsafeCoerce (Variant.inj (Proxy :: Proxy "notModified") (Response { headers: {}, body: unit }))

--------------------------------------------------------------------------------
-- Status Code Helper Functions
--------------------------------------------------------------------------------

continue = respondNoHeaders @"continue"

continueWith = respondWith (Proxy :: Proxy "continue")

switchingProtocols = respondNoHeaders @"switchingProtocols"

switchingProtocolsWith = respondWith (Proxy :: Proxy "switchingProtocols")

processing = respondNoHeaders @"processing"

processingWith = respondWith (Proxy :: Proxy "processing")

earlyHints = respondNoHeaders @"earlyHints"

earlyHintsWith = respondWith (Proxy :: Proxy "earlyHints")

ok = respondNoHeaders @"ok"

okWith = respondWith (Proxy :: Proxy "ok")

created = respondNoHeaders @"created"

createdWith = respondWith (Proxy :: Proxy "created")

accepted = respondNoHeaders @"accepted"

acceptedWith = respondWith (Proxy :: Proxy "accepted")

nonAuthoritativeInformation = respondNoHeaders @"nonAuthoritativeInformation"

nonAuthoritativeInformationWith = respondWith (Proxy :: Proxy "nonAuthoritativeInformation")

noContent = respondNoHeaders @"noContent"

noContentWith = respondWith (Proxy :: Proxy "noContent")

resetContent = respondNoHeaders @"resetContent"

resetContentWith = respondWith (Proxy :: Proxy "resetContent")

partialContent = respondNoHeaders @"partialContent"

partialContentWith = respondWith (Proxy :: Proxy "partialContent")

multiStatus = respondNoHeaders @"multiStatus"

multiStatusWith = respondWith (Proxy :: Proxy "multiStatus")

alreadyReported = respondNoHeaders @"alreadyReported"

alreadyReportedWith = respondWith (Proxy :: Proxy "alreadyReported")

imUsed = respondNoHeaders @"imUsed"

imUsedWith = respondWith (Proxy :: Proxy "imUsed")

multipleChoices = respondNoHeaders @"multipleChoices"

multipleChoicesWith = respondWith (Proxy :: Proxy "multipleChoices")

movedPermanently = respondNoHeaders @"movedPermanently"

movedPermanentlyWith = respondWith (Proxy :: Proxy "movedPermanently")

found = respondNoHeaders @"found"

foundWith = respondWith (Proxy :: Proxy "found")

seeOther = respondNoHeaders @"seeOther"

seeOtherWith = respondWith (Proxy :: Proxy "seeOther")

notModified = respondNoHeaders @"notModified"

notModifiedWith = respondWith (Proxy :: Proxy "notModified")

useProxy = respondNoHeaders @"useProxy"

useProxyWith = respondWith (Proxy :: Proxy "useProxy")

temporaryRedirect = respondNoHeaders @"temporaryRedirect"

temporaryRedirectWith = respondWith (Proxy :: Proxy "temporaryRedirect")

permanentRedirect = respondNoHeaders @"permanentRedirect"

permanentRedirectWith = respondWith (Proxy :: Proxy "permanentRedirect")

badRequest = respondNoHeaders @"badRequest"

badRequestWith = respondWith (Proxy :: Proxy "badRequest")

unauthorized = respondNoHeaders @"unauthorized"

unauthorizedWith = respondWith (Proxy :: Proxy "unauthorized")

paymentRequired = respondNoHeaders @"paymentRequired"

paymentRequiredWith = respondWith (Proxy :: Proxy "paymentRequired")

forbidden = respondNoHeaders @"forbidden"

forbiddenWith = respondWith (Proxy :: Proxy "forbidden")

notFound = respondNoHeaders @"notFound"

notFoundWith = respondWith (Proxy :: Proxy "notFound")

methodNotAllowed = respondNoHeaders @"methodNotAllowed"

methodNotAllowedWith = respondWith (Proxy :: Proxy "methodNotAllowed")

notAcceptable = respondNoHeaders @"notAcceptable"

notAcceptableWith = respondWith (Proxy :: Proxy "notAcceptable")

proxyAuthenticationRequired = respondNoHeaders @"proxyAuthenticationRequired"

proxyAuthenticationRequiredWith = respondWith (Proxy :: Proxy "proxyAuthenticationRequired")

requestTimeout = respondNoHeaders @"requestTimeout"

requestTimeoutWith = respondWith (Proxy :: Proxy "requestTimeout")

conflict = respondNoHeaders @"conflict"

conflictWith = respondWith (Proxy :: Proxy "conflict")

gone = respondNoHeaders @"gone"

goneWith = respondWith (Proxy :: Proxy "gone")

lengthRequired = respondNoHeaders @"lengthRequired"

lengthRequiredWith = respondWith (Proxy :: Proxy "lengthRequired")

preconditionFailed = respondNoHeaders @"preconditionFailed"

preconditionFailedWith = respondWith (Proxy :: Proxy "preconditionFailed")

payloadTooLarge = respondNoHeaders @"payloadTooLarge"

payloadTooLargeWith = respondWith (Proxy :: Proxy "payloadTooLarge")

uriTooLong = respondNoHeaders @"uriTooLong"

uriTooLongWith = respondWith (Proxy :: Proxy "uriTooLong")

unsupportedMediaType = respondNoHeaders @"unsupportedMediaType"

unsupportedMediaTypeWith = respondWith (Proxy :: Proxy "unsupportedMediaType")

rangeNotSatisfiable = respondNoHeaders @"rangeNotSatisfiable"

rangeNotSatisfiableWith = respondWith (Proxy :: Proxy "rangeNotSatisfiable")

expectationFailed = respondNoHeaders @"expectationFailed"

expectationFailedWith = respondWith (Proxy :: Proxy "expectationFailed")

imATeapot = respondNoHeaders @"imATeapot"

imATeapotWith = respondWith (Proxy :: Proxy "imATeapot")

misdirectedRequest = respondNoHeaders @"misdirectedRequest"

misdirectedRequestWith = respondWith (Proxy :: Proxy "misdirectedRequest")

unprocessableEntity = respondNoHeaders @"unprocessableEntity"

unprocessableEntityWith = respondWith (Proxy :: Proxy "unprocessableEntity")

locked = respondNoHeaders @"locked"

lockedWith = respondWith (Proxy :: Proxy "locked")

failedDependency = respondNoHeaders @"failedDependency"

failedDependencyWith = respondWith (Proxy :: Proxy "failedDependency")

tooEarly = respondNoHeaders @"tooEarly"

tooEarlyWith = respondWith (Proxy :: Proxy "tooEarly")

upgradeRequired = respondNoHeaders @"upgradeRequired"

upgradeRequiredWith = respondWith (Proxy :: Proxy "upgradeRequired")

preconditionRequired = respondNoHeaders @"preconditionRequired"

preconditionRequiredWith = respondWith (Proxy :: Proxy "preconditionRequired")

tooManyRequests = respondNoHeaders @"tooManyRequests"

tooManyRequestsWith = respondWith (Proxy :: Proxy "tooManyRequests")

requestHeaderFieldsTooLarge = respondNoHeaders @"requestHeaderFieldsTooLarge"

requestHeaderFieldsTooLargeWith = respondWith (Proxy :: Proxy "requestHeaderFieldsTooLarge")

unavailableForLegalReasons = respondNoHeaders @"unavailableForLegalReasons"

unavailableForLegalReasonsWith = respondWith (Proxy :: Proxy "unavailableForLegalReasons")

internalServerError = respondNoHeaders @"internalServerError"

internalServerErrorWith = respondWith (Proxy :: Proxy "internalServerError")

notImplemented = respondNoHeaders @"notImplemented"

notImplementedWith = respondWith (Proxy :: Proxy "notImplemented")

badGateway = respondNoHeaders @"badGateway"

badGatewayWith = respondWith (Proxy :: Proxy "badGateway")

serviceUnavailable = respondNoHeaders @"serviceUnavailable"

serviceUnavailableWith = respondWith (Proxy :: Proxy "serviceUnavailable")

gatewayTimeout = respondNoHeaders @"gatewayTimeout"

gatewayTimeoutWith = respondWith (Proxy :: Proxy "gatewayTimeout")

httpVersionNotSupported = respondNoHeaders @"httpVersionNotSupported"

httpVersionNotSupportedWith = respondWith (Proxy :: Proxy "httpVersionNotSupported")

variantAlsoNegotiates = respondNoHeaders @"variantAlsoNegotiates"

variantAlsoNegotiatesWith = respondWith (Proxy :: Proxy "variantAlsoNegotiates")

insufficientStorage = respondNoHeaders @"insufficientStorage"

insufficientStorageWith = respondWith (Proxy :: Proxy "insufficientStorage")

loopDetected = respondNoHeaders @"loopDetected"

loopDetectedWith = respondWith (Proxy :: Proxy "loopDetected")

notExtended = respondNoHeaders @"notExtended"

notExtendedWith = respondWith (Proxy :: Proxy "notExtended")

networkAuthenticationRequired = respondNoHeaders @"networkAuthenticationRequired"

networkAuthenticationRequiredWith = respondWith (Proxy :: Proxy "networkAuthenticationRequired")
