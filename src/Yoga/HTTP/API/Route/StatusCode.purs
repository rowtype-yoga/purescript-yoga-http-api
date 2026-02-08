module Yoga.HTTP.API.Route.StatusCode
  ( class StatusCodeMap
  , class StatusCodeToLabel
  , statusCodeFor
  , statusCodeToString
  , StatusCode(..)
  ) where

import Prelude

import Type.Proxy (Proxy)
import Data.Newtype (class Newtype)

newtype StatusCode = StatusCode Int

derive instance Newtype StatusCode _
derive newtype instance Show StatusCode
derive newtype instance Eq StatusCode
derive newtype instance Ord StatusCode

--------------------------------------------------------------------------------
-- Status Code Mapping
--------------------------------------------------------------------------------

-- | Map variant constructor names (Symbols) to HTTP status codes
-- |
-- | This typeclass enables convention-based status code mapping where
-- | the variant label name determines the HTTP status code.
-- |
-- | Users can extend this by adding their own instances for custom status codes.
class StatusCodeMap (sym :: Symbol) where
  statusCodeFor :: Proxy sym -> StatusCode

-- | Map status code integers to their conventional label names
-- | This is the reverse of StatusCodeMap
class StatusCodeToLabel (code :: Int) (label :: Symbol) | code -> label

--------------------------------------------------------------------------------
-- Standard HTTP Status Code Instances
--------------------------------------------------------------------------------

-- 1xx Informational
instance StatusCodeMap "continue" where
  statusCodeFor _ = StatusCode 100

instance StatusCodeMap "switchingProtocols" where
  statusCodeFor _ = StatusCode 101

instance StatusCodeMap "processing" where
  statusCodeFor _ = StatusCode 102

instance StatusCodeMap "earlyHints" where
  statusCodeFor _ = StatusCode 103

-- 2xx Success
instance StatusCodeMap "ok" where
  statusCodeFor _ = StatusCode 200

instance StatusCodeMap "created" where
  statusCodeFor _ = StatusCode 201

instance StatusCodeMap "accepted" where
  statusCodeFor _ = StatusCode 202

instance StatusCodeMap "nonAuthoritativeInformation" where
  statusCodeFor _ = StatusCode 203

instance StatusCodeMap "noContent" where
  statusCodeFor _ = StatusCode 204

instance StatusCodeMap "resetContent" where
  statusCodeFor _ = StatusCode 205

instance StatusCodeMap "partialContent" where
  statusCodeFor _ = StatusCode 206

instance StatusCodeMap "multiStatus" where
  statusCodeFor _ = StatusCode 207

instance StatusCodeMap "alreadyReported" where
  statusCodeFor _ = StatusCode 208

instance StatusCodeMap "imUsed" where
  statusCodeFor _ = StatusCode 226

-- 3xx Redirection
instance StatusCodeMap "multipleChoices" where
  statusCodeFor _ = StatusCode 300

instance StatusCodeMap "movedPermanently" where
  statusCodeFor _ = StatusCode 301

instance StatusCodeMap "found" where
  statusCodeFor _ = StatusCode 302

instance StatusCodeMap "seeOther" where
  statusCodeFor _ = StatusCode 303

instance StatusCodeMap "notModified" where
  statusCodeFor _ = StatusCode 304

instance StatusCodeMap "useProxy" where
  statusCodeFor _ = StatusCode 305

instance StatusCodeMap "temporaryRedirect" where
  statusCodeFor _ = StatusCode 307

instance StatusCodeMap "permanentRedirect" where
  statusCodeFor _ = StatusCode 308

-- 4xx Client Errors
instance StatusCodeMap "badRequest" where
  statusCodeFor _ = StatusCode 400

instance StatusCodeMap "unauthorized" where
  statusCodeFor _ = StatusCode 401

instance StatusCodeMap "paymentRequired" where
  statusCodeFor _ = StatusCode 402

instance StatusCodeMap "forbidden" where
  statusCodeFor _ = StatusCode 403

instance StatusCodeMap "notFound" where
  statusCodeFor _ = StatusCode 404

instance StatusCodeMap "methodNotAllowed" where
  statusCodeFor _ = StatusCode 405

instance StatusCodeMap "notAcceptable" where
  statusCodeFor _ = StatusCode 406

instance StatusCodeMap "proxyAuthenticationRequired" where
  statusCodeFor _ = StatusCode 407

instance StatusCodeMap "requestTimeout" where
  statusCodeFor _ = StatusCode 408

instance StatusCodeMap "conflict" where
  statusCodeFor _ = StatusCode 409

instance StatusCodeMap "gone" where
  statusCodeFor _ = StatusCode 410

instance StatusCodeMap "lengthRequired" where
  statusCodeFor _ = StatusCode 411

instance StatusCodeMap "preconditionFailed" where
  statusCodeFor _ = StatusCode 412

instance StatusCodeMap "payloadTooLarge" where
  statusCodeFor _ = StatusCode 413

instance StatusCodeMap "uriTooLong" where
  statusCodeFor _ = StatusCode 414

instance StatusCodeMap "unsupportedMediaType" where
  statusCodeFor _ = StatusCode 415

instance StatusCodeMap "rangeNotSatisfiable" where
  statusCodeFor _ = StatusCode 416

instance StatusCodeMap "expectationFailed" where
  statusCodeFor _ = StatusCode 417

instance StatusCodeMap "imATeapot" where
  statusCodeFor _ = StatusCode 418

instance StatusCodeMap "misdirectedRequest" where
  statusCodeFor _ = StatusCode 421

instance StatusCodeMap "unprocessableEntity" where
  statusCodeFor _ = StatusCode 422

instance StatusCodeMap "locked" where
  statusCodeFor _ = StatusCode 423

instance StatusCodeMap "failedDependency" where
  statusCodeFor _ = StatusCode 424

instance StatusCodeMap "tooEarly" where
  statusCodeFor _ = StatusCode 425

instance StatusCodeMap "upgradeRequired" where
  statusCodeFor _ = StatusCode 426

instance StatusCodeMap "preconditionRequired" where
  statusCodeFor _ = StatusCode 428

instance StatusCodeMap "tooManyRequests" where
  statusCodeFor _ = StatusCode 429

instance StatusCodeMap "requestHeaderFieldsTooLarge" where
  statusCodeFor _ = StatusCode 431

instance StatusCodeMap "unavailableForLegalReasons" where
  statusCodeFor _ = StatusCode 451

-- 5xx Server Errors
instance StatusCodeMap "internalServerError" where
  statusCodeFor _ = StatusCode 500

instance StatusCodeMap "notImplemented" where
  statusCodeFor _ = StatusCode 501

instance StatusCodeMap "badGateway" where
  statusCodeFor _ = StatusCode 502

instance StatusCodeMap "serviceUnavailable" where
  statusCodeFor _ = StatusCode 503

instance StatusCodeMap "gatewayTimeout" where
  statusCodeFor _ = StatusCode 504

instance StatusCodeMap "httpVersionNotSupported" where
  statusCodeFor _ = StatusCode 505

instance StatusCodeMap "variantAlsoNegotiates" where
  statusCodeFor _ = StatusCode 506

instance StatusCodeMap "insufficientStorage" where
  statusCodeFor _ = StatusCode 507

instance StatusCodeMap "loopDetected" where
  statusCodeFor _ = StatusCode 508

instance StatusCodeMap "notExtended" where
  statusCodeFor _ = StatusCode 510

instance StatusCodeMap "networkAuthenticationRequired" where
  statusCodeFor _ = StatusCode 511

--------------------------------------------------------------------------------
-- Reverse Mapping: Int -> Symbol
--------------------------------------------------------------------------------

-- 1xx Informational
instance StatusCodeToLabel 100 "continue"
instance StatusCodeToLabel 101 "switchingProtocols"
instance StatusCodeToLabel 102 "processing"
instance StatusCodeToLabel 103 "earlyHints"

-- 2xx Success
instance StatusCodeToLabel 200 "ok"
instance StatusCodeToLabel 201 "created"
instance StatusCodeToLabel 202 "accepted"
instance StatusCodeToLabel 203 "nonAuthoritativeInformation"
instance StatusCodeToLabel 204 "noContent"
instance StatusCodeToLabel 205 "resetContent"
instance StatusCodeToLabel 206 "partialContent"
instance StatusCodeToLabel 207 "multiStatus"
instance StatusCodeToLabel 208 "alreadyReported"
instance StatusCodeToLabel 226 "imUsed"

-- 3xx Redirection
instance StatusCodeToLabel 300 "multipleChoices"
instance StatusCodeToLabel 301 "movedPermanently"
instance StatusCodeToLabel 302 "found"
instance StatusCodeToLabel 303 "seeOther"
instance StatusCodeToLabel 304 "notModified"
instance StatusCodeToLabel 305 "useProxy"
instance StatusCodeToLabel 307 "temporaryRedirect"
instance StatusCodeToLabel 308 "permanentRedirect"

-- 4xx Client Errors
instance StatusCodeToLabel 400 "badRequest"
instance StatusCodeToLabel 401 "unauthorized"
instance StatusCodeToLabel 402 "paymentRequired"
instance StatusCodeToLabel 403 "forbidden"
instance StatusCodeToLabel 404 "notFound"
instance StatusCodeToLabel 405 "methodNotAllowed"
instance StatusCodeToLabel 406 "notAcceptable"
instance StatusCodeToLabel 407 "proxyAuthenticationRequired"
instance StatusCodeToLabel 408 "requestTimeout"
instance StatusCodeToLabel 409 "conflict"
instance StatusCodeToLabel 410 "gone"
instance StatusCodeToLabel 411 "lengthRequired"
instance StatusCodeToLabel 412 "preconditionFailed"
instance StatusCodeToLabel 413 "payloadTooLarge"
instance StatusCodeToLabel 414 "uriTooLong"
instance StatusCodeToLabel 415 "unsupportedMediaType"
instance StatusCodeToLabel 416 "rangeNotSatisfiable"
instance StatusCodeToLabel 417 "expectationFailed"
instance StatusCodeToLabel 418 "imATeapot"
instance StatusCodeToLabel 421 "misdirectedRequest"
instance StatusCodeToLabel 422 "unprocessableEntity"
instance StatusCodeToLabel 423 "locked"
instance StatusCodeToLabel 424 "failedDependency"
instance StatusCodeToLabel 425 "tooEarly"
instance StatusCodeToLabel 426 "upgradeRequired"
instance StatusCodeToLabel 428 "preconditionRequired"
instance StatusCodeToLabel 429 "tooManyRequests"
instance StatusCodeToLabel 431 "requestHeaderFieldsTooLarge"
instance StatusCodeToLabel 451 "unavailableForLegalReasons"

-- 5xx Server Errors
instance StatusCodeToLabel 500 "internalServerError"
instance StatusCodeToLabel 501 "notImplemented"
instance StatusCodeToLabel 502 "badGateway"
instance StatusCodeToLabel 503 "serviceUnavailable"
instance StatusCodeToLabel 504 "gatewayTimeout"
instance StatusCodeToLabel 505 "httpVersionNotSupported"
instance StatusCodeToLabel 506 "variantAlsoNegotiates"
instance StatusCodeToLabel 507 "insufficientStorage"
instance StatusCodeToLabel 508 "loopDetected"
instance StatusCodeToLabel 510 "notExtended"
instance StatusCodeToLabel 511 "networkAuthenticationRequired"

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- | Convert StatusCode to String for OpenAPI generation
statusCodeToString :: StatusCode -> String
statusCodeToString (StatusCode n) = show n
