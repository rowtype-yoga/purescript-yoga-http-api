module Test.PublicSurfaceTest where

import Prelude

import Data.Array (length)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Undefined.NoProblem (opt)
import Data.String as String
import Data.Variant (Variant)
import Data.Variant as Variant
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign.Object as FObject
import Type.Function (type (#))
import Type.Proxy (Proxy(..))
import ViTest (ViTest, describe, test)
import ViTest.Expect (expectToBe)
import Yoga.HTTP.API.Path (Capture, Lit, Path, Required, Root, parseParam, parsePath, pathPattern, type (/), type (:), type (:?))
import Yoga.HTTP.API.Route (ApiKeyCookie(..), ApiKeyHeader(..), BasicAuth(..), BearerToken(..), CustomContentType, DELETE, Deprecated, DigestAuth(..), Enum, Example, Format, FormData, GET, JSON, MaxLength, Maximum, MinLength, Minimum, MultipartFormData, NoBody, NoRequest, Nullable, PATCH, POST, PUT, PlainText, QUERY, Response(..), Route, Schema, Title, XML, apiHandlers, buildOpenAPISpec', default, deprecated, description, enum, example, format, headerValueType, maxLength, maximum, minLength, minimum, mkHandler, nullable, options, parseHeader, pattern, printHeader, runHandler, statusCodeFor, statusCodeToString, title)
import Yoga.HTTP.API.Route.Handler (Request, class EncodingBody)
import Yoga.HTTP.API.Route.HeaderError (HeaderError(..))
import Yoga.HTTP.API.Route.OpenAPI (detectCookieSecurity, detectSecurity, getContentType, renderCookieParamsSchema, renderHeadersSchema, renderJSONSchema, renderPathParamsSchema, renderQueryParamsSchema, renderRequestBodySchema, renderResponseHeadersSchema)
import Yoga.HTTP.API.Route.OpenAPIMetadata (Description, Default, Pattern, links, type (:#))
import Yoga.HTTP.API.Route.RenderMethod (renderMethod)
import Yoga.HTTP.API.Route.Response (ok, respondNoBody, respondNoContent, respondNotModified, respondNothing, respondStatus, respondStatusWith)
import Yoga.HTTP.API.Route.RouteHandler (Handler)
import Yoga.HTTP.API.Route.StatusCode (class StatusCodeMap)
import Yoga.JSON (writeJSON)

data Access = Guest | Member | Admin

derive instance Generic Access _

expectEqual :: forall a. Eq a => Show a => a -> a -> Aff Unit
expectEqual expected actual = expectToBe (show expected) (show actual)

code :: forall @label. StatusCodeMap label => String
code = statusCodeToString (statusCodeFor (Proxy :: Proxy label))

decodeBody :: forall encoding body. EncodingBody encoding body => Proxy encoding -> body -> body
decodeBody _ = identity

type HandlerRoute =
  Route GET
    (Path ("items" / "id" : Int) :? { limit :: Required Int, search :: String })
    { headers :: { requestId :: String }, body :: JSON { suffix :: String } }
    (ok :: { body :: String })

handler :: Handler HandlerRoute
handler = mkHandler \{ path, query, headers, body } ->
  pure $ ok (headers.requestId <> ":" <> show path.id <> ":" <> show query.limit <> ":" <> show query.search <> ":" <> body.suffix)

type WrappedHandlerRoute =
  Route POST "wrapped" (Request { body :: FormData String }) (ok :: { body :: String })

wrappedHandler :: Handler WrappedHandlerRoute
wrappedHandler = mkHandler \{ body } -> pure $ ok body

type NoRequestRoute =
  Route GET Root NoRequest (ok :: { body :: String })

noRequestHandler :: Handler NoRequestRoute
noRequestHandler = mkHandler \_ -> pure $ ok "healthy"

type ServerAPI =
  { health :: NoRequestRoute }

testMethodsAndPaths :: Effect ViTest
testMethodsAndPaths = describe "public methods and paths" do
  _ <- test "renders every method token" do
    expectEqual
      [ "get", "post", "put", "delete", "patch", "query" ]
      [ renderMethod (Proxy :: Proxy GET)
      , renderMethod (Proxy :: Proxy POST)
      , renderMethod (Proxy :: Proxy PUT)
      , renderMethod (Proxy :: Proxy DELETE)
      , renderMethod (Proxy :: Proxy PATCH)
      , renderMethod (Proxy :: Proxy QUERY)
      ]

  _ <- test "renders root, literal, capture, sugar, wrapped, and query paths" do
    expectEqual
      [ "/", "/", "/users", "/:id", "/users/:id/posts", "/users/:id", "/users/:id" ]
      [ pathPattern (Proxy :: Proxy Root)
      , pathPattern (Proxy :: Proxy "/")
      , pathPattern (Proxy :: Proxy (Lit "users"))
      , pathPattern (Proxy :: Proxy (Capture "id" Int))
      , pathPattern (Proxy :: Proxy (Path (Lit "users" / Capture "id" Int / Lit "posts")))
      , pathPattern (Proxy :: Proxy (Path ("users" / "id" : Int)))
      , pathPattern (Proxy :: Proxy (Path ("users" / "id" : Int) :? { q :: String }))
      ]

  _ <- test "parses primitive parameters and rejects malformed values" do
    expectEqual (Right "") (parseParam "" :: Either String String)
    expectEqual (Right 42) (parseParam "42" :: Either String Int)
    expectEqual (Left "Expected an integer but got: 4.2") (parseParam "4.2" :: Either String Int)
    expectEqual (Right 4.2) (parseParam "4.2" :: Either String Number)
    expectEqual (Left "Expected a number but got: nope") (parseParam "nope" :: Either String Number)
    expectEqual (Right true) (parseParam "true" :: Either String Boolean)
    expectEqual (Right false) (parseParam "FALSE" :: Either String Boolean)
    expectEqual (Left "Expected a boolean but got: yes") (parseParam "yes" :: Either String Boolean)

  test "parses exact paths into correctly named captures" do
    expectEqual (Just {}) (parsePath (Proxy :: Proxy (Path Root)) "/")
    expectEqual Nothing (parsePath (Proxy :: Proxy (Path Root)) "")
    expectEqual (Just {}) (parsePath (Proxy :: Proxy (Path (Lit "users"))) "/users")
    expectEqual Nothing (parsePath (Proxy :: Proxy (Path (Lit "users"))) "/posts")
    expectEqual (Just { id: 7 }) (parsePath (Proxy :: Proxy (Path (Capture "id" Int))) "/7")
    expectEqual Nothing (parsePath (Proxy :: Proxy (Path (Capture "id" Int))) "/bad")
    expectEqual (Just { id: 7 }) (parsePath (Proxy :: Proxy (Path (Lit "users" / Capture "id" Int))) "/users/7")
    expectEqual Nothing (parsePath (Proxy :: Proxy (Path (Lit "users" / Capture "id" Int))) "/users/7/extra")

testHeadersAndAuth :: Effect ViTest
testHeadersAndAuth = describe "header values, errors, and authentication" do
  _ <- test "roundtrips strings, integers, and optional headers" do
    expectEqual (Right "alpha") (parseHeader "alpha" :: Either String String)
    expectEqual "alpha" (printHeader "alpha")
    expectEqual (Right 12) (parseHeader "12" :: Either String Int)
    expectEqual (Left "not a valid integer (got: twelve)") (parseHeader "twelve" :: Either String Int)
    expectEqual "12" (printHeader 12)
    expectEqual (Right (Just 12)) (parseHeader "12" :: Either String (Maybe Int))
    expectEqual (Right Nothing) (parseHeader "twelve" :: Either String (Maybe Int))
    expectEqual "" (printHeader (Nothing :: Maybe Int))

  _ <- test "validates and roundtrips every authentication header type" do
    expectEqual (Right (BearerToken "token")) (parseHeader "Bearer token" :: Either String BearerToken)
    expectEqual (Left "missing 'Bearer ' prefix") (parseHeader "bearer token" :: Either String BearerToken)
    expectEqual "Bearer token" (printHeader (BearerToken "token"))
    expectEqual (Right (BasicAuth "dXNlcjpwYXNz")) (parseHeader "Basic dXNlcjpwYXNz" :: Either String BasicAuth)
    expectEqual (Left "missing 'Basic ' prefix") (parseHeader "dXNlcjpwYXNz" :: Either String BasicAuth)
    expectEqual "Basic dXNlcjpwYXNz" (printHeader (BasicAuth "dXNlcjpwYXNz"))
    expectEqual (Right (ApiKeyHeader "key")) (parseHeader "key" :: Either String ApiKeyHeader)
    expectEqual "key" (printHeader (ApiKeyHeader "key"))
    expectEqual (Right (ApiKeyCookie "session")) (parseHeader "session" :: Either String ApiKeyCookie)
    expectEqual "session" (printHeader (ApiKeyCookie "session"))
    expectEqual (Right (DigestAuth "Digest realm=api")) (parseHeader "Digest realm=api" :: Either String DigestAuth)
    expectEqual (Left "missing 'Digest ' prefix") (parseHeader "realm=api" :: Either String DigestAuth)
    expectEqual "Digest realm=api" (printHeader (DigestAuth "Digest realm=api"))

  test "distinguishes missing headers from invalid values" do
    expectEqual "Missing required header: x-request-id" (show (MissingHeader "x-request-id"))
    expectEqual "Invalid header 'content-length': not an integer" (show (InvalidHeaderValue "content-length" "not an integer"))
    expectToBe true (MissingHeader "x" /= InvalidHeaderValue "x" "missing")

testStatusesAndResponses :: Effect ViTest
testStatusesAndResponses = describe "status and response constructors" do
  _ <- test "maps every public status label" do
    expectEqual
      [ "100", "101", "102", "103"
      , "200", "201", "202", "203", "204", "205", "206", "207", "208", "226"
      , "300", "301", "302", "303", "304", "305", "307", "308"
      , "400", "401", "402", "403", "404", "405", "406", "407", "408", "409", "410", "411", "412", "413", "414", "415", "416", "417", "418", "421", "422", "423", "424", "425", "426", "428", "429", "431", "451"
      , "500", "501", "502", "503", "504", "505", "506", "507", "508", "510", "511"
      ]
      [ code @"continue", code @"switchingProtocols", code @"processing", code @"earlyHints"
      , code @"ok", code @"created", code @"accepted", code @"nonAuthoritativeInformation", code @"noContent", code @"resetContent", code @"partialContent", code @"multiStatus", code @"alreadyReported", code @"imUsed"
      , code @"multipleChoices", code @"movedPermanently", code @"found", code @"seeOther", code @"notModified", code @"useProxy", code @"temporaryRedirect", code @"permanentRedirect"
      , code @"badRequest", code @"unauthorized", code @"paymentRequired", code @"forbidden", code @"notFound", code @"methodNotAllowed", code @"notAcceptable", code @"proxyAuthenticationRequired", code @"requestTimeout", code @"conflict", code @"gone", code @"lengthRequired", code @"preconditionFailed", code @"payloadTooLarge", code @"uriTooLong", code @"unsupportedMediaType", code @"rangeNotSatisfiable", code @"expectationFailed", code @"imATeapot", code @"misdirectedRequest", code @"unprocessableEntity", code @"locked", code @"failedDependency", code @"tooEarly", code @"upgradeRequired", code @"preconditionRequired", code @"tooManyRequests", code @"requestHeaderFieldsTooLarge", code @"unavailableForLegalReasons"
      , code @"internalServerError", code @"notImplemented", code @"badGateway", code @"serviceUnavailable", code @"gatewayTimeout", code @"httpVersionNotSupported", code @"variantAlsoNegotiates", code @"insufficientStorage", code @"loopDetected", code @"notExtended", code @"networkAuthenticationRequired"
      ]

  _ <- test "constructs numeric responses with status-derived labels" do
    let
      plain :: Variant (ok :: Response () String)
      plain = respondStatus @200 "body"
      withHeaders :: Variant (created :: Response (location :: String) String)
      withHeaders = respondStatusWith @201 { location: "/items/1" } "created"
    expectEqual "body" (Variant.match { ok: \(Response r) -> r.body } plain)
    expectEqual "/items/1:created" (Variant.match { created: \(Response r) -> r.headers.location <> ":" <> r.body } withHeaders)

  test "constructs bodyless and empty standard responses" do
    let
      bodyless :: Variant (noContent :: Response (etag :: String) Unit)
      bodyless = respondNoBody (Proxy :: Proxy "noContent") { etag: "v1" }
      nothing :: Variant (accepted :: Response () Unit)
      nothing = respondNothing @"accepted"
      noContentResponse :: Variant (noContent :: Response () Unit)
      noContentResponse = respondNoContent
      notModifiedResponse :: Variant (notModified :: Response () Unit)
      notModifiedResponse = respondNotModified
    expectEqual "v1" (Variant.match { noContent: \(Response r) -> r.headers.etag } bodyless)
    expectEqual unit (Variant.match { accepted: \(Response r) -> r.body } nothing)
    expectEqual unit (Variant.match { noContent: \(Response r) -> r.body } noContentResponse)
    expectEqual unit (Variant.match { notModified: \(Response r) -> r.body } notModifiedResponse)

testMetadataAndOpenAPI :: Effect ViTest
testMetadataAndOpenAPI = describe "metadata and OpenAPI primitives" do
  _ <- test "extracts every composable schema metadata value" do
    let p = Proxy :: Proxy (String # Description "identifier" # Example "abc" # Format "uuid" # Minimum 1 # Maximum 9 # Pattern "^[a-z]+$" # MinLength 2 # MaxLength 8 # Title "Identifier" # Nullable # Default "abc" # Deprecated)
    expectEqual (Just "identifier") (description p)
    expectEqual (Just "abc") (example p)
    expectEqual (Just "uuid") (format p)
    expectEqual (Just 1) (minimum p)
    expectEqual (Just 9) (maximum p)
    expectEqual (Just "^[a-z]+$") (pattern p)
    expectEqual (Just 2) (minLength p)
    expectEqual (Just 8) (maxLength p)
    expectEqual (Just "Identifier") (title p)
    expectEqual true (nullable p)
    expectEqual (Just "abc") (default p)
    expectEqual true (deprecated p)

  _ <- test "extracts generic enums and chained links" do
    expectEqual (Just [ "Guest", "Member", "Admin" ]) (enum (Proxy :: Proxy (Enum Access)))
    let
      metadata = links (Proxy :: Proxy (((((String :# "getItem") "getItemById" (id :: "$response.body#/id")) :# "getOwner") "getOwnerById" (id :: "$response.body#/ownerId"))))
    expectEqual 2 (length metadata)
    expectEqual [ "getOwner", "getItem" ] (map _.name metadata)
    expectEqual [ "getOwnerById", "getItemById" ] (map _.operationId metadata)
    expectToBe true (String.contains (String.Pattern "$response.body#/ownerId") (writeJSON metadata))

  _ <- test "maps every request content encoding" do
    expectEqual
      [ "", "application/json", "application/x-www-form-urlencoded", "multipart/form-data", "text/plain", "application/xml", "application/vnd.test+json", "application/json" ]
      [ getContentType (Proxy :: Proxy NoBody)
      , getContentType (Proxy :: Proxy (JSON String))
      , getContentType (Proxy :: Proxy (FormData String))
      , getContentType (Proxy :: Proxy (MultipartFormData String))
      , getContentType (Proxy :: Proxy PlainText)
      , getContentType (Proxy :: Proxy (XML String))
      , getContentType (Proxy :: Proxy (CustomContentType "application/vnd.test+json" String))
      , getContentType (Proxy :: Proxy (Schema "Text" String))
      ]

  _ <- test "unwraps every request encoding to its handler body" do
    expectEqual "json" (decodeBody (Proxy :: Proxy (JSON String)) "json")
    expectEqual "form" (decodeBody (Proxy :: Proxy (FormData String)) "form")
    expectEqual "multipart" (decodeBody (Proxy :: Proxy (MultipartFormData String)) "multipart")
    expectEqual "text" (decodeBody (Proxy :: Proxy PlainText) "text")
    expectEqual "xml" (decodeBody (Proxy :: Proxy (XML String)) "xml")
    expectEqual "custom" (decodeBody (Proxy :: Proxy (CustomContentType "application/vnd.test+json" String)) "custom")

  _ <- test "reports header schema types through metadata wrappers" do
    expectEqual [ "string", "integer", "integer", "string" ]
      [ headerValueType (Proxy :: Proxy String)
      , headerValueType (Proxy :: Proxy Int)
      , headerValueType (Proxy :: Proxy (Maybe Int))
      , headerValueType (Proxy :: Proxy (Description "token" BearerToken))
      ]

  _ <- test "builds explicit server metadata with buildOpenAPISpec'" do
    let
      document = buildOpenAPISpec' @ServerAPI
        (options { title: "Server API", version: "1.0.0" })
        { servers: opt [ { url: "https://api.example.test", description: Just "production" } ] }
      json = writeJSON document
    expectToBe true (String.contains (String.Pattern "\"servers\"") json)
    expectToBe true (String.contains (String.Pattern "https://api.example.test") json)

  test "renders public OpenAPI schema and security seams" do
    expectEqual
      [ 0, 1, 1, 1, 2, 1, 0, 1, 1, 1 ]
      [ length (renderHeadersSchema (Proxy :: Proxy ()))
      , length (renderHeadersSchema (Proxy :: Proxy (requestId :: String)))
      , length (renderCookieParamsSchema (Proxy :: Proxy (theme :: String)))
      , length (renderPathParamsSchema (Proxy :: Proxy (id :: Int)))
      , length (renderQueryParamsSchema (Proxy :: Proxy (limit :: Int, search :: Maybe String)))
      , FObject.size (renderResponseHeadersSchema (Proxy :: Proxy (etag :: String)))
      , length (detectSecurity (Proxy :: Proxy (requestId :: String)))
      , length (detectSecurity (Proxy :: Proxy (authorization :: BearerToken, apiKey :: ApiKeyHeader)))
      , length (detectCookieSecurity (Proxy :: Proxy (session :: ApiKeyCookie)))
      , length (detectCookieSecurity (Proxy :: Proxy (primary :: ApiKeyCookie, secondary :: ApiKeyCookie)))
      ]
    case renderRequestBodySchema (Proxy :: Proxy NoBody) of
      Nothing -> expectToBe true true
      Just _ -> expectToBe "no request body" "unexpected request body"
    case renderRequestBodySchema (Proxy :: Proxy (JSON { name :: String })) of
      Nothing -> expectToBe "request body" "missing"
      Just body -> expectEqual true body.required
    expectToBe true (String.contains (String.Pattern "nullable") (writeJSON (renderJSONSchema (Proxy :: Proxy (Maybe String)))))

testHandlers :: Effect ViTest
testHandlers = describe "route and API handler combinators" do
  _ <- test "mkHandler/runHandler preserve typed path, query, header, and body inputs" do
    response <- runHandler handler
      { path: { id: 7 }
      , query: { limit: 5, search: Just "red" }
      , headers: { requestId: "req" }
      , body: { suffix: "done" }
      }
    expectEqual "req:7:5:(Just \"red\"):done" (Variant.match { ok: \(Response r) -> r.body } response)

  _ <- test "apiHandlers preserves a complete named handler record" do
    let handlers = apiHandlers @{ getItem :: HandlerRoute } { getItem: handler }
    response <- runHandler handlers.getItem
      { path: { id: 1 }
      , query: { limit: 2, search: Nothing }
      , headers: { requestId: "r" }
      , body: { suffix: "x" }
      }
    expectEqual "r:1:2:Nothing:x" (Variant.match { ok: \(Response r) -> r.body } response)

  test "Request and NoRequest wrappers compute executable handler inputs" do
    wrapped <- runHandler wrappedHandler { path: {}, query: {}, headers: {}, body: "form body" }
    healthy <- runHandler noRequestHandler { path: {}, query: {}, headers: {}, body: unit }
    expectEqual "form body" (Variant.match { ok: \(Response r) -> r.body } wrapped)
    expectEqual "healthy" (Variant.match { ok: \(Response r) -> r.body } healthy)

spec :: Effect ViTest
spec = do
  _ <- testMethodsAndPaths
  _ <- testHeadersAndAuth
  _ <- testStatusesAndResponses
  _ <- testMetadataAndOpenAPI
  testHandlers

