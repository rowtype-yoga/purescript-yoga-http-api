module Test.ContentTypeTest where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String as String
import Effect (Effect)
import Yoga.HTTP.API.Path (Path, Lit)
import Yoga.HTTP.API.Route (GET, POST, PUT, Route, Request, buildOpenAPISpec)
import Yoga.HTTP.API.Route.Encoding (JSON, FormData, MultipartFormData, PlainText, XML, CustomContentType)
import Yoga.JSON (writeJSON)
import ViTest (ViTest, describe, test)
import ViTest.Expect (expectToBe)

type User = { name :: String, email :: String }

type FileUpload = { filename :: String, content :: String }

type XmlDocument = { root :: String }

testRequestContentTypes :: Effect ViTest
testRequestContentTypes = describe "Request Content Types" do
  _ <- test "JSON request body generates application/json" do
    let
      spec = buildOpenAPISpec @{ createUser :: Route POST (Path (Lit "users")) (Request { body :: JSON User }) (ok :: { body :: User }) }
        { title: "API"
        , version: "1.0.0"
        , description: Nothing
        , contact: Nothing
        , license: Nothing
        }
      json = writeJSON spec
    expectToBe true (String.contains (String.Pattern "\"application/json\"") json)
    expectToBe false (String.contains (String.Pattern "\"application/x-www-form-urlencoded\"") json)

  _ <- test "FormData request body generates application/x-www-form-urlencoded" do
    let
      spec = buildOpenAPISpec @{ createUser :: Route POST (Path (Lit "users")) (Request { body :: FormData User }) (ok :: { body :: JSON User }) }
        { title: "API"
        , version: "1.0.0"
        , description: Nothing
        , contact: Nothing
        , license: Nothing
        }
      json = writeJSON spec
    expectToBe true (String.contains (String.Pattern "\"application/x-www-form-urlencoded\"") json)
    expectToBe true (String.contains (String.Pattern "\"application/json\"") json)

  _ <- test "MultipartFormData request body generates multipart/form-data" do
    let
      spec = buildOpenAPISpec @{ uploadFile :: Route POST (Path (Lit "upload")) (Request { body :: MultipartFormData FileUpload }) (ok :: { body :: { success :: Boolean } }) }
        { title: "API"
        , version: "1.0.0"
        , description: Nothing
        , contact: Nothing
        , license: Nothing
        }
      json = writeJSON spec
    expectToBe true (String.contains (String.Pattern "\"multipart/form-data\"") json)

  _ <- test "PlainText request body generates text/plain" do
    let
      spec = buildOpenAPISpec @{ echo :: Route POST (Path (Lit "echo")) (Request { body :: PlainText String }) (ok :: { body :: PlainText String }) }
        { title: "API"
        , version: "1.0.0"
        , description: Nothing
        , contact: Nothing
        , license: Nothing
        }
      json = writeJSON spec
    expectToBe true (String.contains (String.Pattern "\"text/plain\"") json)

  _ <- test "XML request body generates application/xml" do
    let
      spec = buildOpenAPISpec @{ updateDoc :: Route PUT (Path (Lit "document")) (Request { body :: XML XmlDocument }) (ok :: { body :: XML XmlDocument }) }
        { title: "API"
        , version: "1.0.0"
        , description: Nothing
        , contact: Nothing
        , license: Nothing
        }
      json = writeJSON spec
    expectToBe true (String.contains (String.Pattern "\"application/xml\"") json)

  test "CustomContentType request body generates custom MIME type" do
    let
      spec = buildOpenAPISpec @{ customEndpoint :: Route POST (Path (Lit "custom")) (Request { body :: CustomContentType "application/vnd.api+json" User }) (ok :: { body :: User }) }
        { title: "API"
        , version: "1.0.0"
        , description: Nothing
        , contact: Nothing
        , license: Nothing
        }
      json = writeJSON spec
    expectToBe true (String.contains (String.Pattern "\"application/vnd.api+json\"") json)

testResponseContentTypes :: Effect ViTest
testResponseContentTypes = describe "Response Content Types" do
  _ <- test "JSON response body generates application/json" do
    let
      spec = buildOpenAPISpec @{ getUser :: Route GET (Path (Lit "users")) (Request {}) (ok :: { body :: JSON User }) }
        { title: "API"
        , version: "1.0.0"
        , description: Nothing
        , contact: Nothing
        , license: Nothing
        }
      json = writeJSON spec
    expectToBe true (String.contains (String.Pattern "\"application/json\"") json)

  _ <- test "PlainText response body generates text/plain" do
    let
      spec = buildOpenAPISpec @{ getText :: Route GET (Path (Lit "text")) (Request {}) (ok :: { body :: PlainText String }) }
        { title: "API"
        , version: "1.0.0"
        , description: Nothing
        , contact: Nothing
        , license: Nothing
        }
      json = writeJSON spec
    expectToBe true (String.contains (String.Pattern "\"text/plain\"") json)

  _ <- test "XML response body generates application/xml" do
    let
      spec = buildOpenAPISpec @{ getDoc :: Route GET (Path (Lit "document")) (Request {}) (ok :: { body :: XML XmlDocument }) }
        { title: "API"
        , version: "1.0.0"
        , description: Nothing
        , contact: Nothing
        , license: Nothing
        }
      json = writeJSON spec
    expectToBe true (String.contains (String.Pattern "\"application/xml\"") json)

  _ <- test "CustomContentType response body generates custom MIME type" do
    let
      spec = buildOpenAPISpec @{ customGet :: Route GET (Path (Lit "custom")) (Request {}) (ok :: { body :: CustomContentType "application/vnd.api+json" User }) }
        { title: "API"
        , version: "1.0.0"
        , description: Nothing
        , contact: Nothing
        , license: Nothing
        }
      json = writeJSON spec
    expectToBe true (String.contains (String.Pattern "\"application/vnd.api+json\"") json)

  test "Unwrapped response body defaults to application/json for backward compatibility" do
    let
      spec = buildOpenAPISpec @{ getUser :: Route GET (Path (Lit "users")) (Request {}) (ok :: { body :: User }) }
        { title: "API"
        , version: "1.0.0"
        , description: Nothing
        , contact: Nothing
        , license: Nothing
        }
      json = writeJSON spec
    expectToBe true (String.contains (String.Pattern "\"application/json\"") json)

testMixedContentTypes :: Effect ViTest
testMixedContentTypes = describe "Mixed Content Types in API" do
  test "API with different content types per route" do
    let
      spec = buildOpenAPISpec
        @
        { createUser :: Route POST (Path (Lit "users")) (Request { body :: JSON User }) (ok :: { body :: JSON User })
        , uploadFile :: Route POST (Path (Lit "upload")) (Request { body :: MultipartFormData FileUpload }) (ok :: { body :: { success :: Boolean } })
        , echo :: Route POST (Path (Lit "echo")) (Request { body :: PlainText String }) (ok :: { body :: PlainText String })
        , getDoc :: Route GET (Path (Lit "document")) (Request {}) (ok :: { body :: XML XmlDocument })
        }
        { title: "Multi-Content API"
        , version: "1.0.0"
        , description: Nothing
        , contact: Nothing
        , license: Nothing
        }
      json = writeJSON spec
    expectToBe true (String.contains (String.Pattern "\"application/json\"") json)
    expectToBe true (String.contains (String.Pattern "\"multipart/form-data\"") json)
    expectToBe true (String.contains (String.Pattern "\"text/plain\"") json)
    expectToBe true (String.contains (String.Pattern "\"application/xml\"") json)
