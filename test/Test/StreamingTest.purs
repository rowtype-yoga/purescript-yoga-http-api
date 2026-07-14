module Test.StreamingTest where

import Prelude

import Data.Array as Array
import Data.Variant as Variant
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Type.Proxy (Proxy(..))
import Yoga.HTTP.API.Route
  ( GET
  , Handler
  , Request
  , Response(..)
  , Route
  , Streaming
  , StreamChunk
  , mkHandler
  , respondNoHeaders
  , streaming
  , textChunk
  , runHandlerWithCookies
  )
import ViTest (ViTest, describe, test)
import ViTest.Expect (expectToBe)
import Yoga.HTTP.API.Route.OpenAPI (getContentType)

foreign import collectImpl :: Streaming StreamChunk -> EffectFnAff (Array String)

type CookieRoute = Route GET "cookie"
  (Request { cookies :: { session :: String } })
  (ok :: { body :: String })

cookieHandler :: Handler CookieRoute
cookieHandler = mkHandler \{ cookies } ->
  pure $ respondNoHeaders @"ok" cookies.session

foreign import readOneAndCancelImpl :: Streaming StreamChunk -> EffectFnAff String

spec :: Effect ViTest
spec = describe "Define streaming responses and typed handlers" do
  test "build a stream whose pull returns chunks and Nothing closes it" do
    index <- liftEffect $ Ref.new 0
    stream <- liftEffect $ streaming
      { pull: do
          current <- liftEffect $ Ref.read index
          liftEffect $ Ref.modify_ (_ + 1) index
          pure $ textChunk <$> Array.index [ "first", "second" ] current
      , cancel: pure unit
      }
    chunks <- fromEffectFnAff (collectImpl stream)
    expectToBe true (chunks == [ "first", "second" ])

  test "put cleanup in cancel so consumers can release resources" do
    cancelled <- liftEffect $ Ref.new false
    stream <- liftEffect $ streaming
      { pull: pure (Just (textChunk "chunk"))
      , cancel: Ref.write true cancelled
      }
    chunk <- fromEffectFnAff (readOneAndCancelImpl stream)
    didCancel <- liftEffect $ Ref.read cancelled
    expectToBe "chunk" chunk
    expectToBe true didCancel

  test "publishes an explicit binary response content type" do
    expectToBe "application/octet-stream"
      (getContentType (Proxy :: Proxy (Streaming StreamChunk)))

  test "Request cookies → mkHandler { cookies }" do
    result <- runHandlerWithCookies cookieHandler
      { path: {}
      , query: {}
      , headers: {}
      , cookies: { session: "abc123" }
      , body: unit
      }
    case Variant.prj (Proxy :: Proxy "ok") result of
      Just (Response response) -> expectToBe "abc123" response.body
      Nothing -> expectToBe true false
