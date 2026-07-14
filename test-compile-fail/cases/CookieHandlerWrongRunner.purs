module Test.CompileFail.CookieHandlerWrongRunner where

import Prelude

import Effect.Aff (Aff)
import Yoga.HTTP.API.Route
  ( ApiKeyCookie
  , GET
  , Handler
  , Route
  , mkHandler
  , respondNoHeaders
  , runHandler
  )

type CookieRoute =
  Route GET "cookie" { cookies :: { session :: ApiKeyCookie } } (ok :: { body :: String })

handler :: Handler CookieRoute
handler = mkHandler \{ cookies } ->
  pure $ respondNoHeaders @"ok" cookies.session

wrong :: Aff _
wrong = runHandler handler
  { path: {}
  , query: {}
  , headers: {}
  , body: unit
  }
