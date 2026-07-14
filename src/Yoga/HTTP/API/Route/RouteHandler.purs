module Yoga.HTTP.API.Route.RouteHandler
  ( Handler
  , class RouteHandler
  , class RouteCookies
  , mkHandler
  , mkHandlerWithCookies
  , runHandler
  , runHandlerWithCookies
  , class APIHandlers
  , apiHandlers
  , class ApiRecord
  ) where

import Prim.Row as Row
import Prim.RowList as RL
import Prim.RowList (class RowToList)
import Unsafe.Coerce (unsafeCoerce)
import Yoga.HTTP.API.Route.Handler
  ( HandlerFn
  , Request
  , HandlerFnWithCookies
  , class DefaultRequestFields
  , class EncodingBody
  , class SegmentPathParams
  , class SegmentQueryParams
  )
import Yoga.HTTP.API.Route.Route (Route, class ConvertResponseVariant)

-- | Type class that computes the handler function type from a Route type.
class
  RouteHandler
    (route :: Type)
    (pathParams :: Row Type)
    (queryParams :: Row Type)
    (reqHeaders :: Row Type)
    (body :: Type)
    (respVariant :: Row Type)
  | route -> pathParams queryParams reqHeaders body respVariant

instance
  ( Row.Union partialRequest o_ (headers :: Record fullHeaders, cookies :: Record fullCookies, body :: fullEncoding)
  , DefaultRequestFields partialRequest fullHeaders fullCookies fullEncoding
  , SegmentPathParams segments pathParams
  , SegmentQueryParams segments queryParams
  , EncodingBody fullEncoding body
  , ConvertResponseVariant userResp respVariant
  ) =>
  RouteHandler
    (Route method segments (Record partialRequest) userResp)
    pathParams
    queryParams
    fullHeaders
    body
    respVariant

else instance
  ( Row.Union partialRequest o_ (headers :: Record fullHeaders, cookies :: Record fullCookies, body :: fullEncoding)
  , DefaultRequestFields partialRequest fullHeaders fullCookies fullEncoding
  , SegmentPathParams segments pathParams
  , SegmentQueryParams segments queryParams
  , EncodingBody fullEncoding body
  , ConvertResponseVariant userResp respVariant
  ) =>
  RouteHandler
    (Route method segments (Request (Record partialRequest)) userResp)
    pathParams
    queryParams
    fullHeaders
    body
    respVariant

-- | Compute the cookie row declared by a route request.
class RouteCookies (route :: Type) (cookies :: Row Type) | route -> cookies

instance
  DefaultRequestFields partialRequest headers cookies encoding =>
  RouteCookies
    (Route method segments (Record partialRequest) response)
    cookies

else instance
  DefaultRequestFields partialRequest headers cookies encoding =>
  RouteCookies
    (Route method segments (Request (Record partialRequest)) response)
    cookies

-- | A handler tied to a specific route type.
-- |
-- | Usage:
-- |   userHandler :: Handler UserRoute
-- |   userHandler = mkHandler \{ path } -> ...
foreign import data Handler :: Type -> Type

-- | Create a handler from the route's complete typed request.
-- | The cookie row is inferred from the route, so cookie-aware routes need no
-- | alternate constructor.
mkHandler
  :: forall route pathParams queryParams reqHeaders reqCookies body respVariant
   . RouteHandler route pathParams queryParams reqHeaders body respVariant
  => RouteCookies route reqCookies
  => HandlerFnWithCookies pathParams queryParams reqHeaders reqCookies body respVariant
  -> Handler route
mkHandler = unsafeCoerce

-- | Compatibility name for cookie-aware handlers. Prefer `mkHandler`.
mkHandlerWithCookies
  :: forall route pathParams queryParams reqHeaders reqCookies body respVariant
   . RouteHandler route pathParams queryParams reqHeaders body respVariant
  => RouteCookies route reqCookies
  => HandlerFnWithCookies pathParams queryParams reqHeaders reqCookies body respVariant
  -> Handler route
mkHandlerWithCookies = mkHandler

-- | Run a handler for a route whose cookie row is empty.
-- | Cookie-bearing routes must use `runHandlerWithCookies`.
runHandler
  :: forall route pathParams queryParams reqHeaders body respVariant
   . RouteHandler route pathParams queryParams reqHeaders body respVariant
  => RouteCookies route ()
  => Handler route
  -> HandlerFn pathParams queryParams reqHeaders body respVariant
runHandler handler { path, query, headers, body } =
  (unsafeCoerce handler) { path, query, headers, cookies: {}, body }

-- | Run the canonical cookie-aware handler representation.
runHandlerWithCookies
  :: forall route pathParams queryParams reqHeaders reqCookies body respVariant
   . RouteHandler route pathParams queryParams reqHeaders body respVariant
  => RouteCookies route reqCookies
  => Handler route
  -> HandlerFnWithCookies pathParams queryParams reqHeaders reqCookies body respVariant
runHandlerWithCookies = unsafeCoerce

--------------------------------------------------------------------------------
-- APIHandlers: Map an API record row to a handler record row
--------------------------------------------------------------------------------

class APIHandlers (rl :: RL.RowList Type) (handlerRow :: Row Type) | rl -> handlerRow

instance APIHandlers RL.Nil ()

instance
  ( APIHandlers tail tailRow
  , Row.Cons label (Handler (Route method segments request resp)) tailRow handlerRow
  , Row.Lacks label tailRow
  ) =>
  APIHandlers (RL.Cons label (Route method segments request resp) tail) handlerRow

class ApiRecord (api :: Type) (row :: Row Type) | api -> row

instance ApiRecord (Record row) row

apiHandlers
  :: forall @api apiRow rl handlerRow
   . ApiRecord api apiRow
  => RowToList apiRow rl
  => APIHandlers rl handlerRow
  => Record handlerRow
  -> Record handlerRow
apiHandlers handlers = handlers
