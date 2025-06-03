module PureStack.Server
  ( Server
  , serve
  , class ServeAPI
  , serveAPI
  , Nt(..)
  ) where

import Prelude
import PureStack.Route

import Bun.Request (Request)
import Bun.Request as Request
import Bun.Response (Response)
import Bun.Response as Response
import Control.Alternative (class Plus, empty)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.URL as URL
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Prim.Row (class Lacks)
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RowList
import Record as Record
import Type.Proxy (Proxy(..))

type Route = { path :: Array String, verb :: String }

routeFromRequest :: Request -> Route
routeFromRequest req =
  { verb: Request.method req
  , path: case URL.fromString (Request.url req) of
      Nothing -> []
      Just url -> case URL.path url of
        URL.PathEmpty -> []
        URL.PathAbsolute p -> p
        URL.PathRelative p -> p
  }

type Server a = ExceptT Response Aff a

newtype Nt m = Nt (forall x. m x -> Server x)

class ServeRoute :: forall k. k -> Type -> (Type -> Type) -> Constraint
class ServeRoute route handler m | route -> handler, handler -> m where
  serveRoute :: Route -> Maybe (Nt m -> handler -> Request -> Server Response)

instance (ServeRoute rest handler m, IsSymbol path) => ServeRoute (path / rest) handler m where
  serveRoute { path, verb } = do
    { head: h, tail } <- Array.uncons path
    assert $ h == (reflectSymbol @path Proxy)
    serveRoute @rest { path: tail, verb: verb }

else instance (ServeRoute rest handler m, ParsePathPiece t) => ServeRoute (t / rest) (t -> handler) m where
  serveRoute { path, verb } = do
    { head: h, tail } <- Array.uncons path
    t <- parsePathPiece @t h
    serveRoute @rest { path: tail, verb } <#> (\f nt handler req -> f nt (handler t) req)

else instance ToResponse resp => ServeRoute (GET resp) (m resp) m where
  serveRoute { path, verb } = do
    assert $ verb == "GET"
    assert $ path == []
    pure $ \(Nt nt) handler _req -> do
      resp <- nt handler
      pure $ toResponse resp

else instance (ToResponse resp) => ServeRoute (POST Unit resp) (m resp) m where
  serveRoute { path, verb } = do
    assert $ path == []
    assert $ verb == "POST"
    pure $ \(Nt nt) handler _req -> do
      resp <- nt handler
      pure $ toResponse resp

else instance (FromRequest req, ToResponse resp) => ServeRoute (POST req resp) (req -> m resp) m where
  serveRoute { path, verb } = do
    assert $ path == []
    assert $ verb == "POST"
    pure $ \(Nt nt) handler req -> do
      liftAff (fromRequest req) >>= case _ of
        Nothing -> pure $ badRequest
        Just r -> do
          resp <- nt $ handler r
          pure $ toResponse resp

class ToResponse r where
  toResponse :: r -> Response

instance EncodeJson (Record row) => ToResponse (Record row) where
  toResponse rec = Response.json (encodeJson rec) { status: 200, statusText: "OK", headers: [] }

instance ToResponse Response where
  toResponse resp = resp

class FromRequest r where
  fromRequest :: Request -> Aff (Maybe r)

instance DecodeJson (Record row) => FromRequest (Record row) where
  fromRequest req = Request.json req <#> \j -> case decodeJson j of
    Right r -> Just r
    Left _ -> Nothing

instance FromRequest Request where
  fromRequest req = pure $ Just req

class ParsePathPiece t where
  parsePathPiece :: String -> Maybe t

assert :: forall m. Applicative m => Plus m => Boolean -> m Unit
assert true = pure unit
assert false = empty

class ServeAPI :: Row Type -> RowList Type -> Row Type -> (Type -> Type) -> Constraint
class ServeAPI row list handlers m | row -> handlers, handlers -> m where
  serveAPI :: Nt m -> Record handlers -> Route -> Request -> Server Response

instance ServeAPI row RowList.Nil () m where
  serveAPI _ _ _ _ = pure notFound

instance
  ( ServeRoute t handler m
  , Row.Cons field t tail row
  , ServeAPI row rest restHandlers m
  , IsSymbol field
  , Row.Cons field handler restHandlers handlers
  , Row.Lacks field restHandlers
  ) =>
  ServeAPI row (RowList.Cons field t rest) handlers m where
  serveAPI nt handlers route@{ path, verb } req =
    case serveRoute @t { path, verb } of
      Nothing -> serveAPI @row @rest @restHandlers nt (Record.delete (Proxy @field) handlers) route req
      Just f -> f nt (Record.get (Proxy @field) handlers) req

notFound :: Response
notFound = Response.string "" { status: 404, statusText: "Not Found", headers: [] }

internalServerError :: Response
internalServerError = Response.string "" { status: 500, statusText: "Internal Server Error", headers: [] }

badRequest :: Response
badRequest = Response.string "" { status: 400, statusText: "Bad Request", headers: [] }

serve
  :: forall @row list handlers m
   . RowToList row list
  => ServeAPI row list handlers m
  => (forall x. m x -> Server x)
  -> Record handlers
  -> Request
  -> Aff Response
serve nt handlers req = do
  res <- runExceptT $ serveAPI @row @list @handlers (Nt nt) handlers (routeFromRequest req) req
  pure case res of
    Left resp -> resp
    Right resp -> resp

-- class RecordNaturalTransformation
--   :: forall k
--    . (k -> Type)
--   -> (k -> Type)
--   -> Row Type
--   -> RowList Type
--   -> Row Type
--   -> Constraint
-- class
--   RecordNaturalTransformation from to row list row'
--   | list from -> row
--   , list to -> row'
--   , row' from -> row
--   , row to -> row where
--   nt' :: (forall a. from a -> to a) -> Record row -> Record row'

-- instance RecordNaturalTransformation from to () RowList.Nil () where
--   nt' _ rec = rec

-- instance
--   ( IsSymbol field
--   , Row.Cons field t tail row
--   , Row.Cons field t' tail' row'
--   , FunctionNaturalTransformation from to t t'
--   , RecordNaturalTransformation from to tail rest tail'
--   , Lacks field tail
--   , Lacks field tail'
--   ) =>
--   RecordNaturalTransformation from to row (RowList.Cons field t rest) row' where
--   nt' f rec = Record.insert (Proxy @field) (ntFunction f $ Record.get (Proxy @field) rec)
--     $ nt' @from @to @tail @rest @tail' f (Record.delete (Proxy @field) rec)

-- class FunctionNaturalTransformation :: forall k. (k -> Type) -> (k -> Type) -> Type -> Type -> Constraint
-- class FunctionNaturalTransformation from to func func' | to func -> func' where
--   ntFunction :: (forall a. from a -> to a) -> func -> func'

-- instance FunctionNaturalTransformation from to (from x) (to x) where
--   ntFunction f g = f g

-- else instance
--   FunctionNaturalTransformation from to func func' =>
--   FunctionNaturalTransformation from to (a -> func) (a -> func') where
--   ntFunction f g = \a -> ntFunction f (g a)

-- nt
--   :: forall @from row row' list
--    . RowToList row list
--   => RecordNaturalTransformation from Aff row list row'
--   => (forall a. from a -> Aff a)
--   -> Record row
--   -> Record row'
-- nt f rec = nt' @from @Aff @row @list @row' f rec
