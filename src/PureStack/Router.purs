module PureStack.Router
  ( run
  , type (/)
  , class RunAPI
  , runAPI
  , Slash
  , GET
  , POST
  , PUT
  , DELETE
  , PATCH
  , class ParseRoute
  , parseRoute
  , class ToResponse
  , toResponse
  ) where

import Prelude
import Record

import Bun.Request (Request)
import Bun.Request as Request
import Bun.Response (Response)
import Bun.Response as Response
import Control.Alternative (class Plus, empty)
import Data.Argonaut (class EncodeJson, Json, encodeJson)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.URL as URL
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Class (liftEffect)
import Prim.Row as Row
import Prim.RowList (class RowToList)
import Prim.RowList as RowList
import Record as Record
import Type.Prelude (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

class Verb :: Type -> Type -> Constraint
class Verb t resp | t -> resp where
  verb :: String

data GET :: Type -> Type
data GET resp

instance Verb (GET resp) resp where
  verb = "GET"

data POST :: Type -> Type -> Type
data POST req resp

instance Verb (POST req resp) resp where
  verb = "POST"

data PUT :: Type -> Type -> Type
data PUT req resp

instance Verb (PUT req resp) resp where
  verb = "PUT"

data DELETE :: Type -> Type -> Type
data DELETE req resp

instance Verb (DELETE req resp) (req -> Effect resp) where
  verb = "DELETE"

data PATCH :: Type -> Type -> Type
data PATCH req resp

instance Verb (PATCH req resp) (req -> Effect resp) where
  verb = "PATCH"

data Slash :: forall k1 k2. k1 -> k2 -> Type
data Slash x y

infixr 6 type Slash as /

type Route = { path :: Array String, verb :: String }

class ParseRoute :: forall k. k -> Type -> Constraint
class ParseRoute route handler | route -> handler where
  parseRoute :: Route -> Maybe (handler -> Request -> Effect Response)

instance (ParseRoute rest handler, IsSymbol path) => ParseRoute (path / rest) handler where
  parseRoute { path, verb } = do
    { head: h, tail } <- Array.uncons path
    assert $ h == (reflectSymbol @path Proxy)
    parseRoute @rest { path: tail, verb: verb }

else instance (ParseRoute rest handler, ParsePathPiece t) => ParseRoute (t / rest) (t -> handler) where
  parseRoute { path, verb } = do
    { head: h, tail } <- Array.uncons path
    t <- parsePathPiece @t h
    parseRoute @rest { path: tail, verb } <#> (\f handler req -> f (handler t) req)

else instance ToResponse resp => ParseRoute (GET resp) (Effect resp) where
  parseRoute { path, verb } = do
    assert $ verb == "GET"
    assert $ path == []
    pure $ \handler _req -> do
      resp <- handler
      pure $ toResponse resp { status: 200, statusText: "OK", headers: [] }

else instance (ToResponse resp) => ParseRoute (POST Unit resp) (Effect resp) where
  parseRoute { path, verb } = do
    assert $ path == []
    assert $ verb == "POST"
    pure $ \handler req -> do
      resp <- handler
      pure $ toResponse resp { status: 200, statusText: "OK", headers: [] }

else instance (FromRequestBody req, ToResponse resp) => ParseRoute (POST req resp) (req -> Effect resp) where
  parseRoute { path, verb } = do
    assert $ path == []
    assert $ verb == "POST"
    pure $ \handler req -> do
      fromRequestBody req >>= case _ of
        Nothing -> pure $ Response.string "" { status: 400, statusText: "Bad Request", headers: [] }
        Just r -> do
          resp <- handler r
          pure $ toResponse resp { status: 200, statusText: "OK", headers: [] }

class ToResponse r where
  toResponse :: r -> Response.Options -> Response

instance EncodeJson (Record row) => ToResponse (Record row) where
  toResponse rec opts = Response.json (encodeJson rec) opts

class FromRequestBody r where
  fromRequestBody :: Request -> Effect (Maybe r)

class ParsePathPiece t where
  parsePathPiece :: String -> Maybe t

assert :: forall m. Applicative m => Plus m => Boolean -> m Unit
assert true = pure unit
assert false = empty

class RunAPI row list handlers | row -> handlers where
  runAPI :: Record handlers -> Request -> Effect Response

instance RunAPI row RowList.Nil () where
  runAPI _ _ = pure notFound

instance
  ( ParseRoute t handler
  , Row.Cons field t tail row
  , RunAPI row rest restHandlers
  , IsSymbol field
  , Row.Cons field handler restHandlers handlers
  , Row.Lacks field restHandlers
  ) =>
  RunAPI row (RowList.Cons field t rest) handlers where
  runAPI handlers req = do
    case URL.fromString (Request.url req) of
      Nothing -> pure internalServerError
      Just url -> do
        let
          path = case URL.path url of
            URL.PathEmpty -> []
            URL.PathAbsolute p -> p
            URL.PathRelative p -> p
          verb = Request.method req
        case parseRoute @t { path, verb } of
          Nothing -> runAPI @row @rest @restHandlers (Record.delete (Proxy @field) handlers) req
          Just f -> f (Record.get (Proxy @field) handlers) req

notFound :: Response
notFound = Response.string "" { status: 404, statusText: "Not Found", headers: [] }

internalServerError :: Response
internalServerError = Response.string "" { status: 500, statusText: "Internal Server Error", headers: [] }

run :: forall @row list handlers. RowToList row list => RunAPI row list handlers => Record handlers -> Request -> Effect Response
run = runAPI @row @list @handlers

---------------------------------------------------------------------------------------------------

