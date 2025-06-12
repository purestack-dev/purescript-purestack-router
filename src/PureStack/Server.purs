module PureStack.Server
  ( Server
  , Response(..)
  , ResponseBody(..)
  , class ToResponse
  , toResponse
  , class FromRequest
  , fromRequest
  , run
  , notFound
  , internalServerError
  , badRequest
  -------------------------------------------------------------------------------------------------
  , class ServeAPI
  , serveAPI
  , Nt(..)
  , class ServeRoute
  , serveRoute
  , class ServeQuery
  , serveQuery
  , class ParsePathPiece
  , parsePathPiece
  ) where

import Prelude
import PureStack.Route

import Bun.Request (Request)
import Bun.Request as Request
import Bun.Response as Bun.Response
import Control.Alternative (class Plus, empty)
import Control.Monad.Except (ExceptT, runExceptT)
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson)
import Data.Array as Array
import Data.Either (Either(..), either)
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.String as String
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Traversable (traverse)
import Data.URL as URL
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Foreign.Object (Object)
import Foreign.Object as Object
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RowList
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Proxy (Proxy(..))

type Route = { path :: Array String, verb :: String, query :: Map String (Array String) }

routeFromRequest :: Request -> Route
routeFromRequest req =
  let
    url = URL.fromString $ Request.url req
  in
    { verb: Request.method req
    , path: case url of
        Nothing -> []
        Just u -> case URL.path u of
          URL.PathEmpty -> []
          URL.PathAbsolute p -> p
          URL.PathRelative p -> p
    , query: case url of
        Nothing -> Map.empty
        Just u -> URL.query u
    }

type Server a = ExceptT Response Aff a

newtype Nt m = Nt (forall x. m x -> Server x)

class ServeRoute :: forall k. k -> Type -> (Type -> Type) -> Constraint
class ServeRoute route handler m | route -> handler, handler -> m where
  serveRoute :: Route -> Maybe (Nt m -> handler -> Request -> Server Response)

instance (ServeRoute rest handler m, IsSymbol path) => ServeRoute (path / rest) handler m where
  serveRoute route@{ path } = do
    { head: h, tail } <- Array.uncons path
    assert $ h == (reflectSymbol @path Proxy)
    serveRoute @rest route { path = tail }

else instance
  ( ServeRoute rest handler m
  , ServeQuery row list
  , RowToList row list
  ) =>
  ServeRoute (Record row / rest) (Record row -> handler) m where
  serveRoute route@{ query } = do
    builder <- serveQuery @row @list query
    serveRoute @rest route <#> (\f nt handler req -> f nt (handler $ Builder.buildFromScratch builder) req)

else instance (ServeRoute rest handler m, ParsePathPiece t) => ServeRoute (t / rest) (t -> handler) m where
  serveRoute route@{ path } = do
    { head: h, tail } <- Array.uncons path
    t <- parsePathPiece @t h
    serveRoute @rest route { path = tail } <#> (\f nt handler req -> f nt (handler t) req)

else instance ToResponse resp => ServeRoute (GET resp) (m resp) m where
  serveRoute { path, verb } = do
    assert $ verb == "GET"
    assert $ path == []
    pure $ \(Nt nt) handler _req -> do
      resp <- nt handler
      pure $ toResponse resp

else instance ServeRoute (POST Unit Unit) (m Unit) m where
  serveRoute { path, verb } = do
    assert $ verb == "POST"
    assert $ path == []
    pure $ \(Nt nt) handler _req -> do
      nt handler
      pure $ ok
else instance (ToResponse resp) => ServeRoute (POST Unit resp) (m resp) m where
  serveRoute { path, verb } = do
    assert $ verb == "POST"
    assert $ path == []
    pure $ \(Nt nt) handler _req -> do
      resp <- nt handler
      pure $ toResponse resp
else instance (FromRequest req) => ServeRoute (POST resp Unit) (req -> m Unit) m where
  serveRoute { path, verb } = do
    assert $ verb == "POST"
    assert $ path == []
    pure $ \(Nt nt) handler req -> do
      liftAff (fromRequest req) >>= case _ of
        Nothing -> pure $ badRequest
        Just r -> do
          nt $ handler r
          pure $ ok
else instance (FromRequest req, ToResponse resp) => ServeRoute (POST req resp) (req -> m resp) m where
  serveRoute { path, verb } = do
    assert $ verb == "POST"
    assert $ path == []
    pure $ \(Nt nt) handler req -> do
      liftAff (fromRequest req) >>= case _ of
        Nothing -> pure $ badRequest
        Just r -> do
          resp <- nt $ handler r
          pure $ toResponse resp

else instance ServeRoute (DELETE Unit Unit) (m Unit) m where
  serveRoute { path, verb } = do
    assert $ verb == "DELETE"
    assert $ path == []
    pure $ \(Nt nt) handler _req -> do
      nt handler
      pure $ ok
else instance (ToResponse resp) => ServeRoute (DELETE Unit resp) (m resp) m where
  serveRoute { path, verb } = do
    assert $ verb == "DELETE"
    assert $ path == []
    pure $ \(Nt nt) handler _req -> do
      resp <- nt handler
      pure $ toResponse resp
else instance (FromRequest req) => ServeRoute (DELETE resp Unit) (req -> m Unit) m where
  serveRoute { path, verb } = do
    assert $ verb == "DELETE"
    assert $ path == []
    pure $ \(Nt nt) handler req -> do
      liftAff (fromRequest req) >>= case _ of
        Nothing -> pure $ badRequest
        Just r -> do
          nt $ handler r
          pure $ ok
else instance (FromRequest req, ToResponse resp) => ServeRoute (DELETE req resp) (req -> m resp) m where
  serveRoute { path, verb } = do
    assert $ verb == "DELETE"
    assert $ path == []
    pure $ \(Nt nt) handler req -> do
      liftAff (fromRequest req) >>= case _ of
        Nothing -> pure $ badRequest
        Just r -> do
          resp <- nt $ handler r
          pure $ toResponse resp

else instance ServeRoute (PUT Unit Unit) (m Unit) m where
  serveRoute { path, verb } = do
    assert $ verb == "PUT"
    assert $ path == []
    pure $ \(Nt nt) handler _req -> do
      nt handler
      pure $ ok
else instance (ToResponse resp) => ServeRoute (PUT Unit resp) (m resp) m where
  serveRoute { path, verb } = do
    assert $ verb == "PUT"
    assert $ path == []
    pure $ \(Nt nt) handler _req -> do
      resp <- nt handler
      pure $ toResponse resp
else instance (FromRequest req) => ServeRoute (PUT resp Unit) (req -> m Unit) m where
  serveRoute { path, verb } = do
    assert $ verb == "PUT"
    assert $ path == []
    pure $ \(Nt nt) handler req -> do
      liftAff (fromRequest req) >>= case _ of
        Nothing -> pure $ badRequest
        Just r -> do
          nt $ handler r
          pure $ ok
else instance (FromRequest req, ToResponse resp) => ServeRoute (PUT req resp) (req -> m resp) m where
  serveRoute { path, verb } = do
    assert $ verb == "PUT"
    assert $ path == []
    pure $ \(Nt nt) handler req -> do
      liftAff (fromRequest req) >>= case _ of
        Nothing -> pure $ badRequest
        Just r -> do
          resp <- nt $ handler r
          pure $ toResponse resp

else instance ServeRoute (PATCH Unit Unit) (m Unit) m where
  serveRoute { path, verb } = do
    assert $ verb == "PATCH"
    assert $ path == []
    pure $ \(Nt nt) handler _req -> do
      nt handler
      pure $ ok
else instance (ToResponse resp) => ServeRoute (PATCH Unit resp) (m resp) m where
  serveRoute { path, verb } = do
    assert $ verb == "PATCH"
    assert $ path == []
    pure $ \(Nt nt) handler _req -> do
      resp <- nt handler
      pure $ toResponse resp
else instance (FromRequest req) => ServeRoute (PATCH resp Unit) (req -> m Unit) m where
  serveRoute { path, verb } = do
    assert $ verb == "PATCH"
    assert $ path == []
    pure $ \(Nt nt) handler req -> do
      liftAff (fromRequest req) >>= case _ of
        Nothing -> pure $ badRequest
        Just r -> do
          nt $ handler r
          pure $ ok
else instance (FromRequest req, ToResponse resp) => ServeRoute (PATCH req resp) (req -> m resp) m where
  serveRoute { path, verb } = do
    assert $ verb == "PATCH"
    assert $ path == []
    pure $ \(Nt nt) handler req -> do
      liftAff (fromRequest req) >>= case _ of
        Nothing -> pure $ badRequest
        Just r -> do
          resp <- nt $ handler r
          pure $ toResponse resp

else instance
  ( ServeAPI routes list handlers m
  , RowToList routes list
  ) =>
  ServeRoute routes (Record handlers) m where
  serveRoute route = pure $ \nt handlers req ->
    serveAPI @routes @list nt handlers route req

class ServeQuery :: Row Type -> RowList Type -> Constraint
class ServeQuery row list | list -> row where
  serveQuery :: Map String (Array String) -> Maybe (Builder (Record ()) (Record row))

instance ServeQuery () RowList.Nil where
  serveQuery _ = Just $ identity

instance
  ( ParsePathPiece t
  , ServeQuery tail rest
  , IsSymbol field
  , Row.Cons field (Array t) tail row
  , Row.Lacks field tail
  ) =>
  ServeQuery row (RowList.Cons field (Array t) rest) where
  serveQuery m = do
    r <- serveQuery @tail @rest m
    case Map.lookup (reflectSymbol @field Proxy) m of
      Nothing -> pure $ Builder.insert (Proxy @field) [] <<< r
      Just a -> do
        v <- parsePathPiece @t `traverse` a
        pure $ Builder.insert (Proxy @field) v <<< r

else instance
  ( ParsePathPiece t
  , ServeQuery tail rest
  , IsSymbol field
  , Row.Cons field (Maybe t) tail row
  , Row.Lacks field tail
  ) =>
  ServeQuery row (RowList.Cons field (Maybe t) rest) where
  serveQuery m = do
    r <- serveQuery @tail @rest m
    case Map.lookup (reflectSymbol @field Proxy) m of
      Nothing -> pure $ Builder.insert (Proxy @field) Nothing <<< r
      Just a -> do
        { head, tail } <- Array.uncons a
        assert $ tail == []
        v <- parsePathPiece @t head
        pure $ Builder.insert (Proxy @field) (Just v) <<< r

else instance
  ( ParsePathPiece t
  , ServeQuery tail rest
  , IsSymbol field
  , Row.Cons field t tail row
  , Row.Lacks field tail
  ) =>
  ServeQuery row (RowList.Cons field t rest) where
  serveQuery m = do
    { head, tail } <- Map.lookup (reflectSymbol @field Proxy) m >>= Array.uncons
    assert $ tail == []
    v <- parsePathPiece @t head
    r <- serveQuery @tail @rest m
    pure $ Builder.insert (Proxy @field) v <<< r

data ResponseBody
  = JsonResponseBody Json
  | EmptyResponseBody
  | StringResponseBody String

newtype Response = Response { body :: ResponseBody, status :: Int, statusText :: String, headers :: Object String }

class ToResponse r where
  toResponse :: r -> Response

instance EncodeJson (Record row) => ToResponse (Record row) where
  toResponse rec = Response { body: JsonResponseBody $ encodeJson rec, status: 200, statusText: "OK", headers: Object.empty }

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

instance ParsePathPiece String where
  parsePathPiece = Just

instance ParsePathPiece Int where
  parsePathPiece = Int.fromString

instance ParsePathPiece Number where
  parsePathPiece = Number.fromString

instance ParsePathPiece Boolean where
  parsePathPiece s = case String.toLower s of
    "true" -> Just true
    "1" -> Just true
    "false" -> Just false
    "0" -> Just false
    _ -> Nothing

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
  serveAPI nt handlers route req =
    case serveRoute @t route of
      Nothing -> serveAPI @row @rest @restHandlers nt (Record.delete (Proxy @field) handlers) route req
      Just f -> f nt (Record.get (Proxy @field) handlers) req

notFound :: Response
notFound = Response { body: EmptyResponseBody, status: 404, statusText: "Not Found", headers: Object.empty }

internalServerError :: Response
internalServerError = Response { body: EmptyResponseBody, status: 500, statusText: "Internal Server Error", headers: Object.empty }

badRequest :: Response
badRequest = Response { body: EmptyResponseBody, status: 400, statusText: "Bad Request", headers: Object.empty }

ok :: Response
ok = Response { body: EmptyResponseBody, status: 200, statusText: "OK", headers: Object.empty }

-- | The first argument is used to run an arbitrary monad in handlers. To use the 'Server' monad
-- | instead you can just pass `identity` as the first argument.
run
  :: forall @row list handlers m
   . RowToList row list
  => ServeAPI row list handlers m
  => (forall x. m x -> Server x)
  -> Record handlers
  -> Request
  -> Aff Bun.Response.Response
run nt handlers req = do
  Response { body, headers, status, statusText } <-
    map (either identity identity) $ runExceptT $ serveAPI @row @list @handlers (Nt nt) handlers (routeFromRequest req) req
  let opts = { headers, status, statusText }
  pure $ case body of
    EmptyResponseBody -> Bun.Response.string "" opts
    JsonResponseBody json -> Bun.Response.json json opts
    StringResponseBody string -> Bun.Response.string string opts
