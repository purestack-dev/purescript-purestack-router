module PureStack.Route
  ( type (/)
  , Slash
  , GET
  , POST
  , PUT
  , DELETE
  , PATCH
  ) where

data GET :: Type -> Type
data GET resp

data POST :: Type -> Type -> Type
data POST req resp

data PUT :: Type -> Type -> Type
data PUT req resp

data DELETE :: Type -> Type -> Type
data DELETE req resp

data PATCH :: Type -> Type -> Type
data PATCH req resp

data Slash :: forall k1 k2. k1 -> k2 -> Type
data Slash x y

-- | Operator for separating path pieces when specisying APIs.
-- | `Symbol`s are treated as constant path pieces.
-- | `Record`s are treated as query parameters. `Array`s are lists, `Maybe`s are optional query parameters. 
-- | Other types are captured path pieces.
infixr 1 type Slash as /

data Headers headers r = Headers (Record headers) r
