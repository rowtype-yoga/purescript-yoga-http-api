-- | Ergonomic APIs with optional fields.
-- |
-- | Define a row type with `f`-wrapped optional fields, then use `options` to
-- | accept any record that provides a subset of those fields:
-- |
-- | ```purescript
-- | type MyConfigR f =
-- |   ( host :: String
-- |   , port :: Int
-- |   , ssl :: f Boolean
-- |   )
-- |
-- | myFunction :: forall r. Options r (MyConfigR UndefinedOr) => { | r } -> Result
-- | myFunction given = myFunctionImpl (options @(MyConfigR UndefinedOr) given)
-- | ```
-- |
-- | Callers just provide the fields they want:
-- | ```purescript
-- | myFunction { host: "localhost", port: 8080 }
-- | myFunction { host: "localhost", port: 8080, ssl: true }
-- | ```
module Yoga.HTTP.API.UnionTrick
  ( class Options
  , options
  , class OptionsRL
  , class LookupField
  , class OptionsFieldType
  , uorToMaybe
  , module ReExports
  ) where

import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Literals.Undefined (Undefined)
import Prim.RowList (class RowToList, RowList, Cons, Nil)
import Untagged.Union (UndefinedOr)
import Untagged.Union (UndefinedOr) as ReExports
import Unsafe.Coerce (unsafeCoerce)

class Options :: Row Type -> Row Type -> Constraint
class Options given full

instance (RowToList given givenRL, RowToList full fullRL, OptionsRL givenRL fullRL) => Options given full

options :: forall @full given. Options given full => { | given } -> { | full }
options = unsafeCoerce

class OptionsRL :: RowList Type -> RowList Type -> Constraint
class OptionsRL givenRL fullRL

instance OptionsRL Nil fullRL

instance
  ( LookupField label fullRL fullType
  , OptionsFieldType givenType fullType
  , OptionsRL givenRL fullRL
  ) =>
  OptionsRL (Cons label givenType givenRL) fullRL

class LookupField :: Symbol -> RowList Type -> Type -> Constraint
class LookupField label rl ty | label rl -> ty

instance LookupField label (Cons label ty tail) ty
else instance LookupField label tail ty => LookupField label (Cons other ty' tail) ty

class OptionsFieldType :: Type -> Type -> Constraint
class OptionsFieldType from to

instance OptionsFieldType Undefined a
else instance (RowToList r1 rl1, RowToList r2 rl2, OptionsRL rl1 rl2) => OptionsFieldType (Record r1) (UndefinedOr (Record r2))
else instance OptionsFieldType a (UndefinedOr a)
else instance OptionsFieldType a (Identity a)
else instance OptionsFieldType a a

foreign import uorToMaybeImpl :: forall a. (forall b. b -> Maybe b) -> (forall b. Maybe b) -> UndefinedOr a -> Maybe a

uorToMaybe :: forall a. UndefinedOr a -> Maybe a
uorToMaybe = uorToMaybeImpl Just Nothing
