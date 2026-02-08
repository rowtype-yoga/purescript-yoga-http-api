-- | A library for the "Union Trick" pattern - ergonomic APIs with optional fields.
-- |
-- | This module provides utilities for creating APIs that accept records with optional
-- | fields without requiring explicit Maybe/Opt constructors.
-- |
-- | Example usage:
-- | ```purescript
-- | type MyConfigR f =
-- |   ( host :: String
-- |   , port :: Int
-- |   , ssl :: f Boolean
-- |   )
-- |
-- | type MyConfigUor = { | MyConfigR UndefinedOr }
-- |
-- | myFunction :: forall r. CoerceConfig r (MyConfigR UndefinedOr) => { | r } -> Result
-- | myFunction given = myFunctionImpl (coerceConfig given)
-- | ```
module Yoga.HTTP.API.UnionTrick
  ( class CoerceConfig
  , coerceConfig
  , class CoerceFields
  , coerceFields
  , coerce
  , class CoerceFieldsRL
  , class LookupField
  , class CoerceFieldType
  , class CoerceToUor
  , coerceToUor
  , uorToMaybe
  , module ReExports
  ) where

import Prelude

import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Literals.Undefined (Undefined)
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList, Cons, Nil)
import Untagged.Union (OneOf, UndefinedOr)
import Untagged.Union (UndefinedOr) as ReExports
import Unsafe.Coerce (unsafeCoerce)

-- | A typeclass for coercing user-provided records to UndefinedOr-based config records.
-- |
-- | This provides a more constrained version of unsafeCoerce that documents the intent
-- | and can have instances for specific patterns.
-- |
-- | The constraint `Union given missing full` ensures that the given record is a subset
-- | of the full expected record type, providing some compile-time safety.
class CoerceConfig :: Row Type -> Row Type -> Constraint
class CoerceConfig given full where
  coerceConfig :: { | given } -> { | full }

-- | Generic instance that works for any record coercion when Union constraint is satisfied.
-- |
-- | This uses unsafeCoerce internally but is constrained to only work when the given
-- | record is a valid subset of the expected record.
instance Row.Union given missing full => CoerceConfig given full where
  coerceConfig = unsafeCoerce

-- | A more sophisticated coercion typeclass that works field-by-field.
-- |
-- | This allows coercing records where each field in `given` can be coerced to the
-- | corresponding field in `full`. For example:
-- |   { name :: String } -> { name :: UndefinedOr String }
-- |
-- | This uses RowList processing to check each field individually.
class CoerceFields :: Row Type -> Row Type -> Constraint
class CoerceFields given full where
  coerceFields :: { | given } -> { | full }

instance (RowToList given givenRL, RowToList full fullRL, CoerceFieldsRL givenRL fullRL) => CoerceFields given full where
  coerceFields = unsafeCoerce

coerce :: forall @full given. CoerceFields given (full (OneOf Undefined)) => { | given } -> { | full (OneOf Undefined) }
coerce = coerceFields

-- | Process RowLists field-by-field to ensure each field can be coerced.
-- | This uses a lookup-based approach: for each field in `given`, we look it up
-- | in `full` by label and check if the types can coerce.
class CoerceFieldsRL :: RowList Type -> RowList Type -> Constraint
class CoerceFieldsRL givenRL fullRL

-- | Base case: empty given record can coerce to anything
instance CoerceFieldsRL Nil fullRL

-- | Recursive case: look up the current field in the full RowList and check coercion
instance
  ( LookupField label fullRL fullType
  , CoerceFieldType givenType fullType
  , CoerceFieldsRL givenRL fullRL
  ) =>
  CoerceFieldsRL (Cons label givenType givenRL) fullRL

-- | Helper typeclass to look up a field by label in a RowList
class LookupField :: Symbol -> RowList Type -> Type -> Constraint
class LookupField label rl ty | label rl -> ty

-- | Found the field with matching label
instance LookupField label (Cons label ty tail) ty

-- | Field not here, continue searching
else instance LookupField label tail ty => LookupField label (Cons other ty' tail) ty

-- | A typeclass for checking if a field type can be coerced.
-- |
-- | This allows:
-- |   - String -> UndefinedOr String
-- |   - { a :: String, b :: Undefined } -> UndefinedOr { a :: String, b :: String }
-- |   - Undefined -> String (for optional nested fields)
-- |   - String -> String (identity)
-- |   - a -> Identity a
-- |   - etc.
class CoerceFieldType :: Type -> Type -> Constraint
class CoerceFieldType from to

-- | Undefined to any type (for optional nested fields)
instance CoerceFieldType Undefined a

-- | Record with potentially Undefined fields to UndefinedOr Record
-- | This handles cases like { name :: String, url :: Undefined } -> UndefinedOr { name :: String, url :: String }
else instance (RowToList r1 rl1, RowToList r2 rl2, CoerceFieldsRL rl1 rl2) => CoerceFieldType (Record r1) (UndefinedOr (Record r2))

-- | Plain type to UndefinedOr
else instance CoerceFieldType a (UndefinedOr a)

-- | Plain type to Identity wrapper
else instance CoerceFieldType a (Identity a)

-- | Identity coercion (most general, last resort)
else instance CoerceFieldType a a

-- | A typeclass for coercing plain values to UndefinedOr-wrapped values.
-- |
-- | This is used for individual fields rather than whole records.
class CoerceToUor :: Type -> Type -> Constraint
class CoerceToUor a b | b -> a where
  coerceToUor :: a -> b

-- | Coerce a plain value to UndefinedOr
instance CoerceToUor a (UndefinedOr a) where
  coerceToUor = unsafeCoerce

-- | Identity functor passes through
else instance CoerceToUor a (Identity a) where
  coerceToUor = Identity

-- | Already wrapped - pass through
else instance CoerceToUor a a where
  coerceToUor = identity

-- | Convert UndefinedOr to Maybe by checking for undefined at runtime.
-- |
-- | This is safer than unsafeCoerce as it properly handles the JavaScript undefined value.
foreign import uorToMaybeImpl :: forall a. (forall b. b -> Maybe b) -> (forall b. Maybe b) -> UndefinedOr a -> Maybe a

-- | Convert UndefinedOr to Maybe.
-- |
-- | Returns Nothing if the value is undefined, Just otherwise.
uorToMaybe :: forall a. UndefinedOr a -> Maybe a
uorToMaybe = uorToMaybeImpl Just Nothing
