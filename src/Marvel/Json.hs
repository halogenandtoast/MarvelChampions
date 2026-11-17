{-# LANGUAGE TemplateHaskell #-}

-- | Compact record codecs. Only fields whose declared type has an unambiguous
-- empty value are omitted; positional values and arbitrary JSON stay untouched.
-- Parsing puts the empty value back, so a legacy default for a missing field
-- must be that same value.
module Marvel.Json (compactToJSON, compactParseJSON, sparseToJSON, sparseParseJSON) where

import Marvel.Prelude
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.TH (mkToJSON, mkParseJSON)
import Data.Aeson.Types (Key, Parser)
import Data.Kind qualified as Kind
import Data.Proxy (Proxy (..))
import GHC.Generics (C1, D1, K1, Rep, S1, Selector, selName, (:*:))
import Language.Haskell.TH hiding (Type)

emptyFields :: Options -> Name -> Q Exp
emptyFields options name = do
  TyConI (DataD _ _ _ _ [RecC _ fields] _) <- reify name
  let emptyValue = \case
        ConT t | nameBase t == "Entities" -> Just [| object [] |]
        AppT ListT _ -> Just [| toJSON ([] :: [Value]) |]
        AppT (ConT t) _ | nameBase t == "EntityMap" -> Just [| object [] |]
        AppT (ConT t) _ | nameBase t == "Maybe" -> Just [| Null |]
        AppT (AppT (ConT t) _) _ | nameBase t == "HashMap" -> Just [| object [] |]
        _ -> Nothing
  listE
    [ [| (fromString $(litE $ stringL $ fieldLabelModifier options $ nameBase field), $value) |]
    | (field, _, typ) <- fields
    , Just value <- [emptyValue typ]
    ]

compactToJSON :: Options -> Name -> Q Exp
compactToJSON options name =
  [| \value -> case $(mkToJSON options name) value of
       Object fields -> Object $ foldr
         (\(key, empty) result -> if KeyMap.lookup key result == Just empty then KeyMap.delete key result else result)
         fields
         $(emptyFields options name)
       other -> other
   |]

compactParseJSON :: Options -> Name -> Q Exp
compactParseJSON options name =
  [| \value -> $(mkParseJSON options name) $ case value of
       Object fields -> Object $ foldr
         (\(key, empty) -> KeyMap.insertWith (\_ existing -> existing) key empty)
         fields
         $(emptyFields options name)
       other -> other
   |]

-- | Generic record codecs that also leave out false and zero, for records such as
-- a card definition whose flags and counts are almost always unset. Generic
-- rather than Template Haskell so a record in a recursive group can use them.
sparseToJSON :: forall a. (Generic a, GToJSON' Value Zero (Rep a), GEmptyFields (Rep a)) => Options -> a -> Value
sparseToJSON options value = case genericToJSON options value of
  Object fields -> Object $ foldr
    (\(key, empty) result -> if KeyMap.lookup key result == Just empty then KeyMap.delete key result else result)
    fields
    (sparseFields options $ Proxy @a)
  other -> other

sparseParseJSON :: forall a. (Generic a, GFromJSON Zero (Rep a), GEmptyFields (Rep a)) => Options -> Value -> Parser a
sparseParseJSON options value = genericParseJSON options $ case value of
  Object fields -> Object $ foldr
    (\(key, empty) -> KeyMap.insertWith (\_ existing -> existing) key empty)
    fields
    (sparseFields options $ Proxy @a)
  other -> other

sparseFields :: forall a. (GEmptyFields (Rep a)) => Options -> Proxy a -> [(Key, Value)]
sparseFields options _ = [(fromString $ fieldLabelModifier options field, empty) | (field, empty) <- gEmptyFields (Proxy @(Rep a))]

-- | The JSON of a field type's empty value, if it has one.
class EmptyField a where
  emptyField :: Proxy a -> Maybe Value

instance {-# OVERLAPPABLE #-} EmptyField a where emptyField _ = Nothing
instance EmptyField (Maybe a) where emptyField _ = Just Null
instance EmptyField [a] where emptyField _ = Just $ Array mempty
instance EmptyField (HashSet a) where emptyField _ = Just $ Array mempty
instance EmptyField Bool where emptyField _ = Just $ Bool False
instance EmptyField Int where emptyField _ = Just $ Number 0
instance EmptyField Natural where emptyField _ = Just $ Number 0

class GEmptyFields (f :: Kind.Type -> Kind.Type) where
  gEmptyFields :: Proxy f -> [(String, Value)]

instance (GEmptyFields f) => GEmptyFields (D1 c f) where
  gEmptyFields _ = gEmptyFields (Proxy @f)

instance (GEmptyFields f) => GEmptyFields (C1 c f) where
  gEmptyFields _ = gEmptyFields (Proxy @f)

instance (GEmptyFields f, GEmptyFields g) => GEmptyFields (f :*: g) where
  gEmptyFields _ = gEmptyFields (Proxy @f) <> gEmptyFields (Proxy @g)

instance (Selector s, EmptyField a) => GEmptyFields (S1 s (K1 i a)) where
  gEmptyFields _ = [(selName (undefined :: S1 s (K1 i a) ()), empty) | Just empty <- [emptyField (Proxy @a)]]
