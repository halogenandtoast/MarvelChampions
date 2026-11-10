{-# LANGUAGE TemplateHaskell #-}

-- | Compact record codecs. Only fields whose declared type has an unambiguous
-- empty value are omitted; positional values and arbitrary JSON stay untouched.
module Marvel.Json (compactToJSON, compactParseJSON) where

import Marvel.Prelude
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.TH (mkToJSON, mkParseJSON)
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
