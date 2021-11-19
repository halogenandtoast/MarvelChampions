{-# LANGUAGE TemplateHaskell #-}
module Marvel.SideScheme.Attrs where

import Marvel.Prelude

import Marvel.Card.Builder
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Entity
import Marvel.Id
import Marvel.Message
import Marvel.Source
import Marvel.Target

class IsSideScheme a

type SideSchemeCard a = CardBuilder SideSchemeId a

data SideSchemeAttrs = SideSchemeAttrs
  { sideSchemeId :: SideSchemeId
  , sideSchemeCardDef :: CardDef
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

makeLensesWith suffixedFields ''SideSchemeAttrs

instance HasCardCode SideSchemeAttrs where
  toCardCode = toCardCode . sideSchemeCardDef

sideScheme :: (SideSchemeAttrs -> a) -> CardDef -> CardBuilder SideSchemeId a
sideScheme f cardDef = CardBuilder
  { cbCardCode = cdCardCode cardDef
  , cbCardBuilder = \mid ->
    f $ SideSchemeAttrs { sideSchemeId = mid, sideSchemeCardDef = cardDef }
  }

instance Entity SideSchemeAttrs where
  type EntityId SideSchemeAttrs = SideSchemeId
  type EntityAttrs SideSchemeAttrs = SideSchemeAttrs
  toId = sideSchemeId
  toAttrs = id

instance IsSource SideSchemeAttrs where
  toSource = SideSchemeSource . toId

instance IsTarget SideSchemeAttrs where
  toTarget = SideSchemeTarget . toId

isTarget :: (Entity a, EntityAttrs a ~ SideSchemeAttrs) => a -> Target -> Bool
isTarget a = (== toTarget (toAttrs a))

instance RunMessage SideSchemeAttrs where
  runMessage _ = pure
