{-# LANGUAGE TemplateHaskell #-}
module Marvel.Minion where

import Marvel.Prelude

import Marvel.Card.Builder
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Entity
import Marvel.Id
import Marvel.Message
import Marvel.Minion.Attrs
import Marvel.Minion.Minions
import Marvel.Source
import Marvel.TH

$(buildEntity "Minion")

allMinions :: HashMap CardCode (IdentityId -> MinionId -> Minion)
allMinions = fromList $ map
  (toCardCode &&& (curry . cbCardBuilder))
  $(buildEntityLookupList "Minion")

lookupMinion :: CardCode -> (IdentityId -> MinionId -> Minion)
lookupMinion cardCode = case lookup cardCode allMinions of
  Just f -> f
  Nothing -> error $ "Invalid card code for minion " <> show cardCode

getMinionDamage :: Minion -> Natural
getMinionDamage = minionDamage . toAttrs

getMinionEngagedIdentity :: Minion -> IdentityId
getMinionEngagedIdentity = minionEngagedIdentity . toAttrs

instance Entity Minion where
  type EntityId Minion = MinionId
  type EntityAttrs Minion = MinionAttrs
  toId = toId . toAttrs
  toAttrs = genericToAttrs

instance RunMessage Minion where
  runMessage = genericRunMessage

instance IsSource Minion where
  toSource = MinionSource . toId

instance HasCardDef Minion where
  getCardDef = getCardDef . toAttrs
