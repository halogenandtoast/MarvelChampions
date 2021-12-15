{-# LANGUAGE TemplateHaskell #-}

module Marvel.Minion where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attack
import Marvel.Card
import Marvel.Id
import Marvel.Minion.Attrs
import Marvel.Minion.Minions
import Marvel.TH

$(buildEntity "Minion")

allMinions :: HashMap CardCode (IdentityId -> MinionId -> Minion)
allMinions =
  fromList $
    map
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

getMinionPrintedHitPoints :: Minion -> HP Natural
getMinionPrintedHitPoints = minionHitPoints . toAttrs

minionAttackDetails :: Minion -> Maybe Attack
minionAttackDetails = minionAttacking . toAttrs

instance Entity Minion where
  type EntityId Minion = MinionId
  type EntityAttrs Minion = MinionAttrs
  toId = toId . toAttrs
  toAttrs = genericToAttrs

instance RunMessage Minion where
  runMessage = genericRunMessage

instance HasAbilities Minion where
  getAbilities = genericGetAbilities

instance HasModifiersFor Minion where
  getModifiersFor = genericGetModifiersFor

instance IsSource Minion where
  toSource = MinionSource . toId

instance IsTarget Minion where
  toTarget = MinionTarget . toId

instance IsCard Minion where
  toCard = toCard . toAttrs

instance HasCardDef Minion where
  getCardDef = getCardDef . toAttrs
