{-# LANGUAGE TemplateHaskell #-}

module Marvel.Ally where

import Marvel.Prelude

import Marvel.Ability hiding (Attack, Thwart)
import Marvel.Ally.Allies
import Marvel.Ally.Attrs
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Id
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Source
import Marvel.Stats
import Marvel.TH

$(buildEntity "Ally")

allAllies :: HashMap CardCode (IdentityId -> AllyId -> Ally)
allAllies =
  fromList $
    map
      (toCardCode &&& (curry . cbCardBuilder))
      $(buildEntityLookupList "Ally")

lookupAlly :: CardCode -> (IdentityId -> AllyId -> Ally)
lookupAlly cardCode = case lookup cardCode allAllies of
  Just f -> f
  Nothing -> error $ "Invalid card code for ally " <> show cardCode

instance Entity Ally where
  type EntityId Ally = AllyId
  type EntityAttrs Ally = AllyAttrs
  toId = toId . toAttrs
  toAttrs = genericToAttrs

instance RunMessage Ally where
  runMessage = genericRunMessage

instance Exhaustable Ally where
  isExhausted = allyExhausted . toAttrs

instance IsCard Ally where
  toCard = toCard . toAttrs

instance IsSource Ally where
  toSource = AllySource . toId

getAllyController :: Ally -> IdentityId
getAllyController = allyController . toAttrs

getAllyUses :: Ally -> Natural
getAllyUses = allyCounters . toAttrs

getAllyDamage :: Ally -> Natural
getAllyDamage = allyDamage . toAttrs

instance HasCardDef Ally where
  getCardDef = getCardDef . toAttrs

instance HasModifiersFor Ally where
  getModifiersFor = genericGetModifiersFor

instance HasAbilities Ally where
  getAbilities a = genericGetAbilities a <> basicAbilities
   where
    basicAbilities =
      [ ability
        a
        300
        Basic
        (SchemeExists ThwartableScheme)
        ExhaustCost
        (AllyThwart $ toId a)
      | unThw (allyThwart $ toAttrs a) > 0
      ]
        <> [ ability
              a
              301
              Basic
              (EnemyExists AttackableEnemy)
              ExhaustCost
              (AllyAttack $ toId a)
           ]
