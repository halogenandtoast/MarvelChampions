{-# LANGUAGE TemplateHaskell #-}
module Marvel.Hero where

import Marvel.Prelude

import Marvel.Ability hiding (Attack, Thwart)
import Marvel.AlterEgo.Attrs
import Marvel.Card.Builder
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Hand
import Marvel.Hero.Attrs
import Marvel.Hero.Heroes
import Marvel.Matchers
import Marvel.Message
import Marvel.Question
import Marvel.Source
import Marvel.TH

$(buildEntity "Hero")

instance RunMessage Hero where
  runMessage = genericRunMessage

instance HasAbilities Hero where
  getAbilities a = genericGetAbilities a <> basicAbilities
   where
    basicAbilities =
      [ ability a 300 Basic (SchemeExists ThwartableScheme) ExhaustCost Thwart
      , ability a 301 Basic (EnemyExists AttackableEnemy) ExhaustCost Attack
      ]

allHeroes :: HashMap CardCode (IdentityId -> Hero)
allHeroes =
  fromList $ map (toCardCode &&& cbCardBuilder) $(buildEntityLookupList "Hero")

instance HasStartingHP Hero where
  startingHP = startingHP . toAttrs

instance HasHandSize Hero where
  handSize = handSize . toAttrs

instance HasCardCode Hero where
  toCardCode = toCardCode . toAttrs

instance HasCardDef Hero where
  getCardDef = getCardDef . toAttrs

instance IsSource Hero where
  toSource = toSource . toAttrs

instance Entity Hero where
  type EntityId Hero = IdentityId
  type EntityAttrs Hero = HeroAttrs
  toId = toId . toAttrs
  toAttrs = genericToAttrs
