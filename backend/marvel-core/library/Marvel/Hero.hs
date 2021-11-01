{-# LANGUAGE TemplateHaskell #-}
module Marvel.Hero where

import Marvel.Prelude

import Marvel.Ability
import Marvel.AlterEgo.Attrs
import Marvel.Card.Builder
import Marvel.Card.Code
import Marvel.Hero.Attrs
import Marvel.Hero.Heroes
import Marvel.Message
import Marvel.TH

$(buildEntity "Hero")

instance RunMessage Hero where
  runMessage = genericRunMessage

instance HasAbilities Hero where
  getAbilities = genericGetAbilities

allHeroes :: HashMap CardCode (IdentityId -> Hero)
allHeroes =
  fromList $ map (toCardCode &&& cbCardBuilder) $(buildEntityLookupList "Hero")

instance HasStartingHP Hero where
  startingHP = defaultHasStartingHP

instance HasCardCode Hero where
  toCardCode = genericToCardCode
