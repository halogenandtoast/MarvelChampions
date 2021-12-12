{-# LANGUAGE TemplateHaskell #-}

module Marvel.AlterEgo where

import Marvel.Prelude

import Marvel.Ability
import Marvel.AlterEgo.AlterEgos
import Marvel.AlterEgo.Attrs
import Marvel.Card.Builder
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Hand
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Source
import Marvel.TH
import Marvel.Trait

$(buildEntity "AlterEgo")

instance RunMessage AlterEgo where
  runMessage = genericRunMessage

instance HasTraits AlterEgo where
  getTraits = pure . cdTraits . getCardDef

instance HasAbilities AlterEgo where
  getAbilities a = genericGetAbilities a <> basicAbilities
   where
    basicAbilities = [ability a 200 Basic NoCriteria ExhaustCost Recover]

allAlterEgos :: HashMap CardCode (IdentityId -> AlterEgo)
allAlterEgos =
  fromList $
    map (toCardCode &&& cbCardBuilder) $(buildEntityLookupList "AlterEgo")

instance HasStartingHP AlterEgo where
  startingHP = startingHP . toAttrs

instance HasHandSize AlterEgo where
  handSize = handSize . toAttrs

instance HasCardCode AlterEgo where
  toCardCode = toCardCode . toAttrs

instance HasCardDef AlterEgo where
  getCardDef = getCardDef . toAttrs

instance IsSource AlterEgo where
  toSource = toSource . toAttrs

instance Entity AlterEgo where
  type EntityId AlterEgo = IdentityId
  type EntityAttrs AlterEgo = AlterEgoAttrs
  toId = toId . toAttrs
  toAttrs = genericToAttrs

instance HasModifiersFor AlterEgo where
  getModifiersFor = genericGetModifiersFor
