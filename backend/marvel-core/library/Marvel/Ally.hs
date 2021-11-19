{-# LANGUAGE TemplateHaskell #-}
module Marvel.Ally where

import Marvel.Prelude

import Marvel.Ability hiding (Attack, Thwart)
import Marvel.Ally.Allies
import Marvel.Ally.Attrs
import Marvel.Card.Builder
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Id
import Marvel.Message
import Marvel.Question
import Marvel.Source
import Marvel.TH

$(buildEntity "Ally")

allAllies :: HashMap CardCode (IdentityId -> AllyId -> Ally)
allAllies = fromList $ map
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

instance IsSource Ally where
  toSource = AllySource . toId

getAllyController :: Ally -> IdentityId
getAllyController = allyController . toAttrs

instance HasAbilities Ally where
  getAbilities a = genericGetAbilities a <> basicAbilities
   where
    basicAbilities =
      [ ability a 300 Basic NoCriteria ExhaustCost (AllyThwart $ toId a)
      , ability a 301 Basic NoCriteria ExhaustCost (AllyAttack $ toId a)
      ]
