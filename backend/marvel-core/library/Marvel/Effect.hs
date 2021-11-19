{-# LANGUAGE TemplateHaskell #-}
module Marvel.Effect where

import Marvel.Prelude

import Marvel.Card.Builder
import Marvel.Card.Code
import Marvel.Effect.Attrs
import Marvel.Entity
import Marvel.Event.Events
import Marvel.Id
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Source
import Marvel.Support.Supports
import Marvel.TH

$(buildEntity "Effect")

instance RunMessage Effect where
  runMessage = genericRunMessage

instance Entity Effect where
  type EntityId Effect = EffectId
  type EntityAttrs Effect = EffectAttrs
  toId = toId . toAttrs
  toAttrs = genericToAttrs

allEffects :: HashMap CardCode (Source -> EntityMatcher -> EffectId -> Effect)
allEffects = fromList $ map
  (toCardCode &&& (curry3 . cbCardBuilder))
  $(buildEntityLookupList "Effect")

lookupEffect :: CardCode -> (Source -> EntityMatcher -> EffectId -> Effect)
lookupEffect cardCode = case lookup cardCode allEffects of
  Just f -> f
  Nothing -> error $ "Invalid card code for effect " <> show cardCode

instance HasModifiersFor Effect where
  getModifiersFor _ target e = do
    valid <- effectValidFor attrs target
    pure $ if valid then effectModifiers attrs else []
    where attrs = toAttrs e
