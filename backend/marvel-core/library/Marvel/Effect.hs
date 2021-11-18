module Marvel.Effect where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Card.Builder
import Marvel.Effect.Attrs
import Marvel.Entity
import Marvel.Id
import Marvel.Message
import Marvel.Source
import Marvel.Target
import Marvel.Support.Supports

newtype Effect = HelicarrierEffect' HelicarrierEffect
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance RunMessage Effect where
  runMessage = genericRunMessage

instance Entity Effect where
  type EntityId Effect = EffectId
  type EntityAttrs Effect = EffectAttrs
  toId = toId . toAttrs
  toAttrs = genericToAttrs

curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f a b c = f (a, b, c)

allEffects :: HashMap CardCode (Source -> Target -> EffectId -> Effect)
allEffects = fromList [("01092", \s t eid -> HelicarrierEffect' $ curry3 (cbCardBuilder helicarrierEffect) s t eid)]

lookupEffect :: CardCode -> (Source -> Target -> EffectId -> Effect)
lookupEffect cardCode = case lookup cardCode allEffects of
  Just f -> f
  Nothing -> error "Invalid card code"
