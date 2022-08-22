{-# OPTIONS_GHC -Wno-orphans #-}
module Marvel.Effect where

import Marvel.Prelude

import Marvel.Ally.Allies
import Marvel.Card.Builder
import Marvel.Card.Code
import Marvel.Effect.Types
import Marvel.Event.Events
import Marvel.Id
import Marvel.Matchers
import Marvel.Source
import Marvel.Support.Supports
import Marvel.Upgrade.Upgrades

instance FromJSON Effect where
  parseJSON = withObject "Effect" $ \o -> do
    cCode <- o .: "effectCardCode"
    case lookup cCode allEffects of
      Nothing -> error $ "Invalid effect: " <> show cCode
      Just (SomeCardEffect (_ :: CardEffect a)) ->
        Effect <$> parseJSON @a (Object o)

allEffects :: HashMap CardCode SomeCardEffect
allEffects =
  fromList
    $ [ ("01039", SomeCardEffect rocketBootsEffect)
      , ("01068", SomeCardEffect visionEffect)
      , ("01070", SomeCardEffect leadFromTheFrontEffect)
      , ("01092", SomeCardEffect helicarrierEffect)
      ]

lookupEffect :: CardCode -> Source -> EntityMatcher -> EffectId -> Effect
lookupEffect cardCode source entityMatcher effectId =
  case lookup cardCode allEffects of
    Just (SomeCardEffect a) ->
      Effect $ cbCardBuilder a (source, entityMatcher, effectId)
    Nothing -> error $ "Invalid card code for effect " <> show cardCode
