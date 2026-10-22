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
import Marvel.Ref
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
  fromList $
    [ ("01039", SomeCardEffect rocketBootsEffect)
    , ("01068", SomeCardEffect visionEffect)
    , ("01070", SomeCardEffect leadFromTheFrontEffect)
    , ("01092", SomeCardEffect helicarrierEffect)
    , ("03003", SomeCardEffect fearlessDeterminationEffect)
    , ("03015", SomeCardEffect avengersAssembleEffect)
    , ("03024", SomeCardEffect avengersTowerEffect)
    , ("05008", SomeCardEffect nakiaBahadirEffect)
    , ("05010", SomeCardEffect embiggenEffect)
    , ("05011", SomeCardEffect shrinkEffect)
    , ("05032", SomeCardEffect moraleBoostEffect)
    ]

lookupEffect :: CardCode -> Source -> EntityMatcher -> EffectId -> Effect
lookupEffect cardCode source entityMatcher effectId =
  case lookup cardCode allEffects of
    Just (SomeCardEffect a) ->
      Effect $ buildCard a (source, entityMatcher, effectId)
    Nothing -> error $ "Invalid card code for effect " <> show cardCode
