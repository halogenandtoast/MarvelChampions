{-# OPTIONS_GHC -Wno-orphans #-}

module Marvel.Effect where

import Marvel.Prelude

import Marvel.Ally.Allies
import Marvel.Card.Builder
import Marvel.Card.Code
import Marvel.Effect.Types
import Marvel.Event.Events
import Marvel.Hero.Heroes
import Marvel.Id
import Marvel.Matchers
import Marvel.Obligation.Obligations
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
    , ("09027", SomeCardEffect physicalTollEffect)
    -- The Green Goblin

    -- The Wrecking Crew

    -- The Rise of Red Skull
    , ("04031a", SomeCardEffect spiderWomanEffect)
    , ("04039", SomeCardEffect selfPropelledGlideEffect)

    -- Ant-Man
    , ("12009", SomeCardEffect giantStrengthEffect)
    , ("12016", SomeCardEffect moxieEffect)
    , ("12024", SomeCardEffect teamBuildingExerciseEffect)
    , ("12025", SomeCardEffect careForCassieEffect)
    , ("14024", SomeCardEffect needForSpeedEffect)
    , ("14005", SomeCardEffect maximumVelocityEffect)
    , ("14022", SomeCardEffect adrenalineRushEffect)
    , ("14023", SomeCardEffect civicDutyEffect)
    , ("15003", SomeCardEffect chaosMagicEffect)
    , ("12033", SomeCardEffect assessTheSituationEffect)
    , ("17001a", SomeCardEffect starLordEffect)
    , ("17015", SomeCardEffect blazeOfGloryEffect)

    -- Wasp
    , ("13019", SomeCardEffect spiderManMilesMoralesEffect)
    , ("20013", SomeCardEffect makingAnEntranceEffect)
    , ("20027", SomeCardEffect welcomeAboardEffect)

    -- The Once and Future Kang

    ]

lookupEffect :: CardCode -> Source -> EntityMatcher -> EffectId -> Effect
lookupEffect cardCode source entityMatcher effectId =
  case lookup cardCode allEffects of
    Just (SomeCardEffect a) ->
      Effect $ buildCard a (source, entityMatcher, effectId)
    Nothing -> error $ "Invalid card code for effect " <> show cardCode
