{-# OPTIONS_GHC -Wno-orphans #-}

module Marvel.Effect where

import Marvel.FearNoEvil.BullseyeAttachments qualified as BullEquipment

import Marvel.KreeFanatic (bringTheHammerDownEffect)
import Marvel.CampaignPlayers.Helpers (campaignStickEffect, campaignBasicKeywordEffect)
import Marvel.CampaignPlayers.Helpers (campaignNextEventEffect)
import Marvel.Event.Events.SneakAttack ()
import Marvel.Event.Events.JessicaLuke (takeAStandEffect)

import Marvel.Attachment.Attachments.MojoGenres (mojoStakeEffect)
import Marvel.Event.Events.Cable ()
import Marvel.Minion.Minions.NextEvolutionMisterSinister (ne40145BoostEffect)
import Marvel.Resource.Resources.AvianAnatomy (avianAnatomyEffect)
import Marvel.MainScheme.MainSchemes.AgeOfApocalypseFourHorsemen (aoaHorsemenBlankEffect)
import Marvel.FalconWinter.Helpers (fwNextCardReductionEffect)
import Marvel.Resource.Resources.Shield (specialFundingEffect)
import Marvel.Prelude
import Marvel.Resource.Resources.Cyclops (effectiveLeadershipEffect)
import Marvel.Resource.Resources.PassionForJustice (passionForJusticeEffect)
import Marvel.Resource.Resources.MutantGenesis (aggressiveEnergyEffect)

import Marvel.Ally.Allies
import Marvel.Attachment.Attachments.SmModulars (delusionOfCollusionBoostEffect)
import Marvel.Card.Builder
import Marvel.Card.Code
import Marvel.Treachery.Treacheries.SmModulars (callingInFavorsEffect, threatOrMenaceEffect)
import Marvel.Effect.CardPlayReduction (CardPlayReduction)
import Marvel.Effect.Lasting (Lasting)
import Marvel.SideScheme.SideSchemes.KeepUpThePressure (keepUpThePressureEffect)
import Marvel.Effect.Types
import Marvel.Event.Events
import Marvel.Hero.Heroes
import Marvel.Id
import Marvel.Matchers
import Marvel.Obligation.Obligations
import Marvel.Ref
import Marvel.SideScheme.SideSchemes.SmModulars (teamworkMakesTheDreamWorkBoostEffect)
import Marvel.SideScheme.SideSchemes.GalacticArtifacts (crystalBallEffect)
import Marvel.Support.Supports
import Marvel.Upgrade.Upgrades

instance FromJSON Effect where
  parseJSON = withObject "Effect" $ \o -> do
    cCode <- o .: "effectCardCode"
    if cCode == "engine-lasting"
      then Effect <$> parseJSON @Lasting (Object o)
      else if cCode == "engine-card-play-reduction"
      then Effect <$> parseJSON @CardPlayReduction (Object o)
      else case lookup cCode allEffects of
        Nothing -> error $ "Invalid effect: " <> show cCode
        Just (SomeCardEffect (_ :: CardEffect a)) ->
          Effect <$> parseJSON @a (Object o)

allEffects :: HashMap CardCode SomeCardEffect
allEffects =
  fromList $ [(toCardCode card,SomeCardEffect card) | card <- BullEquipment.bullseyeAttackEffects] <>
    [ ("90004", SomeCardEffect bringTheHammerDownEffect)
    , ("56005", SomeCardEffect campaignBasicKeywordEffect)
    , ("58020", SomeCardEffect wmUnifiedStrikeEffect)
    , ("58033", SomeCardEffect wmDisarmingDefenseEffect)
    , ("59014", SomeCardEffect hcGoldenMaceEffect)
    , ("62015", SomeCardEffect takeAStandEffect)
    , ("60029", SomeCardEffect campaignStickEffect)
    , ("60039", SomeCardEffect campaignNextEventEffect)
    , ("59023", SomeCardEffect recruitmentDriveEffect)
    , ("50038", SomeCardEffect covertSurveillanceEffect)
    , ("50037", SomeCardEffect concentratedFireEffect)
    , ("50036", SomeCardEffect mariaHillAllyEffect)
    , ("50013", SomeCardEffect slingshotEffect)
    , ("50007", SomeCardEffect specialFundingEffect)
    , ("54002", SomeCardEffect fw54002Effect)
    , ("54011", SomeCardEffect fw54011Effect)
    , ("54018", SomeCardEffect fw54018Effect)
    , ("54019", SomeCardEffect fwNextCardReductionEffect)
    , ("46018", SomeCardEffect keepUpThePressureEffect)
    , ("46019", SomeCardEffect shadowcatIcemanEffect)
    , ("47019", SomeCardEffect jubileeCellPhoneEffect)
  , ("45085", SomeCardEffect aoaHorsemenBlankEffect)
    , ("44020", SomeCardEffect getRageYEffect)
    , ("43002", SomeCardEffect x23sClawsEffect)
    , ("43004", SomeCardEffect animalInstinctEffect)
    , ("43005", SomeCardEffect clawMasteryEffect)
    , ("43007", SomeCardEffect sisterlyBondEffect)
    , ("42008", SomeCardEffect avianAnatomyEffect)
    , ("42004", SomeCardEffect aerialAgilityEffect)
    , ("42010", SomeCardEffect technoOrganicWingsEffect)
    , ("42016", SomeCardEffect tauntEffect)
    , ("41019", SomeCardEffect directedForceEffect)
    , ("41017", SomeCardEffect floatLikeAButterflyEffect)
    , ("40145", SomeCardEffect ne40145BoostEffect)
    , ("40017", SomeCardEffect cableMissionPlanningEffect)
    , ("39048", SomeCardEffect mojoStakeEffect)
    , ("38001a", SomeCardEffect rogueHeroEffect)
    , ("38014", SomeCardEffect judokaSkillEffect)
    , ("37001a", SomeCardEffect gambitHeroEffect)
    , ("37013", SomeCardEffect operativeSkillEffect)
    , ("36019", SomeCardEffect leadershipSkillEffect)
    , ("52011", SomeCardEffect spiderClawsSilkEffect)
    , ("52012", SomeCardEffect spiderReflexesSilkEffect)
    , ("36020", SomeCardEffect toMeMyXMenEffect)
    , ("36012", SomeCardEffect flashFreezeEffect)
    , ("36005", SomeCardEffect blizzardEffect)
    , ("33018", SomeCardEffect effectiveLeadershipEffect)
    , ("33016", SomeCardEffect coordinatedAttackEffect)
    , ("33033", SomeCardEffect befuddleEffect)
    , ("34020", SomeCardEffect passionForJusticeEffect)
    , ("34034", SomeCardEffect psychicKickerEffect)
    , ("35002", SomeCardEffect wolverinesClawsEffect)
    , ("35003", SomeCardEffect jubileeEffect)
    , ("35016", SomeCardEffect warriorSkillEffect)
    , ("32183", SomeCardEffect mgGroupAssaultEffect)
    , ("32193", SomeCardEffect mgRescueOperationEffect)
    , ("32047", SomeCardEffect aggressiveEnergyEffect)
    , ("31016", SomeCardEffect repurposeEffect)
    , ("30029", SomeCardEffect warriorOfTheGreatWebEffect)
    , ("30010", SomeCardEffect hugeWoodenHammerEffect)
    , ("30011", SomeCardEffect organicWebbingEffect)
    , ("27129", SomeCardEffect callingInFavorsEffect)
    , ("27134", SomeCardEffect threatOrMenaceEffect)
    , ("27145", SomeCardEffect teamworkMakesTheDreamWorkBoostEffect)
    , ("01039", SomeCardEffect rocketBootsEffect)
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

    -- Rocket Raccoon
    , ("16030", SomeCardEffect iveGotAPlanEffect)
    , ("16032", SomeCardEffect schadenfreudeEffect)
    , ("16046", SomeCardEffect handCannonEffect)

    -- Wasp
    , ("13019", SomeCardEffect spiderManMilesMoralesEffect)
    , ("20013", SomeCardEffect makingAnEntranceEffect)
    , ("20027", SomeCardEffect welcomeAboardEffect)

    -- Groot
    , ("16024", SomeCardEffect deftFocusEffect)

    -- Spectrum
    , ("23017", SomeCardEffect sneakAttackEffect)
    , ("21009", SomeCardEffect pulsarShieldEffect)
    , ("21012", SomeCardEffect powerManVictorAlvarezEffect)

    -- Adam Warlock
    , ("21031a", SomeCardEffect adamWarlockEffect)

    -- The Galaxy's Most Wanted: The Market
    , ("16169", SomeCardEffect sureGambleEffect)
    , ("16165", SomeCardEffect reactorCoreEffect)

    -- The Once and Future Kang

    -- The Galaxy's Most Wanted: Galactic Artifacts
    , ("16130", SomeCardEffect crystalBallEffect)
    , ("27170", SomeCardEffect delusionOfCollusionBoostEffect)
    , ("28006", SomeCardEffect unleashNovaForceEffect)
    , ("28016", SomeCardEffect fluidMotionEffect)
    , ("28017", SomeCardEffect honedTechniqueEffect)
    , ("29008", SomeCardEffect sectorScanEffect)
    , ("29014", SomeCardEffect cloud9Effect)
    , ("29016", SomeCardEffect patriotRayshaunLucasEffect)
    , ("29020", SomeCardEffect rAndDFacilityEffect)
    , ("29024", SomeCardEffect vivianIronheartEffect)
    , ("29025", SomeCardEffect goForChampionsEffect)
    ]

lookupEffect :: CardCode -> Source -> EntityMatcher -> EffectId -> Effect
lookupEffect cardCode source entityMatcher effectId =
  case lookup cardCode allEffects of
    Just (SomeCardEffect a) ->
      Effect $ buildCard a (source, entityMatcher, effectId)
    Nothing -> error $ "Invalid card code for effect " <> show cardCode
