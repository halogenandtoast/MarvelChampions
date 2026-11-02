{-# OPTIONS_GHC -Wno-orphans #-}
module Marvel.Upgrade where

import Marvel.Upgrade.Upgrades.AgeOfApocalypseBasicCampaign
import Marvel.Upgrade.Upgrades.MutantGenesisCampaign
import Marvel.Prelude

import Marvel.Card
import Marvel.Id
import Marvel.Upgrade.Types
import Marvel.Upgrade.Upgrades

instance FromJSON Upgrade where
  parseJSON = withObject "Upgrade" $ \o -> do
    cardDef <- o .: "upgradeCardDef"
    withUpgradeCardCode (cdCardCode cardDef)
      $ \(_ :: UpgradeCard a) -> Upgrade <$> parseJSON @a (Object o)

withUpgradeCardCode
  :: CardCode -> (forall a . IsUpgrade a => UpgradeCard a -> r) -> r
withUpgradeCardCode cCode f = case lookup cCode allUpgrades of
  Nothing -> error "invalid upgrade"
  Just (SomeUpgradeCard a) -> f a

allUpgrades :: HashMap CardCode SomeUpgradeCard
allUpgrades = fromList $ map
  (toFst someUpgradeCardCode)
  $ concatMap (\(SomeUpgradeCard a) -> SomeUpgradeCard <$> printings a)
  [ SomeUpgradeCard fw53008, SomeUpgradeCard fw53009, SomeUpgradeCard fw53010, SomeUpgradeCard fw53011, SomeUpgradeCard fw53012, SomeUpgradeCard fw53013, SomeUpgradeCard fw53023, SomeUpgradeCard fw53024, SomeUpgradeCard fw53034, SomeUpgradeCard fw54002, SomeUpgradeCard fw54008, SomeUpgradeCard fw54009, SomeUpgradeCard fw54010, SomeUpgradeCard fw54011, SomeUpgradeCard fw54017, SomeUpgradeCard fw54018, SomeUpgradeCard fw54019, SomeUpgradeCard fw54020, SomeUpgradeCard fw54033
  , SomeUpgradeCard aosP50010, SomeUpgradeCard aosP50011, SomeUpgradeCard aosP50035a, SomeUpgradeCard aosP50035b, SomeUpgradeCard aosP50042, SomeUpgradeCard aosP50043, SomeUpgradeCard aosP50044, SomeUpgradeCard aosP50045, SomeUpgradeCard aosP50046, SomeUpgradeCard aosP50050, SomeUpgradeCard aosP50051, SomeUpgradeCard aosP50052, SomeUpgradeCard aosP50058
  , SomeUpgradeCard ajaAdannaShuri, SomeUpgradeCard kimoyoBeadsShuri, SomeUpgradeCard pantherClawsShuri, SomeUpgradeCard spiderBitesShuri, SomeUpgradeCard vibraniumSuitShuri, SomeUpgradeCard invisibilityGearShuri, SomeUpgradeCard stingOperationShuri, SomeUpgradeCard redemptionShuri, SomeUpgradeCard eideticMemorySilk, SomeUpgradeCard organicWebbingSilk, SomeUpgradeCard outwitSilk, SomeUpgradeCard spiderClawsSilk, SomeUpgradeCard spiderReflexesSilk, SomeUpgradeCard energyShieldSilk, SomeUpgradeCard readyforaFightSilk, SomeUpgradeCard stunGunSilk
  , SomeUpgradeCard nc48004, SomeUpgradeCard nc48005, SomeUpgradeCard nc48006, SomeUpgradeCard nc48014, SomeUpgradeCard nc48015
  , SomeUpgradeCard jubilee47004, SomeUpgradeCard jubilee47005, SomeUpgradeCard jubilee47013, SomeUpgradeCard jubilee47019
  , SomeUpgradeCard suppressingFire
  , SomeUpgradeCard frozenSolid
  , SomeUpgradeCard iceSlide
  , SomeUpgradeCard cryokineticPerception
  , SomeUpgradeCard powerBelt
  , SomeUpgradeCard frostbite
  , SomeUpgradeCard magnetosHelmet, SomeUpgradeCard magnetosArmor, SomeUpgradeCard magnetosCape, SomeUpgradeCard magneticBubble, SomeUpgradeCard wrappedInMetal
  , SomeUpgradeCard desperateMeasures, SomeUpgradeCard bloodgem
  , SomeUpgradeCard testTheDefense
  , SomeUpgradeCard bloodRage
  , SomeUpgradeCard mysticalArmor
  , SomeUpgradeCard soulsword
  , SomeUpgradeCard magiksCrown
  , SomeUpgradeCard bishopsRifle, SomeUpgradeCard bishopsUniform, SomeUpgradeCard superCharged, SomeUpgradeCard advancedSuit, SomeUpgradeCard sidekick
  , SomeUpgradeCard armedToTheTeeth, SomeUpgradeCard deadpoolsKatana, SomeUpgradeCard itAintOver, SomeUpgradeCard gitGud, SomeUpgradeCard healingFactor, SomeUpgradeCard stickToItiveness, SomeUpgradeCard ambush, SomeUpgradeCard bazooka, SomeUpgradeCard blackout, SomeUpgradeCard distraction, SomeUpgradeCard laserSwords, SomeUpgradeCard rockPaperScissors, SomeUpgradeCard ticTacToe, SomeUpgradeCard war
  , SomeUpgradeCard x23sClaws, SomeUpgradeCard adamantiumLacing, SomeUpgradeCard grimResolve, SomeUpgradeCard painTolerance, SomeUpgradeCard punctureWound, SomeUpgradeCard nowImMad, SomeUpgradeCard theDirectApproach, SomeUpgradeCard combatSpecialist, SomeUpgradeCard defenseSpecialist, SomeUpgradeCard frontLineSpecialist, SomeUpgradeCard surveillanceSpecialist
  , SomeUpgradeCard technoOrganicWings, SomeUpgradeCard containmentStrategy, SomeUpgradeCard soaringAcrobatics, SomeUpgradeCard eyesInTheSky, SomeUpgradeCard xForceRecruit
  , SomeUpgradeCard psiKnife, SomeUpgradeCard psiKatana, SomeUpgradeCard martialArtsTraining, SomeUpgradeCard psionicTraining, SomeUpgradeCard weaponsTraining, SomeUpgradeCard floatLikeAButterfly, SomeUpgradeCard telepathy, SomeUpgradeCard telekinesis
  , SomeUpgradeCard sharpshooter
  , SomeUpgradeCard probabilityField
  , SomeUpgradeCard luckyBreak
  , SomeUpgradeCard luckyandGood
  , SomeUpgradeCard dominosPistol
  , SomeUpgradeCard cable40009, SomeUpgradeCard cable40010, SomeUpgradeCard cable40011, SomeUpgradeCard cable40012, SomeUpgradeCard cable40013, SomeUpgradeCard cable40023, SomeUpgradeCard cable40029
  , SomeUpgradeCard touched, SomeUpgradeCard roguesJacket, SomeUpgradeCard judokaSkill, SomeUpgradeCard xGene, SomeUpgradeCard gambitsStaff, SomeUpgradeCard gambitsGuildArmor, SomeUpgradeCard operativeSkill
  , SomeUpgradeCard stormsCrown
  , SomeUpgradeCard stormsCape
  , SomeUpgradeCard leadershipSkill
  , SomeUpgradeCard rubyQuartzVisor, SomeUpgradeCard fieldCommander, SomeUpgradeCard exploitWeakness, SomeUpgradeCard practicedDefense, SomeUpgradeCard priorityTarget, SomeUpgradeCard dangerRoomTraining, SomeUpgradeCard coordinatedAttack, SomeUpgradeCard marked, SomeUpgradeCard befuddle, SomeUpgradeCard pinnedDown, SomeUpgradeCard honoraryXMen
  , SomeUpgradeCard wolverinesClaws, SomeUpgradeCard adamantiumSkeleton, SomeUpgradeCard berserkerFrenzy, SomeUpgradeCard iGotBetter, SomeUpgradeCard warriorSkill
  , SomeUpgradeCard mgCoupDeGrace, SomeUpgradeCard mgSwagger, SomeUpgradeCard mgBrazenDefense, SomeUpgradeCard mgFerociousAttack, SomeUpgradeCard mgWarCry, SomeUpgradeCard mgCommanderCoupDeGrace, SomeUpgradeCard mgCompassion, SomeUpgradeCard mgGroupAssault, SomeUpgradeCard mgShockAndAwe, SomeUpgradeCard mgImprovisation, SomeUpgradeCard mgDefenderSwagger, SomeUpgradeCard mgSurprise, SomeUpgradeCard mgHeroicIntervention, SomeUpgradeCard mgDeterminedDefense, SomeUpgradeCard mgBodyguard, SomeUpgradeCard mgPeacekeeperSurprise, SomeUpgradeCard mgPeacekeeperCompassion, SomeUpgradeCard mgRescueOperation, SomeUpgradeCard mgMentorship, SomeUpgradeCard mgFortitude
  , SomeUpgradeCard smCompactDartsA, SomeUpgradeCard smCompactDartsB, SomeUpgradeCard smImpactDampeningSuitA, SomeUpgradeCard smImpactDampeningSuitB, SomeUpgradeCard smLaserGogglesA, SomeUpgradeCard smLaserGogglesB, SomeUpgradeCard smPropulsionGauntletA, SomeUpgradeCard smPropulsionGauntletB, SomeUpgradeCard smRetinalDisplayA, SomeUpgradeCard smRetinalDisplayB, SomeUpgradeCard smShockKnucklesA, SomeUpgradeCard smShockKnucklesB, SomeUpgradeCard smWaveBracersA, SomeUpgradeCard smWaveBracersB, SomeUpgradeCard smWristNavigatorA, SomeUpgradeCard smWristNavigatorB
  , SomeUpgradeCard spiderTracer
  , SomeUpgradeCard webShooter
  , SomeUpgradeCard webbedUp
  , SomeUpgradeCard captainMarvelsHelmet
  , SomeUpgradeCard cosmicFlight
  , SomeUpgradeCard energyChannel
  , SomeUpgradeCard focusedRage
  , SomeUpgradeCard superhumanStrength
  , SomeUpgradeCard arcReactor
  , SomeUpgradeCard markVArmor
  , SomeUpgradeCard markVHelmet
  , SomeUpgradeCard poweredGauntlets
  , SomeUpgradeCard rocketBoots
  , SomeUpgradeCard energyDaggers
  , SomeUpgradeCard pantherClaws
  , SomeUpgradeCard tacticalGenius
  , SomeUpgradeCard vibraniumSuit
  , SomeUpgradeCard combatTraining
  , SomeUpgradeCard heroicIntuition
  , SomeUpgradeCard inspired
  , SomeUpgradeCard armoredVest
  , SomeUpgradeCard indomitable
  , SomeUpgradeCard tenacity
  , SomeUpgradeCard captainAmericasHelmet
  , SomeUpgradeCard captainAmericasShield
  , SomeUpgradeCard superSoldierSerum
  , SomeUpgradeCard honoraryAvenger
  , SomeUpgradeCard enraged
  , SomeUpgradeCard followed
  , SomeUpgradeCard enhancedAwareness
  , SomeUpgradeCard biokineticPolymerSuit
  , SomeUpgradeCard embiggen
  , SomeUpgradeCard shrink
  , SomeUpgradeCard energyBarrier
  , SomeUpgradeCard endurance
  , SomeUpgradeCard enhancedReflexes
  , SomeUpgradeCard downTime
  , SomeUpgradeCard attacrobatics
  , SomeUpgradeCard blackWidowsGauntlet
  , SomeUpgradeCard grapplingHook
  , SomeUpgradeCard synthSuit
  , SomeUpgradeCard widowsBite
  , SomeUpgradeCard counterintelligence
  , SomeUpgradeCard spycraft
  , SomeUpgradeCard targetAcquired
  , SomeUpgradeCard counterattack
  , SomeUpgradeCard rapidResponse
  , SomeUpgradeCard defensiveStance
  , SomeUpgradeCard espionage
  , SomeUpgradeCard godOfThunder
  , SomeUpgradeCard mjolnir
  , SomeUpgradeCard thorsHelmet
  , SomeUpgradeCard battleFury
  , SomeUpgradeCard jarnbjorn
  , SomeUpgradeCard underSurveillance
  , SomeUpgradeCard enhancedPhysique
  , SomeUpgradeCard cloakOfLevitation
  , SomeUpgradeCard magicalEnhancements
  , SomeUpgradeCard theEyeOfAgamotto
  , SomeUpgradeCard theSorcererSupreme
  , SomeUpgradeCard unflappable
  , SomeUpgradeCard boundlessRage
  , SomeUpgradeCard immovableObject
  , SomeUpgradeCard martialProwess
  , SomeUpgradeCard electrostaticArmor
  , SomeUpgradeCard resourceful
  -- The Green Goblin

  -- The Wrecking Crew

  -- The Rise of Red Skull
  , SomeUpgradeCard hawkeyesBow
  , SomeUpgradeCard hawkeyesQuiver
  , SomeUpgradeCard expertMarksman
  , SomeUpgradeCard skyCycle
  , SomeUpgradeCard finesse
  , SomeUpgradeCard skilledInvestigator

  -- The Rise of Red Skull: the Hydra Campaign set
  , SomeUpgradeCard adrenalStims
  , SomeUpgradeCard tacticalScanner
  , SomeUpgradeCard emergencyTeleporter
  , SomeUpgradeCard laserCannon
  , SomeUpgradeCard basicThwartUpgrade
  , SomeUpgradeCard improvedThwartUpgrade
  , SomeUpgradeCard basicAttackUpgrade
  , SomeUpgradeCard improvedAttackUpgrade
  , SomeUpgradeCard basicDefenseUpgrade
  , SomeUpgradeCard improvedDefenseUpgrade
  , SomeUpgradeCard basicRecoveryUpgrade
  , SomeUpgradeCard improvedRecoveryUpgrade
  -- The Once and Future Kang


  -- Ant-Man
  , SomeUpgradeCard antMansHelmet
  , SomeUpgradeCard giantStrength
  , SomeUpgradeCard wristGauntlets
  , SomeUpgradeCard powerGloves
  , SomeUpgradeCard reinforcedSuit

  -- Wasp
  , SomeUpgradeCard redRoomTraining
  , SomeUpgradeCard bioSyntheticWings
  , SomeUpgradeCard waspsHelmet
  , SomeUpgradeCard lieInWait

  -- Quicksilver
  , SomeUpgradeCard acceleratedReflex
  , SomeUpgradeCard frictionResistance
  , SomeUpgradeCard hyperPerception
  , SomeUpgradeCard reinforcedSinew
  , SomeUpgradeCard nervesOfSteel
  , SomeUpgradeCard adrenalineRush
  , SomeUpgradeCard civicDuty
  , SomeUpgradeCard bruteForce
  , SomeUpgradeCard senseOfJustice

  -- Scarlet Witch
  , SomeUpgradeCard magicShield
  , SomeUpgradeCard scarletWitchsCrest

  -- Star-Lord
  , SomeUpgradeCard elementGun
  , SomeUpgradeCard jetBoots
  , SomeUpgradeCard leaderOfTheGuardians
  , SomeUpgradeCard starLordsHelmet
  , SomeUpgradeCard laserBlaster
  , SomeUpgradeCard pulseGrenade

  -- Gamora
  , SomeUpgradeCard keenInstincts
  , SomeUpgradeCard gamorasSword
  , SomeUpgradeCard godslayer
  , SomeUpgradeCard commsImplant

  -- Groot
  , SomeUpgradeCard entanglingVines
  , SomeUpgradeCard lashingVines
  , SomeUpgradeCard vineShield
  , SomeUpgradeCard vineSpikes
  , SomeUpgradeCard dauntless
  , SomeUpgradeCard hardToIgnore
  , SomeUpgradeCard deftFocus

  -- Rocket Raccoon
  , SomeUpgradeCard batteryPack
  , SomeUpgradeCard cyberneticSkeleton
  , SomeUpgradeCard particleCannon
  , SomeUpgradeCard rocketLauncher
  , SomeUpgradeCard rocketsPistol
  , SomeUpgradeCard thrusterBoots
  , SomeUpgradeCard followThrough
  , SomeUpgradeCard handCannon
  , SomeUpgradeCard boosterBoots

  -- The Galaxy's Most Wanted: The Market
  , SomeUpgradeCard armorPlating
  , SomeUpgradeCard heavyCannon
  , SomeUpgradeCard hyperThrusters
  , SomeUpgradeCard reactorCore
  , SomeUpgradeCard cargoHold
  , SomeUpgradeCard mountedLaser
  , SomeUpgradeCard navigationColumn
  , SomeUpgradeCard targetingScreen
  -- Drax
  , SomeUpgradeCard draxsKnife
  , SomeUpgradeCard draxsOtherKnife
  , SomeUpgradeCard dwiTheetMastery
  , SomeUpgradeCard tooStubbornToDie

  -- Venom
  , SomeUpgradeCard multiGun
  , SomeUpgradeCard spiderSense
  , SomeUpgradeCard venomsPistol
  , SomeUpgradeCard sonicRifle
  , SomeUpgradeCard sideHolster
  , SomeUpgradeCard plasmaPistol

  -- Spectrum
  , SomeUpgradeCard gamma
  , SomeUpgradeCard photon
  , SomeUpgradeCard pulsar
  , SomeUpgradeCard energyDuplication
  , SomeUpgradeCard readyToRumble

  -- Adam Warlock
  , SomeUpgradeCard karmicStaff
  , SomeUpgradeCard warlocksCape
  , SomeUpgradeCard cosmicWard
  , SomeUpgradeCard mysticSenses

  -- The Mad Titan's Shadow: the campaign set
  , SomeUpgradeCard nornStoneSetup
  , SomeUpgradeCard nornStone
  , SomeUpgradeCard cutthroatAmbition
  , SomeUpgradeCard evasiveManeuvering
  , SomeUpgradeCard unyieldingPersistence
  , SomeUpgradeCard weaponsMaster
  , SomeUpgradeCard wideStance
  , SomeUpgradeCard justiceServed
  , SomeUpgradeCard energySpear
  , SomeUpgradeCard honoraryGuardian
  , SomeUpgradeCard upgradedChassis
  , SomeUpgradeCard gauntletGun
  , SomeUpgradeCard missileLauncher
  , SomeUpgradeCard shoulderCannon
  , SomeUpgradeCard sidearm
  , SomeUpgradeCard deathGlow
  , SomeUpgradeCard valkyriesSpear
  , SomeUpgradeCard dragonfang
  , SomeUpgradeCard aragorn
  , SomeUpgradeCard flightOfTheValkyrior
  , SomeUpgradeCard anticipation
  , SomeUpgradeCard intangible
  , SomeUpgradeCard solarGem
  , SomeUpgradeCard visionsCape
  , SomeUpgradeCard densityControl
  , SomeUpgradeCard flowLikeWater
  , SomeUpgradeCard chanceEncounter
  , SomeUpgradeCard dense
  , SomeUpgradeCard ticketToTheMultiverse
  , SomeUpgradeCard webBracelet
  , SomeUpgradeCard planB
  , SomeUpgradeCard powerWithin
  , SomeUpgradeCard defenseMechanism
  , SomeUpgradeCard webShooterMilesMorales
  , SomeUpgradeCard symbioteSuit
  , SomeUpgradeCard supernovaHelmet
  , SomeUpgradeCard fluidMotion
  , SomeUpgradeCard honedTechnique
  , SomeUpgradeCard heightAdvantage
  , SomeUpgradeCard photonBlasters
  , SomeUpgradeCard propulsionJets
  , SomeUpgradeCard ingenuity
  , SomeUpgradeCard cartoonPhysics
  , SomeUpgradeCard hugeWoodenHammer
  , SomeUpgradeCard organicWebbing
  , SomeUpgradeCard overwatch
  , SomeUpgradeCard warriorOfTheGreatWeb
  , SomeUpgradeCard spdrPilot
  , SomeUpgradeCard hostSpider
  , SomeUpgradeCard psychicLink
  , SomeUpgradeCard speedMetalAlloy
  , SomeUpgradeCard webFluidCompressor
  , SomeUpgradeCard forcefieldGenerator
  , SomeUpgradeCard spiderTingle
  , SomeUpgradeCard unshakable
  , SomeUpgradeCard clarityOfPurpose
  , SomeUpgradeCard ironWill
  , SomeUpgradeCard titaniumMuscles
  , SomeUpgradeCard organicSteel
  , SomeUpgradeCard protectiveTraining
  , SomeUpgradeCard solid
  , SomeUpgradeCard phased
  , SomeUpgradeCard acuteControl
  , SomeUpgradeCard intangibleInterference
  , SomeUpgradeCard phasedAndConfused
  , SomeUpgradeCard attackTraining
  , SomeUpgradeCard gatekeeper
  , SomeUpgradeCard phoenixSuit
  , SomeUpgradeCard riseFromTheAshes
  , SomeUpgradeCard telekineticShield
  , SomeUpgradeCard mentalParalysis
  , SomeUpgradeCard mindControl
  , SomeUpgradeCard missionTraining
  , SomeUpgradeCard phoenixForceRestrained
  , SomeUpgradeCard phoenixForceUnleashed
  ]

lookupUpgrade :: CardCode -> IdentityId -> UpgradeId -> Upgrade
lookupUpgrade cardCode identityId upgradeId =
  case lookup cardCode allUpgrades of
    Just (SomeUpgradeCard a) ->
      Upgrade $ buildCard a (identityId, upgradeId)
    Nothing -> error $ "Invalid card code for upgrade " <> show cardCode
