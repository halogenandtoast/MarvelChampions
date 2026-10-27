{-# OPTIONS_GHC -Wno-orphans #-}
module Marvel.Upgrade where

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
  [ SomeUpgradeCard spiderTracer
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

  -- Drax
  , SomeUpgradeCard draxsKnife
  , SomeUpgradeCard draxsOtherKnife
  , SomeUpgradeCard dwiTheetMastery
  , SomeUpgradeCard tooStubbornToDie
  ]

lookupUpgrade :: CardCode -> IdentityId -> UpgradeId -> Upgrade
lookupUpgrade cardCode identityId upgradeId =
  case lookup cardCode allUpgrades of
    Just (SomeUpgradeCard a) ->
      Upgrade $ buildCard a (identityId, upgradeId)
    Nothing -> error $ "Invalid card code for upgrade " <> show cardCode
