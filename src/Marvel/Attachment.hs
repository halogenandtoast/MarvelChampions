{-# OPTIONS_GHC -Wno-orphans #-}

module Marvel.Attachment where

import Marvel.FearNoEvil.OwlTombstone qualified as Owl

import Marvel.Shadowland.Whisper qualified as Whisper

import Marvel.Shadowland.Spells qualified as Spells

import Marvel.Shadowland.DarkProtege qualified as Dark

import Marvel.Shadowland.Traps qualified as StreetTraps

import Marvel.CivilWar.MoonKnight qualified as SynthMoon
import Marvel.CivilWar.DeadlyDuo qualified as SynthDuo
import Marvel.CivilWar.Taskmaster qualified as SynthTask
import Marvel.CivilWar.SpiderMan qualified as CwSM

import Marvel.CivilWar.Namor qualified as CwN

import Marvel.CivilWar.CloakAndDagger qualified as CwCD

import Marvel.CivilWar.HellsKitchen qualified as CwHK

import Marvel.KreeFanatic qualified as KF
import Marvel.CampaignPlayers.NemesisAttachments qualified as CN

import Marvel.Attachment.Attachments.JessicaLuke qualified as JL
import Marvel.Attachment.Attachments.AgentsOfShield qualified as AosEncounter

import Marvel.Attachment.Attachments.TricksterTakeover qualified as TT

import Marvel.Attachment.Attachments.AgeOfApocalypseUnus

import Marvel.Attachment.Attachments.AgeOfApocalypseHounds

import Marvel.Attachment.Attachments.AgeOfApocalypseGenosha

import Marvel.Attachment.Attachments.AgeOfApocalypseFourHorsemen

import Marvel.Attachment.Attachments.AgeOfApocalypseEnSabahNur

import Marvel.Attachment.Attachments.AgeOfApocalypseDarkBeast

import Marvel.Attachment.Attachments.AgeOfApocalypseCelestialTech

import Marvel.Attachment.Attachments.AgeOfApocalypseBlueMoon

import Marvel.Attachment.Attachments.AgeOfApocalypseApocalypse

import Marvel.Attachment.Attachments.HoodPack qualified as HoodPack

import Marvel.Attachment.Attachments.NextEvolutionTelepathySet

import Marvel.Attachment.Attachments.NextEvolutionSuperStrengthSet

import Marvel.Attachment.Attachments.NextEvolutionNeStryfe

import Marvel.Attachment.Attachments.NextEvolutionOnTheRun

import Marvel.Attachment.Attachments.NextEvolutionMorlockSiege

import Marvel.Attachment.Attachments.NextEvolutionMisterSinister

import Marvel.Attachment.Attachments.NextEvolutionMilitaryGrade

import Marvel.Attachment.Attachments.NextEvolutionNeJuggernaut

import Marvel.Attachment.Attachments.NextEvolutionFlightSet

import Marvel.Attachment.Attachments.Mojo
import Marvel.Attachment.Attachments.MojoGenres
import Marvel.Attachment.Attachments.MojoOpening
import Marvel.Attachment.Attachments.MutantGenesisCampaign
import Marvel.Attachment.Attachments.TheHood qualified as Hood
import Marvel.Prelude

import Marvel.Attachment.Attachments
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Card.Builder ()
import Marvel.Attachment.Types
import Marvel.CivilWar.SignatureAttachments qualified as CW
import Marvel.CivilWar.Initiative qualified as CwI
import Marvel.CivilWar.Paladin qualified as CwP
import Marvel.CivilWar.MariaHill qualified as CwMH
import Marvel.CivilWar.IronManAttachments qualified as CWTech
import Marvel.CivilWar.LeaderAttachments qualified as CWGear
import Marvel.Card
import Marvel.Id

instance FromJSON Attachment where
  parseJSON = withObject "Attachment" $ \o -> do
    cardDef <- o .: "attachmentCardDef"
    withAttachmentCardCode (cdCardCode cardDef) $
      \(_ :: AttachmentCard a) -> Attachment <$> parseJSON @a (Object o)

withAttachmentCardCode ::
  CardCode -> (forall a. (IsAttachment a) => AttachmentCard a -> r) -> r
withAttachmentCardCode cCode f = case lookup cCode allAttachments of
  Nothing -> error "invalid attachment"
  Just (SomeAttachmentCard a) -> f a

allAttachments :: HashMap CardCode SomeAttachmentCard
allAttachments =
  fromList $ [(toCardCode card,SomeAttachmentCard card) | card <- Owl.streetAttachments] <>  [(toCardCode Whisper.whisper,SomeAttachmentCard Whisper.whisper)] <>  [(toCardCode card,SomeAttachmentCard card) | card <- Spells.shadowSpells] <>  [(toCardCode card,SomeAttachmentCard card) | card <- Dark.darkAttachments] <>  [(toCardCode StreetTraps.nervousJitters,SomeAttachmentCard StreetTraps.nervousJitters)] <>  [("57066",SomeAttachmentCard SynthMoon.crescentDart)] <> [("57029",SomeAttachmentCard SynthDuo.madJacksPlatform)] <> [(toCardCode card,SomeAttachmentCard card) | card <- SynthTask.taskmasterEquipment] <> [(toCardCode CwSM.tangledUp,SomeAttachmentCard CwSM.tangledUp)] <> [(toCardCode card,SomeAttachmentCard card) | card <- CwN.namorEquipment] <> [(toCardCode CwCD.darkforce,SomeAttachmentCard CwCD.darkforce)] <> [(toCardCode CwHK.resistanceFighter,SomeAttachmentCard CwHK.resistanceFighter)] <> [(toCardCode CwP.paladinsPistol,SomeAttachmentCard CwP.paladinsPistol)] <> [(toCardCode CwMH.lifeModelDecoy,SomeAttachmentCard CwMH.lifeModelDecoy)] <> [(toCardCode CwI.drafted,SomeAttachmentCard CwI.drafted)] <> [(toCardCode card,SomeAttachmentCard card) | card <- CW.signatureAttachments] <> [(toCardCode card,SomeAttachmentCard card) | card <- CWTech.ironManAttachments] <> [(toCardCode card,SomeAttachmentCard card) | card <- CWGear.leaderAttachments] <>
    map
      (toFst someAttachmentCardCode)
      [ SomeAttachmentCard CN.cn56026, SomeAttachmentCard CN.cn56057, SomeAttachmentCard CN.cn60035
  , SomeAttachmentCard KF.theAccused
  , SomeAttachmentCard hc59002, SomeAttachmentCard hc59003, SomeAttachmentCard hc59039
  , SomeAttachmentCard JL.jl62031
  , SomeAttachmentCard TT.tt55007a, SomeAttachmentCard TT.tt55007b, SomeAttachmentCard TT.tt55008a, SomeAttachmentCard TT.tt55008b, SomeAttachmentCard TT.tt55009a, SomeAttachmentCard TT.tt55009b, SomeAttachmentCard TT.tt55010a, SomeAttachmentCard TT.tt55010b, SomeAttachmentCard TT.tt55011a, SomeAttachmentCard TT.tt55011b, SomeAttachmentCard TT.tt55012, SomeAttachmentCard TT.tt55013, SomeAttachmentCard TT.tt55014, SomeAttachmentCard TT.tt55015, SomeAttachmentCard TT.tt55016, SomeAttachmentCard TT.tt55034a, SomeAttachmentCard TT.tt55034b, SomeAttachmentCard TT.tt55035, SomeAttachmentCard TT.tt55036, SomeAttachmentCard TT.tt55062
  , SomeAttachmentCard AosEncounter.aos50068, SomeAttachmentCard AosEncounter.aos50069, SomeAttachmentCard AosEncounter.aos50070, SomeAttachmentCard AosEncounter.aos50071, SomeAttachmentCard AosEncounter.aos50092, SomeAttachmentCard AosEncounter.aos50114, SomeAttachmentCard AosEncounter.aos50115, SomeAttachmentCard AosEncounter.aos50116, SomeAttachmentCard AosEncounter.aos50117, SomeAttachmentCard AosEncounter.aos50118, SomeAttachmentCard AosEncounter.aos50119, SomeAttachmentCard AosEncounter.aos50132, SomeAttachmentCard AosEncounter.aos50144, SomeAttachmentCard AosEncounter.aos50145, SomeAttachmentCard AosEncounter.aos50149, SomeAttachmentCard AosEncounter.aos50153, SomeAttachmentCard AosEncounter.aos50157, SomeAttachmentCard AosEncounter.aos50158, SomeAttachmentCard AosEncounter.aos50170, SomeAttachmentCard AosEncounter.aos50171, SomeAttachmentCard AosEncounter.aos50181b, SomeAttachmentCard AosEncounter.aos50182b, SomeAttachmentCard AosEncounter.aos50183b
  , SomeAttachmentCard fw53039, SomeAttachmentCard fw53040, SomeAttachmentCard fw54030, SomeAttachmentCard fw54035
  , SomeAttachmentCard energyTruncheonShuri
  , SomeAttachmentCard nc48029
  , SomeAttachmentCard lifeDrain
  , SomeAttachmentCard pyrosFlamethrower
  , SomeAttachmentCard jubilee47026, SomeAttachmentCard jubilee47027
  , SomeAttachmentCard prelateSidearm, SomeAttachmentCard prelateArmor, SomeAttachmentCard geneticExperiments, SomeAttachmentCard goldenHorse, SomeAttachmentCard metalWings, SomeAttachmentCard ahabsEnergySpear, SomeAttachmentCard noLongerWorthy, SomeAttachmentCard cyberpathy, SomeAttachmentCard biomorphing, SomeAttachmentCard molecularControl, SomeAttachmentCard theFittest, SomeAttachmentCard highTechGoggles, SomeAttachmentCard geneticEnhancement, SomeAttachmentCard cruelExperiment, SomeAttachmentCard escapedMutant, SomeAttachmentCard imperialGuardsman, SomeAttachmentCard staggeringStrength, SomeAttachmentCard celestialArmor, SomeAttachmentCard celestialWeapon
  , SomeAttachmentCard tabulaRasa16, SomeAttachmentCard antiRegenerationRay, SomeAttachmentCard poolIzed
  , SomeAttachmentCard cybermods, SomeAttachmentCard criticalWound
  , SomeAttachmentCard harpoonsHarpoon
  , SomeAttachmentCard psionicIllusion
  , SomeAttachmentCard HoodPack.hood24019, SomeAttachmentCard HoodPack.hood24020, SomeAttachmentCard HoodPack.hood24021, SomeAttachmentCard HoodPack.hood24022, SomeAttachmentCard HoodPack.hood24037, SomeAttachmentCard HoodPack.hood24038, SomeAttachmentCard HoodPack.hood24039, SomeAttachmentCard HoodPack.hood24040
  , SomeAttachmentCard ne40082, SomeAttachmentCard ne40083, SomeAttachmentCard ne40090, SomeAttachmentCard ne40091, SomeAttachmentCard ne40092, SomeAttachmentCard ne40105a, SomeAttachmentCard ne40105b, SomeAttachmentCard ne40106, SomeAttachmentCard ne40107, SomeAttachmentCard ne40122a, SomeAttachmentCard ne40122b, SomeAttachmentCard ne40123, SomeAttachmentCard ne40144, SomeAttachmentCard ne40151, SomeAttachmentCard ne40152, SomeAttachmentCard ne40153, SomeAttachmentCard ne40155, SomeAttachmentCard ne40156, SomeAttachmentCard ne40157, SomeAttachmentCard ne40159, SomeAttachmentCard ne40169, SomeAttachmentCard ne40170, SomeAttachmentCard ne40171, SomeAttachmentCard ne40172, SomeAttachmentCard ne40173
  , SomeAttachmentCard superpowerFeedback
  , SomeAttachmentCard cable40034
  , SomeAttachmentCard mojo39005, SomeAttachmentCard mojo39006, SomeAttachmentCard mojo39018, SomeAttachmentCard mojo39027, SomeAttachmentCard mojo39028, SomeAttachmentCard mojo39048, SomeAttachmentCard mojo39067
  , SomeAttachmentCard cyberneticEnhancements, SomeAttachmentCard psionicShield
  , SomeAttachmentCard switchblade
  , SomeAttachmentCard possessed
  , SomeAttachmentCard geneTherapy
  , SomeAttachmentCard deathFactor, SomeAttachmentCard adamantiumUpgrades
  , SomeAttachmentCard mgMagnetosPower
  , SomeAttachmentCard mgAdamantiumClaws, SomeAttachmentCard mgAnimalFerocity, SomeAttachmentCard mgHomoSuperior, SomeAttachmentCard mgGauntletBeam, SomeAttachmentCard mgLearningAI, SomeAttachmentCard mgAdaptiveArmor, SomeAttachmentCard mgEnergyBarrier, SomeAttachmentCard mgTargetedForElimination, SomeAttachmentCard mgUnitUpgrade, SomeAttachmentCard mgStunBeam, SomeAttachmentCard mgPhysicalStrain, SomeAttachmentCard mgMagnetosHelmet, SomeAttachmentCard mgMagnetosArmor, SomeAttachmentCard mgMagneticBubble, SomeAttachmentCard mgWrappedInMetal, SomeAttachmentCard mgNanoSentinelTech, SomeAttachmentCard smSandForm, SomeAttachmentCard smNowWeReAngry, SomeAttachmentCard smHumongousHallucination, SomeAttachmentCard smMasterfulMirage, SomeAttachmentCard smHeightenedMorale, SomeAttachmentCard smTauntingPresence, SomeAttachmentCard smTeamLeader, SomeAttachmentCard smTakeOneForTheTeam, SomeAttachmentCard smWeAreOne, SomeAttachmentCard smAdvancedGlider, SomeAttachmentCard smConcussiveBombs, SomeAttachmentCard smIncendiaryBombs, SomeAttachmentCard smSmokeBombs, SomeAttachmentCard smArmCannon, SomeAttachmentCard smIonicBoots, SomeAttachmentCard smKineticArmor, SomeAttachmentCard smNeocarbonScales, SomeAttachmentCard smSpikedGauntlet, SomeAttachmentCard smTrackingDisplay, SomeAttachmentCard smInducedPanic, SomeAttachmentCard smImprovisedWeapons, SomeAttachmentCard smViolentTendencies, SomeAttachmentCard smWebbedUp, SomeAttachmentCard smDelusionOfCollusion, SomeAttachmentCard smManipulatedMind, SomeAttachmentCard smOldGrudge, SomeAttachmentCard smSnitchesGetStitches
  , SomeAttachmentCard Hood.establishedDominance
      , SomeAttachmentCard Hood.theHoodsMantle
      , SomeAttachmentCard Hood.theHoodsPistol
      , SomeAttachmentCard armoredRhinoSuit
      , SomeAttachmentCard charge
      , SomeAttachmentCard enhancedIvoryHorn
      , SomeAttachmentCard sonicConverter
      , SomeAttachmentCard solidSoundBody
      , SomeAttachmentCard programTransmitter
      , SomeAttachmentCard upgradedDrones
      , SomeAttachmentCard vibraniumArmor
      , SomeAttachmentCard concussionBlasters
      , SomeAttachmentCard geneticallyEnhanced
      , SomeAttachmentCard biomechanicalUpgrades
      , SomeAttachmentCard counterspell
      -- The Green Goblin
      , SomeAttachmentCard goblinGlider
      , SomeAttachmentCard pumpkinBombs
      , SomeAttachmentCard allTiedUp
      , SomeAttachmentCard mediaCoverage
      , SomeAttachmentCard (reprintAs Cards.goblinGliderMutagenFormula goblinGlider)
      , SomeAttachmentCard hysteria
      , SomeAttachmentCard (reprintAs Cards.pumpkinBombsMutagenFormula pumpkinBombs)

      -- The Wrecking Crew
      , SomeAttachmentCard heldHostage
      , SomeAttachmentCard (reprintAs Cards.heldHostageThunderball heldHostage)
      , SomeAttachmentCard (reprintAs Cards.heldHostagePiledriver heldHostage)
      , SomeAttachmentCard (reprintAs Cards.heldHostageBulldozer heldHostage)
      , SomeAttachmentCard magicCrowbar
      , SomeAttachmentCard wreckersCommand
      , SomeAttachmentCard ballAndChain
      , SomeAttachmentCard radioactiveBuildup
      , SomeAttachmentCard distractingTaunts
      , SomeAttachmentCard bulldozersHelmet
      , SomeAttachmentCard rammingSpeed

      -- The Rise of Red Skull
      , SomeAttachmentCard crossfiresRifle
      , SomeAttachmentCard crossbonesMachineGun
      , SomeAttachmentCard crossbonesArmor
      , SomeAttachmentCard laserRifle
      , SomeAttachmentCard energyShield
      , SomeAttachmentCard powerGauntlets
      , SomeAttachmentCard exoSuit
      , SomeAttachmentCard absorbingMansBallAndChain
      , SomeAttachmentCard combatKnife
      , SomeAttachmentCard hydraSidearm

      -- Zola
      , SomeAttachmentCard defensiveProgramming
      , SomeAttachmentCard painInhibitors
      , SomeAttachmentCard neurologicalImplants

      -- Red Skull
      , SomeAttachmentCard redSkullsLuger
      , SomeAttachmentCard redSkullsRightHook
      , SomeAttachmentCard masterStrategist
      , SomeAttachmentCard twistedReality

      -- Taskmaster
      , SomeAttachmentCard taskmastersSword
      , SomeAttachmentCard taskmastersShield
      , SomeAttachmentCard photographicReflexes

      -- The Once and Future Kang
      , SomeAttachmentCard temporalShield
      , SomeAttachmentCard futureWeapon
      , SomeAttachmentCard frozenInTime


      -- Ant-Man
      , SomeAttachmentCard sizeIncrease

      -- Wasp
      , SomeAttachmentCard beetleArmorMkIV

      -- Quicksilver
      , SomeAttachmentCard vibrationResistance

      -- Scarlet Witch
      , SomeAttachmentCard magicalSuspension

      -- Gamora
      , SomeAttachmentCard inABind

      -- Rocket Raccoon
      , SomeAttachmentCard blackjacksBazooka

      -- The Galaxy's Most Wanted
      , SomeAttachmentCard powerStone
      -- Brotherhood of Badoon
      , SomeAttachmentCard drangsSpear
      -- Ronan the Accuser
      , SomeAttachmentCard universalWeapon
      , SomeAttachmentCard fanaticism
      , SomeAttachmentCard kreeCombatArmor
      -- The Galaxy's Most Wanted: Nebula
      , SomeAttachmentCard cutthroatAmbition
      , SomeAttachmentCard evasiveManeuvering
      , SomeAttachmentCard unyieldingPersistence
      , SomeAttachmentCard weaponMastery
      , SomeAttachmentCard wideStance

      -- The Galaxy's Most Wanted: Infiltrate the Museum, Galactic Artifacts
      , SomeAttachmentCard biogramImage
      , SomeAttachmentCard cloakOfHercules
      , SomeAttachmentCard obediencePotion
      , SomeAttachmentCard theBeyondersBlazer
      , SomeAttachmentCard thePoison
      , SomeAttachmentCard vandarianPowerWand
      -- Drax
      , SomeAttachmentCard challengeAccepted
      -- Spectrum
      , SomeAttachmentCard sapPower
      -- The Mad Titan's Shadow: Tower Defense
      , SomeAttachmentCard focusedDefense
      , SomeAttachmentCard proximasSpear
      , SomeAttachmentCard corvussGlaive
      , SomeAttachmentCard directAssault
      -- The Mad Titan's Shadow: Ebony Maw
      , SomeAttachmentCard abjuration
      , SomeAttachmentCard restrained
      -- The Mad Titan's Shadow: Thanos, Infinity Gauntlet
      , SomeAttachmentCard thanossArmor
      , SomeAttachmentCard thanossHelmet
      , SomeAttachmentCard masterOfTheStones
      , SomeAttachmentCard infinityGauntlet
      -- The Mad Titan's Shadow: Loki
      , SomeAttachmentCard lokisStaff
      , SomeAttachmentCard lokisCrown
      , SomeAttachmentCard lokisCape
      , SomeAttachmentCard masterOfIllusions
      -- The Mad Titan's Shadow: Hela, Legions of Hel, Frost Giants, Enchantress
      , SomeAttachmentCard nightsword
      , SomeAttachmentCard helasCrown
      , SomeAttachmentCard helasCloak
      , SomeAttachmentCard fallenWarrior
      , SomeAttachmentCard frozen
      , SomeAttachmentCard beguiled
      , SomeAttachmentCard seduced
      -- The Mad Titan's Shadow: the campaign set
      , SomeAttachmentCard jormungand
      , SomeAttachmentCard lethalWeapon
      , SomeAttachmentCard (reprintAs Cards.beguiledValkyrie beguiled)
      , SomeAttachmentCard (reprintAs Cards.seducedValkyrie seduced)
  , SomeAttachmentCard experimentalInjection
  , SomeAttachmentCard razorClaws
  , SomeAttachmentCard rollinRollin
  , SomeAttachmentCard cyborgTech
  , SomeAttachmentCard haywire
      , SomeAttachmentCard gobblerGlider
  , SomeAttachmentCard unstoppable
  , SomeAttachmentCard telepathicRestraint
  ]

lookupAttachment :: CardCode -> AttachmentId -> Attachment
lookupAttachment cardCode = case lookup cardCode allAttachments of
  Just (SomeAttachmentCard a) -> Attachment <$> buildCard a
  Nothing -> error $ "Invalid card code for attachment " <> show cardCode
