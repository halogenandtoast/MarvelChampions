{-# OPTIONS_GHC -Wno-orphans #-}
module Marvel.Treachery where

import Marvel.Treachery.Treacheries.AgentsOfShield qualified as AosEncounter

import Marvel.Treachery.Treacheries.TricksterTakeover qualified as TT

import Marvel.Treachery.Treacheries.AgeOfApocalypseUnus

import Marvel.Treachery.Treacheries.AgeOfApocalypseStandardIII

import Marvel.Treachery.Treacheries.AgeOfApocalypseSavageLand

import Marvel.Treachery.Treacheries.AgeOfApocalypseInfinites

import Marvel.Treachery.Treacheries.AgeOfApocalypseFourHorsemen

import Marvel.Treachery.Treacheries.AgeOfApocalypseEnSabahNur

import Marvel.Treachery.Treacheries.AgeOfApocalypseDystopianNightmare

import Marvel.Treachery.Treacheries.AgeOfApocalypseDarkBeast

import Marvel.Treachery.Treacheries.AgeOfApocalypseClanAkkaba

import Marvel.Treachery.Treacheries.AgeOfApocalypseCelestialTech

import Marvel.Treachery.Treacheries.AgeOfApocalypseApocalypse

import Marvel.Treachery.Treacheries.AgeOfApocalypseAgeOfApocalypse

import Marvel.Treachery.Treacheries.HoodPack qualified as HoodPack

import Marvel.Treachery.Treacheries.NextEvolutionTelepathySet

import Marvel.Treachery.Treacheries.NextEvolutionSuperStrengthSet

import Marvel.Treachery.Treacheries.NextEvolutionNeStryfe

import Marvel.Treachery.Treacheries.NextEvolutionOnTheRun

import Marvel.Treachery.Treacheries.NextEvolutionNextEvolutionCampaign

import Marvel.Treachery.Treacheries.NextEvolutionMutantSlayers

import Marvel.Treachery.Treacheries.NextEvolutionMorlockSiege

import Marvel.Treachery.Treacheries.NextEvolutionMisterSinister

import Marvel.Treachery.Treacheries.NextEvolutionNeJuggernaut

import Marvel.Treachery.Treacheries.NextEvolutionBlackTomCassidy

import Marvel.Treachery.Treacheries.Mojo
import Marvel.Treachery.Treacheries.MojoGenres
import Marvel.Treachery.Treacheries.MojoOpening
import Marvel.Treachery.Treacheries.TheHood qualified as Hood
import Marvel.Prelude

import Marvel.Card
import Marvel.Id
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types
import Marvel.Treachery.Treacheries

instance FromJSON Treachery where
  parseJSON = withObject "Treachery" $ \o -> do
    cardDef <- o .: "treacheryCardDef"
    withTreacheryCardCode (cdCardCode cardDef)
      $ \(_ :: TreacheryCard a) -> Treachery <$> parseJSON @a (Object o)

withTreacheryCardCode
  :: CardCode -> (forall a . IsTreachery a => TreacheryCard a -> r) -> r
withTreacheryCardCode cCode f = case lookup cCode allTreacheries of
  Nothing -> error "invalid treachery"
  Just (SomeTreacheryCard a) -> f a

allTreacheries :: HashMap CardCode SomeTreacheryCard
allTreacheries = fromList $ map
  (toFst someTreacheryCardCode)
  [ SomeTreacheryCard wm58028, SomeTreacheryCard wm58029, SomeTreacheryCard hc59040
  , SomeTreacheryCard TT.tt55023, SomeTreacheryCard TT.tt55024, SomeTreacheryCard TT.tt55025, SomeTreacheryCard TT.tt55026, SomeTreacheryCard TT.tt55049, SomeTreacheryCard TT.tt55050, SomeTreacheryCard TT.tt55051
  , SomeTreacheryCard AosEncounter.aos50076, SomeTreacheryCard AosEncounter.aos50077, SomeTreacheryCard AosEncounter.aos50078, SomeTreacheryCard AosEncounter.aos50079, SomeTreacheryCard AosEncounter.aos50082, SomeTreacheryCard AosEncounter.aos50096, SomeTreacheryCard AosEncounter.aos50097, SomeTreacheryCard AosEncounter.aos50102, SomeTreacheryCard AosEncounter.aos50123, SomeTreacheryCard AosEncounter.aos50124, SomeTreacheryCard AosEncounter.aos50128, SomeTreacheryCard AosEncounter.aos50137, SomeTreacheryCard AosEncounter.aos50138, SomeTreacheryCard AosEncounter.aos50141, SomeTreacheryCard AosEncounter.aos50142, SomeTreacheryCard AosEncounter.aos50147, SomeTreacheryCard AosEncounter.aos50151, SomeTreacheryCard AosEncounter.aos50155, SomeTreacheryCard AosEncounter.aos50160, SomeTreacheryCard AosEncounter.aos50163, SomeTreacheryCard AosEncounter.aos50164, SomeTreacheryCard AosEncounter.aos50175, SomeTreacheryCard AosEncounter.aos50176, SomeTreacheryCard AosEncounter.aos50177, SomeTreacheryCard AosEncounter.aos50184a, SomeTreacheryCard AosEncounter.aos50184b, SomeTreacheryCard AosEncounter.aos50184c
  , SomeTreacheryCard fw53033, SomeTreacheryCard fw53042, SomeTreacheryCard fw54037
  , SomeTreacheryCard aosP50033, SomeTreacheryCard aosP50063
  , SomeTreacheryCard theScreamShuri, SomeTreacheryCard extremeRiskShuri, SomeTreacheryCard huntingtheSpiderBrideSilk, SomeTreacheryCard growingStrongSilk, SomeTreacheryCard titanicProportionsSilk
  , SomeTreacheryCard nc48030, SomeTreacheryCard nc48038
  , SomeTreacheryCard theEyeOfSauron
  , SomeTreacheryCard burnIceman
  , SomeTreacheryCard elaborateTrap
  , SomeTreacheryCard angryAcolyte, SomeTreacheryCard powerAndDecadence
  , SomeTreacheryCard infinitePrelate, SomeTreacheryCard cullingTheWeak, SomeTreacheryCard warWearyDystopianNightmare, SomeTreacheryCard darkDesigns, SomeTreacheryCard sinisterStrike, SomeTreacheryCard evilAlliance, SomeTreacheryCard nowhereIsSafe, SomeTreacheryCard horsemanOfWar, SomeTreacheryCard horsemanOfFamine, SomeTreacheryCard horsemanOfPestilence, SomeTreacheryCard horsemanOfDeath, SomeTreacheryCard roughRiders, SomeTreacheryCard wolfAmongSheep, SomeTreacheryCard evilGenius, SomeTreacheryCard landOutOfTime, SomeTreacheryCard biomorphicBlast, SomeTreacheryCard technologicalInterface, SomeTreacheryCard giantSizedDespot, SomeTreacheryCard celestialTech, SomeTreacheryCard tyrantWorship, SomeTreacheryCard worldwideCrisis
  , SomeTreacheryCard battleForLimbo
  , SomeTreacheryCard temporalTrickery
  , SomeTreacheryCard crisisOfInfiniteDeadpools, SomeTreacheryCard metacidalTendencies
  , SomeTreacheryCard hackNSlashX23
  , SomeTreacheryCard spearShot
  , SomeTreacheryCard telekineticDragon
  , SomeTreacheryCard HoodPack.hood24017, SomeTreacheryCard HoodPack.hood24028, SomeTreacheryCard HoodPack.hood24029, SomeTreacheryCard HoodPack.hood24030, SomeTreacheryCard HoodPack.hood24031, SomeTreacheryCard HoodPack.hood24032, SomeTreacheryCard HoodPack.hood24036, SomeTreacheryCard HoodPack.hood24048, SomeTreacheryCard HoodPack.hood24050, SomeTreacheryCard HoodPack.hood24051, SomeTreacheryCard HoodPack.hood24052, SomeTreacheryCard HoodPack.hood24053, SomeTreacheryCard HoodPack.hood24054, SomeTreacheryCard HoodPack.hood24059, SomeTreacheryCard HoodPack.hood24069, SomeTreacheryCard HoodPack.hood24070
  , SomeTreacheryCard ne40154
  , SomeTreacheryCard ne40080, SomeTreacheryCard ne40088, SomeTreacheryCard ne40089, SomeTreacheryCard ne40102, SomeTreacheryCard ne40110, SomeTreacheryCard ne40111, SomeTreacheryCard ne40125, SomeTreacheryCard ne40126, SomeTreacheryCard ne40127, SomeTreacheryCard ne40128, SomeTreacheryCard ne40129, SomeTreacheryCard ne40135, SomeTreacheryCard ne40147, SomeTreacheryCard ne40148, SomeTreacheryCard ne40149, SomeTreacheryCard ne40150, SomeTreacheryCard ne40158, SomeTreacheryCard ne40162, SomeTreacheryCard ne40177, SomeTreacheryCard ne40178, SomeTreacheryCard ne40179, SomeTreacheryCard ne40203
  , SomeTreacheryCard cable40035, SomeTreacheryCard cable40036
  , SomeTreacheryCard mojo39009, SomeTreacheryCard mojo39010, SomeTreacheryCard mojo39011, SomeTreacheryCard mojo39017, SomeTreacheryCard mojo39019, SomeTreacheryCard mojo39020, SomeTreacheryCard mojo39021, SomeTreacheryCard mojo39032, SomeTreacheryCard mojo39033, SomeTreacheryCard mojo39034, SomeTreacheryCard mojo39040, SomeTreacheryCard mojo39046, SomeTreacheryCard mojo39070
  , SomeTreacheryCard misled, SomeTreacheryCard assassinationAttempt, SomeTreacheryCard acolyteFrenzy
  , SomeTreacheryCard knifeFight
  , SomeTreacheryCard astralAttack
  , SomeTreacheryCard concussiveForce
  , SomeTreacheryCard tentacleStrike, SomeTreacheryCard hackNSlash
  , SomeTreacheryCard mgSabretoothStrikes, SomeTreacheryCard mgUnrelentingSavage, SomeTreacheryCard mgMutantTerrorists, SomeTreacheryCard mgInfiltration, SomeTreacheryCard mgShapeshifterSurprise, SomeTreacheryCard mgSelfRepair, SomeTreacheryCard mgMutantDetected, SomeTreacheryCard mgMasterMoldsChildren, SomeTreacheryCard mgShieldsUp, SomeTreacheryCard mgBrotherhoodBeatdown, SomeTreacheryCard mgGroundSwell, SomeTreacheryCard mgImmovable, SomeTreacheryCard mgPyromaniac, SomeTreacheryCard mgHoppingMad, SomeTreacheryCard mgMasterOfMagnetism, SomeTreacheryCard mgElectricShock, SomeTreacheryCard mgElectromagneticBlast, SomeTreacheryCard mgMetalShards, SomeTreacheryCard mgMagneticMissile, SomeTreacheryCard mgZealForTheCause, SomeTreacheryCard smSandslide, SomeTreacheryCard smSandStorm, SomeTreacheryCard smSandSmash, SomeTreacheryCard smBitingRetort, SomeTreacheryCard smForWhomTheBellTolls, SomeTreacheryCard smDejaVu, SomeTreacheryCard smFearmonger, SomeTreacheryCard smFrequentFlyers, SomeTreacheryCard smHighFashion, SomeTreacheryCard smRoboticEnhancements, SomeTreacheryCard smPartnershipOfPain, SomeTreacheryCard smSurprise, SomeTreacheryCard smSpreadingPanic, SomeTreacheryCard smCallingInFavors, SomeTreacheryCard smNowOrNever, SomeTreacheryCard smThreatOrMenace, SomeTreacheryCard smLooseEnds, SomeTreacheryCard smRemoteNavigation, SomeTreacheryCard smFromEveryDirection, SomeTreacheryCard smDeepestFears, SomeTreacheryCard smSwingingAssault, SomeTreacheryCard smUnstableSentience, SomeTreacheryCard smSmearCampaign
  , SomeTreacheryCard Hood.fieldRecruitment
  , SomeTreacheryCard Hood.upperHand
  , SomeTreacheryCard hardToKeepDown
  , SomeTreacheryCard imTough
  , SomeTreacheryCard stampede
  , SomeTreacheryCard explosion
  , SomeTreacheryCard falseAlarm
  , SomeTreacheryCard klawsVengeance
  , SomeTreacheryCard sonicBoom
  , SomeTreacheryCard soundManipulation
  , SomeTreacheryCard mastersOfMayhem
  , SomeTreacheryCard heartShapedHerb
  , SomeTreacheryCard ritualCombat
  , SomeTreacheryCard titaniasFury
  , SomeTreacheryCard sweepingSwoop
  , SomeTreacheryCard theVulturesPlans
  , SomeTreacheryCard electricWhipAttack
  , SomeTreacheryCard electromagneticBacklash
  , SomeTreacheryCard kreeManipulator
  , SomeTreacheryCard yonRoggsTreason
  , SomeTreacheryCard advance
  , SomeTreacheryCard assault
  , SomeTreacheryCard caughtOffGuard
  , SomeTreacheryCard gangUp
  , SomeTreacheryCard shadowOfThePast
  , SomeTreacheryCard androidEfficiencyA
  , SomeTreacheryCard androidEfficiencyB
  , SomeTreacheryCard androidEfficiencyC
  , SomeTreacheryCard rageOfUltron
  , SomeTreacheryCard repairSequence
  , SomeTreacheryCard swarmAttack
  , SomeTreacheryCard concussiveBlast
  , SomeTreacheryCard exhaustion
  , SomeTreacheryCard masterplan
  , SomeTreacheryCard underFire
  , SomeTreacheryCard hailHydra
  , SomeTreacheryCard harvest
  , SomeTreacheryCard deadlyShot
  , SomeTreacheryCard trickster
  , SomeTreacheryCard thoughtcasting
  , SomeTreacheryCard clashOfTheTitans
  -- The Green Goblin
  , SomeTreacheryCard allInADaysWork
  , SomeTreacheryCard deathFromAbove
  , SomeTreacheryCard iSeeYou
  , SomeTreacheryCard madGenius
  , SomeTreacheryCard overconfidence
  , SomeTreacheryCard wickedAmbitions
  , SomeTreacheryCard intimidation
  , SomeTreacheryCard regenerativeHealing
  , SomeTreacheryCard (reprintAs Cards.gangUpAMessOfThings gangUp)
  , SomeTreacheryCard tailSweep
  , SomeTreacheryCard electromagneticPulse
  , SomeTreacheryCard lightningBolt
  , SomeTreacheryCard shockTherapy

  -- The Wrecking Crew
  , SomeTreacheryCard buddySystem
  , SomeTreacheryCard (reprintAs Cards.buddySystemThunderball buddySystem)
  , SomeTreacheryCard (reprintAs Cards.buddySystemPiledriver buddySystem)
  , SomeTreacheryCard (reprintAs Cards.buddySystemBulldozer buddySystem)
  , SomeTreacheryCard chaosInThePrison
  , SomeTreacheryCard (reprintAs Cards.chaosInThePrisonThunderball chaosInThePrison)
  , SomeTreacheryCard (reprintAs Cards.chaosInThePrisonBulldozer chaosInThePrison)
  , SomeTreacheryCard crowbarToss
  , SomeTreacheryCard getWrecked
  , SomeTreacheryCard (reprintAs Cards.getWreckedThunderball getWrecked)
  , SomeTreacheryCard (reprintAs Cards.getWreckedPiledriver getWrecked)
  , SomeTreacheryCard (reprintAs Cards.getWreckedBulldozer getWrecked)
  , SomeTreacheryCard iveBeenWaitingForThis
  , SomeTreacheryCard (reprintAs Cards.iveBeenWaitingForThisThunderball iveBeenWaitingForThis)
  , SomeTreacheryCard (reprintAs Cards.iveBeenWaitingForThisPiledriver iveBeenWaitingForThis)
  , SomeTreacheryCard mysticalLink
  , SomeTreacheryCard youreDeadMeat
  , SomeTreacheryCard energyProjectiles
  , SomeTreacheryCard lightningBlast
  , SomeTreacheryCard tacticalProwess
  , SomeTreacheryCard oversizedHands
  , SomeTreacheryCard escapePlan
  , SomeTreacheryCard pummel
  , SomeTreacheryCard uncannyResilience
  , SomeTreacheryCard bullRush
  , SomeTreacheryCard headbutt
  , SomeTreacheryCard leadingTheCharge

  -- The Rise of Red Skull
  , SomeTreacheryCard sniperShot
  , SomeTreacheryCard (reprintAs Cards.hailHydraSpiderWoman hailHydra)
  , SomeTreacheryCard fullAuto
  , SomeTreacheryCard hardAsNails
  , SomeTreacheryCard raidTheArmory
  , SomeTreacheryCard stallTactics
  , SomeTreacheryCard swingingStone
  , SomeTreacheryCard steelKick
  , SomeTreacheryCard piercingThorns
  , SomeTreacheryCard omniMorphDuplication
  , SomeTreacheryCard icyGrip
  , SomeTreacheryCard (reprintAs Cards.hailHydraHydraAssault hailHydra)
  , SomeTreacheryCard weaponMaster
  , SomeTreacheryCard concussionGrenade

  -- Zola
  , SomeTreacheryCard mindRay
  , SomeTreacheryCard technologicalEnhancements

  -- Red Skull
  , SomeTreacheryCard bitterRival
  , SomeTreacheryCard spreadingLies
  , SomeTreacheryCard infinitePower

  -- Taskmaster
  , SomeTreacheryCard mimicry
  , SomeTreacheryCard huntedByHydra

  -- The Once and Future Kang
  , SomeTreacheryCard kangsChosen
  , SomeTreacheryCard ancientGrudge
  , SomeTreacheryCard energyBlast
  , SomeTreacheryCard manipulatedTimestream
  , SomeTreacheryCard timeTravelTactics
  , SomeTreacheryCard pastMachinations


  -- Ant-Man
  , SomeTreacheryCard yellowjacketsPlan

  -- Wasp
  , SomeTreacheryCard beetleMania

  -- Quicksilver
  , SomeTreacheryCard earthquake

  -- Scarlet Witch
  , SomeTreacheryCard chaosManipulation

  -- Star-Lord
  , SomeTreacheryCard spartoiCunning

  -- Gamora
  , SomeTreacheryCard waylay

  -- Groot
  , SomeTreacheryCard fanTheFlames

  -- Rocket Raccoon
  , SomeTreacheryCard planetaryInvasion

  -- The Galaxy's Most Wanted
  , SomeTreacheryCard blindSide
  , SomeTreacheryCard hullBreach
  , SomeTreacheryCard powerSiphon
  , SomeTreacheryCard specialDelivery
  , SomeTreacheryCard onTheHunt
  , SomeTreacheryCard deadToRights

  -- The Galaxy's Most Wanted: Escape the Museum
  , SomeTreacheryCard iHaveYouNow
  , SomeTreacheryCard impossibleGeometry
  -- Ronan the Accuser
  , SomeTreacheryCard singleMindedFury
  , SomeTreacheryCard kreePhysiology
  , SomeTreacheryCard youStandAccused
  -- The Galaxy's Most Wanted: Nebula, Space Pirates
  , SomeTreacheryCard barrelRoll
  , SomeTreacheryCard combatReady
  , SomeTreacheryCard ruthless
  , SomeTreacheryCard honorAmongThieves

  -- The Galaxy's Most Wanted: Infiltrate the Museum
  , SomeTreacheryCard inconspicuousBox
  , SomeTreacheryCard viewTheCosmos
  , SomeTreacheryCard stayAwhile
  , SomeTreacheryCard caughtOffGuardMuseum
  -- Drax
  , SomeTreacheryCard iWillDestroyYou
  -- Spectrum
  , SomeTreacheryCard radioactiveBlast
  -- Adam Warlock
  , SomeTreacheryCard cosmicInquisition
  -- The Mad Titan's Shadow: Ebony Maw, Black Order
  , SomeTreacheryCard agentOfThanos
  , SomeTreacheryCard channelingTrance
  , SomeTreacheryCard bloodToSpare

  -- The Mad Titan's Shadow: Tower Defense
  , SomeTreacheryCard proximasPower
  , SomeTreacheryCard corvussCunning
  , SomeTreacheryCard boundByBlood
  , SomeTreacheryCard rainFire

  -- The Mad Titan's Shadow: Thanos
  , SomeTreacheryCard avatarOfDeath
  , SomeTreacheryCard deviantSyndrome
  , SomeTreacheryCard iAmInevitable
  , SomeTreacheryCard theMadTitan

  -- The Mad Titan's Shadow: Loki
  , SomeTreacheryCard deviousSorcery
  , SomeTreacheryCard infiniteMischief
  , SomeTreacheryCard theTrickster

  -- The Mad Titan's Shadow: Hela, Legions of Hel
  , SomeTreacheryCard helasDomain
  , SomeTreacheryCard theQueenOfHel
  , SomeTreacheryCard theWastesOfNiffleheim
  , SomeTreacheryCard noPlaceForTheLiving

  -- The Mad Titan's Shadow: the campaign set
  , SomeTreacheryCard summonedBack
  , SomeTreacheryCard oldRivals
  , SomeTreacheryCard laserStrike
  , SomeTreacheryCard relentlessAndroid
  , SomeTreacheryCard inColdBlood
  , SomeTreacheryCard sliceAndDice
  , SomeTreacheryCard warDelivery
  , SomeTreacheryCard theWarsBeenBrought
  , SomeTreacheryCard toughAndTumble
  , SomeTreacheryCard toughItOut
  , SomeTreacheryCard politicalRetribution
  , SomeTreacheryCard zzzap
  , SomeTreacheryCard feastOnThis
  , SomeTreacheryCard energyDrain
  , SomeTreacheryCard slammed
  , SomeTreacheryCard fieryRage
  ]

lookupTreachery :: CardCode -> TreacheryId -> Treachery
lookupTreachery cardCode = case lookup cardCode allTreacheries of
  Just (SomeTreacheryCard a) -> Treachery <$> buildCard a
  Nothing -> error $ "Invalid card code for treachery " <> show cardCode
