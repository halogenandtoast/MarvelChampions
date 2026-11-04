module Marvel.Treachery.Cards where

import Marvel.FearNoEvil.Cards qualified as OwlDefs

import Marvel.Shadowland.Cards qualified as ShadowsDefs

import Marvel.Shadowland.Cards qualified as DarkDefs

import Marvel.Shadowland.Cards qualified as FlowerCards

import Marvel.Shadowland.Cards qualified as StreetTrapsCards

import Marvel.FearNoEvil.Cards qualified as FNE

import Marvel.Shadowland.Cards qualified as SL

import Marvel.CivilWar.Cards qualified as CW
import Marvel.CampaignPlayers.EncounterCards qualified as CN
import Marvel.AgentsOfShield.EncounterCards qualified as AosEncounter
import Marvel.FalconWinter.Cards qualified as FalconWinter

import Marvel.Ability.Types qualified as AospAbility ()
import Marvel.Aspect qualified as AospAspect ()
import Marvel.Boost qualified as AospBoost
import Marvel.Card.Def qualified as AospCard
import Marvel.Cost.Types qualified as AospCost
import Marvel.Criteria qualified as AospCriteria
import Marvel.EncounterSet qualified as AospSet
import Marvel.Keyword qualified as AospKeyword
import Marvel.Matchers qualified as AospMatch ()
import Marvel.Name qualified as AospName ()
import Marvel.Resource.Types qualified as AospResource ()
import Marvel.Trait qualified as AospTrait ()
import Marvel.Window.Types qualified as AospWindow ()

import Marvel.BlackPantherShuri.Cards qualified as BlackPantherShuriPack
import Marvel.Silk.Cards qualified as SilkPack

import Marvel.TricksterTakeover.Cards qualified as TT

import Marvel.Window.Types qualified as NcWindow ()

import Marvel.Trait.Types qualified as NcTrait ()

import Marvel.Resource.Types qualified as NcResource ()

import Marvel.Name qualified as NcName ()

import Marvel.Matchers qualified as NcMatch ()

import Marvel.Keyword qualified as NcKeyword ()

import Marvel.EncounterSet qualified as NcSet

import Marvel.Criteria qualified as NcCriteria

import Marvel.Cost.Types qualified as NcCost

import Marvel.Card.Def qualified as NcCard ()

import Marvel.Boost qualified as NcBoost

import Marvel.Aspect qualified as NcAspect ()

import Marvel.Ability.Types qualified as NcAbility ()

import Marvel.Iceman.Cards qualified as IcemanPack
import Marvel.Jubilee.Cards qualified as Jubilee

import Marvel.Ability.Types qualified as MgAbility ()
import Marvel.Aspect qualified as MgAspect ()
import Marvel.Boost qualified as MgBoost
import Marvel.Card.Def qualified as MgCard
import Marvel.Cost.Types qualified as MgCost
import Marvel.Criteria qualified as MgCriteria
import Marvel.EncounterSet qualified as MgSet
import Marvel.Keyword qualified as MgKeyword ()
import Marvel.Matchers qualified as MgMatch ()
import Marvel.Name qualified as MgName ()
import Marvel.Resource.Types qualified as MgResource ()
import Marvel.Trait qualified as MgTrait ()
import Marvel.Window.Types qualified as MgWindow ()




import Marvel.Boost qualified as AoaMB
import Marvel.EncounterSet qualified as AoaMS
import Marvel.Boost qualified as BpBoost
import Marvel.Card.Def qualified as BpCard
import Marvel.Cost.Types qualified as BpCost
import Marvel.Criteria qualified as BpCriteria
import Marvel.EncounterSet qualified as BpSet
import Marvel.Trait qualified as BpTrait

import Marvel.Ability.Types qualified as DpAbility ()
import Marvel.Aspect qualified as DpAspect ()
import Marvel.Boost qualified as DpBoost
import Marvel.Card.Def qualified as DpCard
import Marvel.Cost.Types qualified as DpCost
import Marvel.Criteria qualified as DpCriteria
import Marvel.EncounterSet qualified as DpSet
import Marvel.Keyword qualified as DpKeyword ()
import Marvel.Matchers qualified as DpMatch ()
import Marvel.Name qualified as DpName
import Marvel.Resource.Types qualified as DpResource ()
import Marvel.Trait qualified as DpTrait ()
import Marvel.Window.Types qualified as DpWindow ()

import Marvel.Ability.Types qualified as AnAbility ()
import Marvel.Aspect qualified as AnAspect ()
import Marvel.Boost qualified as AnBoost
import Marvel.Card.Def qualified as AnCard
import Marvel.Cost.Types qualified as AnCost
import Marvel.Criteria qualified as AnCriteria
import Marvel.EncounterSet qualified as AnSet
import Marvel.Keyword qualified as AnKeyword ()
import Marvel.Matchers qualified as AnMatch ()
import Marvel.Name qualified as AnName ()
import Marvel.Resource.Types qualified as AnResource ()
import Marvel.Trait qualified as AnTrait ()
import Marvel.Window.Types qualified as AnWindow ()

import Marvel.Ability.Types qualified as PsyAbility ()
import Marvel.Aspect qualified as PsyAspect ()
import Marvel.Boost qualified as PsyBoost
import Marvel.Card.Def qualified as PsyCard
import Marvel.Cost.Types qualified as PsyCost
import Marvel.Criteria qualified as PsyCriteria
import Marvel.EncounterSet qualified as PsySet
import Marvel.Keyword qualified as PsyKeyword ()
import Marvel.Matchers qualified as PsyMatch ()
import Marvel.Name qualified as PsyName ()
import Marvel.Resource.Types qualified as PsyResource ()
import Marvel.Trait qualified as PsyTrait ()
import Marvel.Window.Types qualified as PsyWindow ()

import Marvel.Hood.Cards (hoodCardDef)
import Marvel.EncounterSet qualified as HoodSet
import Marvel.Trait qualified as HoodTrait ()
import Marvel.Keyword qualified as HoodKeyword
import Marvel.Boost qualified as HoodBoost
import Marvel.GameValue.Types qualified as HoodValue ()



import Marvel.Resource.Types qualified as NEResource ()

import Marvel.Boost qualified as NEBoost

import Marvel.GameValue.Types qualified as NEValue ()

import Marvel.EncounterSet qualified as NESet

import Marvel.Keyword qualified as NEKeyword

import Marvel.Trait qualified as NETrait

import Marvel.Aspect qualified as NEAspect ()

import Marvel.Boost qualified as NE
import Marvel.Ability.Types qualified as NEAbility ()

import Marvel.Trait qualified as MojoTrait ()
import Marvel.EncounterSet qualified as MojoSet
import Marvel.Keyword qualified as MojoKeyword
import Marvel.GameValue.Types qualified as MojoValue ()
import Marvel.Boost qualified as MojoBoost

import Marvel.EncounterSet qualified as SpdrSet
import Marvel.Resource ()
import Marvel.EncounterSet qualified as SMSet
import Marvel.Trait qualified as SMTrait ()
import Marvel.Keyword qualified as SMKeyword
import Marvel.GameValue.Types qualified as SMValue ()
import Marvel.Boost qualified as SMBoost

import Marvel.Keyword
import Marvel.Cost.Types
import Marvel.Window ()
import Marvel.Matchers ()
import Marvel.Trait qualified as MGTrait
import Marvel.EncounterSet qualified as MGSet
import Marvel.Keyword qualified as MGKeyword
import Marvel.GameValue.Types qualified as MGValue ()
import Marvel.Boost qualified as MGBoost
import Marvel.WonderMan.Cards qualified as WM

import Marvel.Hercules.Cards qualified as HC

import Marvel.CivilWar.StandardTreacheries qualified as CWStandard
import Marvel.Prelude


import Data.HashSet (singleton)
import Marvel.Boost
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Criteria
import Marvel.EncounterSet
import Marvel.Keyword qualified as Keyword
import Marvel.Name

allTreacheries :: HashMap CardCode CardDef
allTreacheries = fromList $ map
  (toFst toCardCode) $ CWStandard.standardTreacheryDefinitions <>
  [ CW.cw56089,CW.cw56090,CW.cw56085,CW.cw56076,CW.cw56081,CW.cw56070,CW.cw56071,CW.cw56101,CW.cw56102,CW.cw56103,CW.cw56146,CW.cw56147,CW.cw56176,CW.cw57009,CW.cw57010,CW.cw57051,CW.cw57052,CW.cw57053,CW.cw56148,CW.cw56177,CW.cw56178,CW.cw57011
  , CN.enc56058, CN.enc60036, CN.enc60064
  , bringTheHammerDown, youDareOpposeMe
  , WM.wm58028, WM.wm58029, HC.hc59040
  , SL.street65036, FNE.street60181, StreetTrapsCards.street65050, StreetTrapsCards.street65051, StreetTrapsCards.street65052, StreetTrapsCards.street65053, StreetTrapsCards.street65054, StreetTrapsCards.street65055, StreetTrapsCards.street65056, FlowerCards.street65077, FlowerCards.street65078, DarkDefs.street65026, DarkDefs.street65027, ShadowsDefs.street65010, ShadowsDefs.street65011, ShadowsDefs.street65044, ShadowsDefs.street65045, ShadowsDefs.street65071, OwlDefs.street60096, OwlDefs.street60083, OwlDefs.street60084, OwlDefs.street60085, OwlDefs.street60074, OwlDefs.street60075, OwlDefs.street60190, OwlDefs.street60194, TT.tt55023, TT.tt55024, TT.tt55025, TT.tt55026, TT.tt55049, TT.tt55050, TT.tt55051
  , aos50076, aos50077, aos50078, aos50079, aos50082, aos50096, aos50097, aos50102, aos50123, aos50124, aos50128, aos50137, aos50138, aos50141, aos50142, aos50147, aos50151, aos50155, aos50160, aos50163, aos50164, aos50175, aos50176, aos50177, aos50184a, aos50184b, aos50184c
  , FalconWinter.fw53033, FalconWinter.fw53042, FalconWinter.fw54037
  , aosP50033, aosP50063
  , BlackPantherShuriPack.theScreamShuri, BlackPantherShuriPack.extremeRiskShuri, SilkPack.huntingtheSpiderBrideSilk, SilkPack.growingStrongSilk, SilkPack.titanicProportionsSilk
  , nc48030, nc48038
  , burnIceman
  , theEyeOfSauron
  , Jubilee.jubilee47034
  , angryAcolyte, powerAndDecadence
  , infinitePrelate, cullingTheWeak, warWearyDystopianNightmare, darkDesigns, sinisterStrike, evilAlliance, nowhereIsSafe, horsemanOfWar, horsemanOfFamine, horsemanOfPestilence, horsemanOfDeath, roughRiders, wolfAmongSheep, evilGenius, landOutOfTime, biomorphicBlast, technologicalInterface, giantSizedDespot, celestialTech, tyrantWorship, worldwideCrisis
  , battleForLimbo
  , temporalTrickery
  , crisisOfInfiniteDeadpools, metacidalTendencies
  , hackNSlashX23
  , spearShot
  , telekineticDragon
  , hood24017, hood24028, hood24029, hood24030, hood24031, hood24032, hood24036, hood24048, hood24050, hood24051, hood24052, hood24053, hood24054, hood24059, hood24069, hood24070
      , ne40154
  , ne40080, ne40088, ne40089, ne40102, ne40110, ne40111, ne40125, ne40126, ne40127, ne40128, ne40129, ne40135, ne40147, ne40148, ne40149, ne40150, ne40158, ne40162, ne40177, ne40178, ne40179, ne40203
  , cable40035, cable40036
  , mojo39009, mojo39010, mojo39011, mojo39017, mojo39019, mojo39020, mojo39021, mojo39032, mojo39033, mojo39034, mojo39040, mojo39046, mojo39070
  , misled
  , assassinationAttempt, acolyteFrenzy
  , knifeFight, astralAttack
  , concussiveForce
  , tentacleStrike, hackNSlash
  , mgSabretoothStrikes, mgUnrelentingSavage, mgMutantTerrorists, mgInfiltration, mgShapeshifterSurprise, mgSelfRepair, mgMutantDetected, mgMasterMoldsChildren, mgShieldsUp, mgBrotherhoodBeatdown, mgGroundSwell, mgImmovable, mgPyromaniac, mgHoppingMad, mgMasterOfMagnetism, mgElectricShock, mgElectromagneticBlast, mgMetalShards, mgMagneticMissile, mgZealForTheCause, smSandslide, smSandStorm, smSandSmash, smBitingRetort, smForWhomTheBellTolls, smDejaVu, smFearmonger, smFrequentFlyers, smHighFashion, smRoboticEnhancements, smPartnershipOfPain, smSurprise, smSpreadingPanic, smCallingInFavors, smNowOrNever, smThreatOrMenace, smLooseEnds, smRemoteNavigation, smFromEveryDirection, smDeepestFears, smSwingingAssault, smUnstableSentience, smSmearCampaign
  , fieldRecruitment
  , upperHand
  , hardToKeepDown
  , imTough
  , stampede
  , explosion
  , falseAlarm
  , klawsVengeance
  , sonicBoom
  , soundManipulation
  , mastersOfMayhem
  , heartShapedHerb
  , ritualCombat
  , titaniasFury
  , sweepingSwoop
  , theVulturesPlans
  , electricWhipAttack
  , electromagneticBacklash
  , kreeManipulator
  , yonRoggsTreason
  , advance
  , assault
  , caughtOffGuard
  , gangUp
  , shadowOfThePast
  , androidEfficiencyA
  , androidEfficiencyB
  , androidEfficiencyC
  , rageOfUltron
  , repairSequence
  , swarmAttack
  , concussiveBlast
  , exhaustion
  , masterplan
  , underFire
  , hailHydra
  , harvest
  , deadlyShot
  , trickster
  , thoughtcasting
  , clashOfTheTitans
  -- The Green Goblin
  , allInADaysWork
  , madGenius
  , deathFromAbove
  , iSeeYou
  , overconfidence
  , wickedAmbitions
  , intimidation
  , regenerativeHealing
  , gangUpAMessOfThings
  , tailSweep
  , electromagneticPulse
  , lightningBolt
  , shockTherapy

  -- The Wrecking Crew
  , buddySystem
  , buddySystemThunderball
  , buddySystemPiledriver
  , buddySystemBulldozer
  , chaosInThePrison
  , chaosInThePrisonThunderball
  , chaosInThePrisonBulldozer
  , crowbarToss
  , getWrecked
  , getWreckedThunderball
  , getWreckedPiledriver
  , getWreckedBulldozer
  , iveBeenWaitingForThis
  , iveBeenWaitingForThisThunderball
  , iveBeenWaitingForThisPiledriver
  , mysticalLink
  , youreDeadMeat
  , energyProjectiles
  , lightningBlast
  , tacticalProwess
  , oversizedHands
  , escapePlan
  , pummel
  , uncannyResilience
  , bullRush
  , headbutt
  , leadingTheCharge

  -- The Rise of Red Skull
  , sniperShot
  , hailHydraSpiderWoman
  , fullAuto
  , hardAsNails
  , raidTheArmory
  , stallTactics
  , swingingStone
  , steelKick
  , piercingThorns
  , omniMorphDuplication
  , icyGrip
  , hailHydraHydraAssault
  , weaponMaster
  , concussionGrenade

  -- Zola
  , mindRay
  , technologicalEnhancements

  -- Red Skull
  , bitterRival
  , spreadingLies
  , infinitePower

  -- Taskmaster
  , mimicry
  , huntedByHydra

  -- The Once and Future Kang
  , kangsChosen
  , ancientGrudge
  , energyBlast
  , manipulatedTimestream
  , timeTravelTactics
  , pastMachinations


  -- Ant-Man
  , yellowjacketsPlan

  -- Wasp
  , beetleMania

  -- Quicksilver
  , earthquake

  -- Scarlet Witch
  , chaosManipulation

  -- Star-Lord
  , spartoiCunning

  -- Gamora
  , waylay

  -- Groot
  , fanTheFlames

  -- Rocket Raccoon
  , planetaryInvasion

  -- The Galaxy's Most Wanted: Ship Command
  , blindSide
  , hullBreach
  , powerSiphon
  , specialDelivery
  -- Badoon Headhunter
  , onTheHunt
  , deadToRights

  -- The Galaxy's Most Wanted: Escape the Museum
  , iHaveYouNow
  , impossibleGeometry
  -- Ronan the Accuser
  , singleMindedFury
  , kreePhysiology
  , youStandAccused

  -- The Galaxy's Most Wanted: Nebula
  , barrelRoll
  , combatReady
  , ruthless
  -- Space Pirates
  , honorAmongThieves

  -- The Galaxy's Most Wanted: Infiltrate the Museum
  , inconspicuousBox
  , viewTheCosmos
  , stayAwhile
  , caughtOffGuardMuseum
  -- Drax
  , iWillDestroyYou
  -- Spectrum
  , radioactiveBlast

  -- Adam Warlock
  , cosmicInquisition

  -- The Mad Titan's Shadow: Ebony Maw
  , agentOfThanos
  , channelingTrance
  -- Black Order
  , bloodToSpare

  -- The Mad Titan's Shadow: Tower Defense
  , proximasPower
  , corvussCunning
  , boundByBlood
  , rainFire

  -- The Mad Titan's Shadow: Thanos
  , avatarOfDeath
  , deviantSyndrome
  , iAmInevitable
  , theMadTitan
  -- The Mad Titan's Shadow: Loki
  , deviousSorcery
  , infiniteMischief
  , theTrickster

  -- The Mad Titan's Shadow: Hela
  , helasDomain
  , theQueenOfHel
  , theWastesOfNiffleheim
  -- Legions of Hel
  , noPlaceForTheLiving

  -- The Mad Titan's Shadow: the campaign set
  , summonedBack
  , oldRivals
  , laserStrike
  , relentlessAndroid
  , inColdBlood
  , sliceAndDice
  , warDelivery
  , theWarsBeenBrought
  , toughAndTumble
  , toughItOut
  , politicalRetribution
  , zzzap
  , feastOnThis
  , energyDrain
  , slammed
  , fieryRage
  ]

treachery
  :: CardCode -> Name -> [BoostIcon] -> EncounterSet -> Natural -> CardDef
treachery code name boostIcons encounterSet quantity = CardDef
  { cdCardCode = code
  , cdName = name
  , cdCost = Nothing
  , cdTraits = mempty
  , cdKeywords = mempty
  , cdCardType = TreacheryType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdEncounterSet = Just encounterSet
  , cdEncounterSetQuantity = Just quantity
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdBoostIcons = boostIcons
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = NoCost
  }

hardToKeepDown :: CardDef
hardToKeepDown = treachery "01104" "Hard to Keep Down" [] Rhino 2

imTough :: CardDef
imTough = treachery "01105" "\"I'm Tough!\"" [] Rhino 2

stampede :: CardDef
stampede = treachery "01106" "Stampede" [Boost] Rhino 3

explosion :: CardDef
explosion = treachery "01111" "Explosion" [Boost, Boost] BombScare 1

falseAlarm :: CardDef
falseAlarm = treachery "01112" "False Alarm" [Boost] BombScare 2

klawsVengeance :: CardDef
klawsVengeance = treachery "01122" "Klaw's Vengeance" [Boost] Klaw 2

sonicBoom :: CardDef
sonicBoom = treachery "01123" "Sonic Boom" [Star] Klaw 2

soundManipulation :: CardDef
soundManipulation =
  treachery "01124" "Sound Manipulation" [Boost, Boost] Klaw 2

mastersOfMayhem :: CardDef
mastersOfMayhem =
  treachery "01133" "Masters of Mayhem" [Boost, Boost] MastersOfEvil 2

heartShapedHerb :: CardDef
heartShapedHerb =
  treachery "01158" "Heart-Shaped Herb" [Star, Boost] BlackPantherNemesis 1

ritualCombat :: CardDef
ritualCombat =
  treachery "01159" "Ritual Combat" [Boost, Boost] BlackPantherNemesis 2

titaniasFury :: CardDef
titaniasFury =
  treachery "01164" "Titania's Fury" [Star, Boost] SheHulkNemesis 2

sweepingSwoop :: CardDef
sweepingSwoop = treachery "01168" "Sweeping Swoop" [Star] SpiderManNemesis 2

theVulturesPlans :: CardDef
theVulturesPlans =
  treachery "01169" "The Vulture's Plans" [Boost, Boost] SpiderManNemesis 1

electricWhipAttack :: CardDef
electricWhipAttack =
  treachery "01173" "Electric Whip Attack" [Star] IronManNemesis 2

electromagneticBacklash :: CardDef
electromagneticBacklash =
  treachery "01174" "Electromagnetic Backlash" [Boost, Boost] IronManNemesis 1

kreeManipulator :: CardDef
kreeManipulator =
  (treachery "01178" "Kree Manipulator" [Star] CaptainMarvelNemesis 2)
    { cdKeywords = singleton Keyword.Surge
    }

yonRoggsTreason :: CardDef
yonRoggsTreason =
  treachery "01179" "Yon-Rogg's Treason" [Boost] CaptainMarvelNemesis 1

advance :: CardDef
advance = treachery "01186" "Advance" [] Standard 2

assault :: CardDef
assault = treachery "01187" "Assault" [] Standard 2

caughtOffGuard :: CardDef
caughtOffGuard = treachery "01188" "Caught Off Guard" [Boost] Standard 1

gangUp :: CardDef
gangUp = treachery "01189" "Gang-Up" [Boost] Standard 1

shadowOfThePast :: CardDef
shadowOfThePast =
  treachery "01190" "Shadow of the Past" [Boost, Boost] Standard 1

-- | Android Efficiency's three copies differ in the resource their boost asks for.
androidEfficiencyA :: CardDef
androidEfficiencyA = treachery "01144a" "Android Efficiency" [Star] Ultron 1

androidEfficiencyB :: CardDef
androidEfficiencyB = treachery "01144b" "Android Efficiency" [Star] Ultron 1

androidEfficiencyC :: CardDef
androidEfficiencyC = treachery "01144c" "Android Efficiency" [Star] Ultron 1

rageOfUltron :: CardDef
rageOfUltron = treachery "01145" "Rage of Ultron" [Boost, Boost] Ultron 2

repairSequence :: CardDef
repairSequence = treachery "01146" "Repair Sequence" [Boost, Star] Ultron 2

swarmAttack :: CardDef
swarmAttack = treachery "01147" "Swarm Attack" [Boost] Ultron 2

concussiveBlast :: CardDef
concussiveBlast = treachery "01154" "Concussive Blast" [Star] UnderAttack 2

exhaustion :: CardDef
exhaustion =
  (treachery "01191" "Exhaustion" [Boost, Boost] ExpertSet 1)
    { cdKeywords = singleton Keyword.Surge
    }

masterplan :: CardDef
masterplan = treachery "01192" "Masterplan" [Boost, Boost] ExpertSet 1

underFire :: CardDef
underFire =
  (treachery "01193" "Under Fire" [Boost, Boost, Boost] ExpertSet 1)
    { cdKeywords = singleton Keyword.Surge
    }

hailHydra :: CardDef
hailHydra = treachery "03030" "Hail Hydra!" [] CaptainAmericaNemesis 1

harvest :: CardDef
harvest = treachery "05029" "Harvest" [] MsMarvelNemesis 2
clashOfTheTitans :: CardDef
clashOfTheTitans =
  treachery "10028" "Clash of the Titans" [Boost, Boost, Boost] HulkNemesis 3

deadlyShot :: CardDef
deadlyShot = treachery "08029" "Deadly Shot" [Boost] BlackWidowNemesis 1

trickster :: CardDef
trickster = treachery "06030" "Trickster" [Boost] ThorNemesis 1

thoughtcasting :: CardDef
thoughtcasting = treachery "09031" "Thoughtcasting" [Boost] DoctorStrangeNemesis 1

sniperShot :: CardDef
sniperShot = treachery "04030" "Sniper Shot" [Boost] HawkeyeNemesis 2

-- | Hail Hydra! as Spider-Woman's nemesis set has it
hailHydraSpiderWoman :: CardDef
hailHydraSpiderWoman =
  (reprint "04057" hailHydra)
    { cdEncounterSet = Just SpiderWomanNemesis
    , cdEncounterSetQuantity = Just 1
    , cdBoostIcons = [Boost, Boost]
    }

fullAuto :: CardDef
fullAuto = treachery "04067" "Full Auto" [Boost, Boost] Crossbones 2

hardAsNails :: CardDef
hardAsNails = treachery "04068" "Hard as Nails" [Star] Crossbones 2

raidTheArmory :: CardDef
raidTheArmory =
  (treachery "04069" "Raid the Armory" [Boost] Crossbones 2)
    { cdKeywords = singleton (Keyword.Incite 1)
    }

stallTactics :: CardDef
stallTactics = treachery "04085" "Stall Tactics" [Star] AbsorbingMan 2

swingingStone :: CardDef
swingingStone = treachery "04086" "Swinging Stone" [Boost] AbsorbingMan 2

steelKick :: CardDef
steelKick = treachery "04087" "Steel Kick" [Boost, Boost] AbsorbingMan 2

piercingThorns :: CardDef
piercingThorns = treachery "04088" "Piercing Thorns" [Boost, Star] AbsorbingMan 2

omniMorphDuplication :: CardDef
omniMorphDuplication = treachery "04089" "Omni-Morph Duplication" [Boost] AbsorbingMan 3

icyGrip :: CardDef
icyGrip = treachery "04090" "Icy Grip" [Boost, Boost, Star] AbsorbingMan 2

-- | Captain America's nemesis set's Hail Hydra!, in the Hydra Assault set.
hailHydraHydraAssault :: CardDef
hailHydraHydraAssault =
  (reprint "04147" hailHydra)
    { cdEncounterSet = Just HydraAssault
    , cdBoostIcons = [Boost, Boost]
    }

weaponMaster :: CardDef
weaponMaster = treachery "04150" "Weapon Master" [Boost] WeaponMaster 2

concussionGrenade :: CardDef
concussionGrenade = treachery "04151" "Concussion Grenade" [Boost] WeaponMaster 1

-- The Rise of Red Skull
-- Zola

mindRay :: CardDef
mindRay = treachery "04120" "Mind Ray" [Boost, Boost] Zola 3

technologicalEnhancements :: CardDef
technologicalEnhancements =
  (treachery "04121" "Technological Enhancements" [Star] Zola 2)
    { cdKeywords = singleton (Keyword.Incite 1)
    }

-- Red Skull

bitterRival :: CardDef
bitterRival = treachery "04136" "Bitter Rival" [Star] RedSkull 2

spreadingLies :: CardDef
spreadingLies = treachery "04137" "Spreading Lies" [Boost, Star] RedSkull 2

infinitePower :: CardDef
infinitePower = treachery "04138" "Infinite Power" [Boost, Boost] RedSkull 2

-- The Green Goblin

allInADaysWork :: CardDef
allInADaysWork = treachery "02012" "All in a Day's Work" [Star] RiskyBusiness 4

madGenius :: CardDef
madGenius = treachery "02013" "Mad Genius" [Boost] RiskyBusiness 2

deathFromAbove :: CardDef
deathFromAbove = treachery "02029" "Death from Above" [Boost] MutagenFormula 2

iSeeYou :: CardDef
iSeeYou = treachery "02030" "I See You" [Boost, Star] MutagenFormula 1

overconfidence :: CardDef
overconfidence = treachery "02031" "Overconfidence" [Boost] MutagenFormula 1

wickedAmbitions :: CardDef
wickedAmbitions = treachery "02032" "Wicked Ambitions" [Boost] MutagenFormula 2
intimidation :: CardDef
intimidation = treachery "02035" "Intimidation" [Star, Boost] GoblinGimmicks 2

regenerativeHealing :: CardDef
regenerativeHealing = treachery "02036" "Regenerative Healing" [Star] GoblinGimmicks 2

-- | The Standard set's Gang-Up, one in A Mess of Things.
gangUpAMessOfThings :: CardDef
gangUpAMessOfThings =
  (reprint "02039" gangUp)
    { cdEncounterSet = Just AMessOfThings
    , cdEncounterSetQuantity = Just 1
    }

tailSweep :: CardDef
tailSweep = treachery "02040" "Tail Sweep" [Star] AMessOfThings 2

electromagneticPulse :: CardDef
electromagneticPulse = treachery "02043" "Electromagnetic Pulse" [Star, Boost, Boost] PowerDrain 1

lightningBolt :: CardDef
lightningBolt = treachery "02044" "Lightning Bolt" [Star, Boost] PowerDrain 1

shockTherapy :: CardDef
shockTherapy = treachery "02045" "Shock Therapy" [Star, Boost] PowerDrain 1

-- The Once and Future Kang

kangsChosen :: CardDef
kangsChosen =
  (treachery "11046" "Kang's Chosen" [Boost, Boost] Anachronauts 2)
    { cdKeywords = singleton (Keyword.Incite 1)
    }

ancientGrudge :: CardDef
ancientGrudge = treachery "11051" "Ancient Grudge" [Boost] MasterOfTime 2

energyBlast :: CardDef
energyBlast = treachery "11026" "Energy Blast" [Boost] Kang 2

manipulatedTimestream :: CardDef
manipulatedTimestream = treachery "11027" "Manipulated Timestream" [Boost, Boost] Kang 1

timeTravelTactics :: CardDef
timeTravelTactics = (treachery "11028" "Time-Travel Tactics" [Boost, Star] Kang 2) {cdKeywords = singleton Keyword.Surge}

pastMachinations :: CardDef
pastMachinations = (treachery "11029" "Past Machinations" [Boost, Boost, Boost] Kang 1) {cdKeywords = singleton (Keyword.Incite 1)}

-- The Rise of Red Skull

-- Taskmaster

mimicry :: CardDef
mimicry = treachery "04105" "Mimicry" [Boost] Taskmaster 2

huntedByHydra :: CardDef
huntedByHydra = (treachery "04106" "Hunted by Hydra" [Boost] Taskmaster 2) {cdKeywords = singleton (Keyword.Incite 1)}

-- The Wrecking Crew

-- | The cards shared by the Wrecking Crew's decks are reprints, one per deck.

buddySystem :: CardDef
buddySystem = treachery "07010" "Buddy System" [Star] Wrecker 1

buddySystemThunderball :: CardDef
buddySystemThunderball = (reprint "07025" buddySystem) {cdEncounterSet = Just Thunderball}

buddySystemPiledriver :: CardDef
buddySystemPiledriver = (reprint "07039" buddySystem) {cdEncounterSet = Just Piledriver}

buddySystemBulldozer :: CardDef
buddySystemBulldozer = (reprint "07054" buddySystem) {cdEncounterSet = Just Bulldozer}

chaosInThePrison :: CardDef
chaosInThePrison = treachery "07011" "Chaos In the Prison" [Star] Wrecker 1

chaosInThePrisonThunderball :: CardDef
chaosInThePrisonThunderball = (reprint "07026" chaosInThePrison) {cdEncounterSet = Just Thunderball}

chaosInThePrisonBulldozer :: CardDef
chaosInThePrisonBulldozer = (reprint "07056" chaosInThePrison) {cdEncounterSet = Just Bulldozer}

crowbarToss :: CardDef
crowbarToss = treachery "07012" "Crowbar Toss" [Boost] Wrecker 2

getWrecked :: CardDef
getWrecked = treachery "07013" "Get Wrecked!" [Boost] Wrecker 1

getWreckedThunderball :: CardDef
getWreckedThunderball = (reprint "07028" getWrecked) {cdEncounterSet = Just Thunderball}

getWreckedPiledriver :: CardDef
getWreckedPiledriver = (reprint "07040" getWrecked) {cdEncounterSet = Just Piledriver}

getWreckedBulldozer :: CardDef
getWreckedBulldozer = (reprint "07057" getWrecked) {cdEncounterSet = Just Bulldozer}

iveBeenWaitingForThis :: CardDef
iveBeenWaitingForThis = treachery "07014" "I've Been Waiting For This!" [Star] Wrecker 1

iveBeenWaitingForThisThunderball :: CardDef
iveBeenWaitingForThisThunderball = (reprint "07029" iveBeenWaitingForThis) {cdEncounterSet = Just Thunderball}

iveBeenWaitingForThisPiledriver :: CardDef
iveBeenWaitingForThisPiledriver = (reprint "07041" iveBeenWaitingForThis) {cdEncounterSet = Just Piledriver}

mysticalLink :: CardDef
mysticalLink = treachery "07015" "Mystical Link" [Star] Wrecker 2

youreDeadMeat :: CardDef
youreDeadMeat = treachery "07016" "You're Dead Meat!" [Boost, Boost] Wrecker 2

energyProjectiles :: CardDef
energyProjectiles = treachery "07027" "Energy Projectiles" [Star] Thunderball 2

lightningBlast :: CardDef
lightningBlast = treachery "07030" "Lightning Blast" [Boost, Boost] Thunderball 2

tacticalProwess :: CardDef
tacticalProwess = treachery "07031" "Tactical Prowess" [Boost, Boost] Thunderball 2

oversizedHands :: CardDef
oversizedHands = treachery "07042" "Oversized Hands" [Star] Piledriver 2

escapePlan :: CardDef
escapePlan = treachery "07043" "Escape Plan" [Star] Piledriver 2

pummel :: CardDef
pummel = treachery "07044" "Pummel" [Boost, Boost] Piledriver 2

uncannyResilience :: CardDef
uncannyResilience = treachery "07045" "Uncanny Resilience" [Star] Piledriver 1

bullRush :: CardDef
bullRush = treachery "07055" "Bull Rush" [Boost] Bulldozer 2

headbutt :: CardDef
headbutt = treachery "07058" "Headbutt" [Boost] Bulldozer 2

leadingTheCharge :: CardDef
leadingTheCharge = treachery "07059" "Leading the Charge" [Boost, Boost] Bulldozer 2

-- Ant-Man

yellowjacketsPlan :: CardDef
yellowjacketsPlan = treachery "12029" "Yellowjacket's Plan" [Boost] AntManNemesis 1

-- Wasp

beetleMania :: CardDef
beetleMania = treachery "13030" "Beetle Mania" [Boost] WaspNemesis 2

-- Quicksilver

earthquake :: CardDef
earthquake = (treachery "14028" "Earthquake" [Star] QuicksilverNemesis 2) {cdKeywords = singleton (Keyword.Incite 1)}

-- Scarlet Witch

chaosManipulation :: CardDef
chaosManipulation = treachery "15027" "Chaos Manipulation" [Boost, Boost] ScarletWitchNemesis 1

-- Star-Lord

spartoiCunning :: CardDef
spartoiCunning = treachery "17027" "Spartoi Cunning" [Boost] StarLordNemesis 3

-- Gamora

waylay :: CardDef
waylay = treachery "18028" "Waylay" [Boost, Boost] GamoraNemesis 2

-- Groot

fanTheFlames :: CardDef
fanTheFlames = treachery "16028" "Fan the Flames" [Boost] GrootNemesis 3

-- Rocket Raccoon

planetaryInvasion :: CardDef
planetaryInvasion = treachery "16057" "Planetary Invasion" [Boost] RocketRaccoonNemesis 2

-- The Galaxy's Most Wanted: Ship Command

peril :: CardDef -> CardDef
peril def = def {cdKeywords = singleton Keyword.Peril}

blindSide :: CardDef
blindSide = peril $ treachery "16145" "Blind Side" [Boost] ShipCommand 1

hullBreach :: CardDef
hullBreach = peril $ treachery "16146" "Hull Breach" [Boost, Boost] ShipCommand 1

powerSiphon :: CardDef
powerSiphon = peril $ treachery "16147" "Power Siphon" [Boost] ShipCommand 1

specialDelivery :: CardDef
specialDelivery = treachery "16148" "Special Delivery" [] ShipCommand 1

-- Badoon Headhunter

onTheHunt :: CardDef
onTheHunt = (treachery "16184" "On the Hunt" [Boost, Star] BadoonHeadhunter 1) {cdKeywords = singleton Keyword.Surge}

deadToRights :: CardDef
deadToRights = (treachery "16185" "Dead to Rights" [Boost, Star] BadoonHeadhunter 1) {cdKeywords = singleton Keyword.Surge}

-- The Galaxy's Most Wanted: Escape the Museum

iHaveYouNow :: CardDef
iHaveYouNow = treachery "16086" "\"I Have You Now!\"" [Star, Boost] EscapeTheMuseum 3

impossibleGeometry :: CardDef
impossibleGeometry = (treachery "16087" "Impossible Geometry" [Boost] EscapeTheMuseum 3) {cdKeywords = singleton (Keyword.Incite 1)}

-- Ronan the Accuser

singleMindedFury :: CardDef
singleMindedFury = treachery "16114" "Single-Minded Fury" [Star] Ronan 2

kreePhysiology :: CardDef
kreePhysiology = (treachery "16115" "Kree Physiology" [Boost] Ronan 2) {cdKeywords = singleton Keyword.Surge}

youStandAccused :: CardDef
youStandAccused = treachery "16116" "\"You Stand Accused!\"" [Boost, Star] Ronan 3

-- The Galaxy's Most Wanted: Nebula

barrelRoll :: CardDef
barrelRoll = (treachery "16100" "Barrel Roll" [Boost, Star] Nebula 2) {cdKeywords = fromList [Keyword.Incite 1, Keyword.Surge]}

combatReady :: CardDef
combatReady = treachery "16101" "Combat Ready" [Boost, Boost] Nebula 2

ruthless :: CardDef
ruthless = treachery "16102" "Ruthless" [Boost, Boost] Nebula 2

-- Space Pirates

honorAmongThieves :: CardDef
honorAmongThieves = treachery "16141" "Honor Among Thieves" [Boost, Boost] SpacePirates 2

-- The Galaxy's Most Wanted: Infiltrate the Museum

inconspicuousBox :: CardDef
inconspicuousBox = treachery "16076" "Inconspicuous Box" [Star] InfiltrateTheMuseum 2

viewTheCosmos :: CardDef
viewTheCosmos = treachery "16077" "View the Cosmos" [Boost, Boost] InfiltrateTheMuseum 2

stayAwhile :: CardDef
stayAwhile = treachery "16078" "Stay Awhile" [Boost] InfiltrateTheMuseum 2

-- | The core set's Caught Off Guard, again in this set.
caughtOffGuardMuseum :: CardDef
caughtOffGuardMuseum = treachery "16079" "Caught Off Guard" [Boost] InfiltrateTheMuseum 1
-- Drax

-- boost icons from the printed card (MarvelCDB has none for this pack)
iWillDestroyYou :: CardDef
iWillDestroyYou = treachery "19029" "\"I Will Destroy You!\"" [Boost, Boost] DraxNemesis 2

-- Spectrum

radioactiveBlast :: CardDef
radioactiveBlast = treachery "21030" "Radioactive Blast" [Boost] SpectrumNemesis 1

-- Adam Warlock

cosmicInquisition :: CardDef
cosmicInquisition =
  (treachery "21070" "Cosmic Inquisition" [Boost, Boost] AdamWarlockNemesis 1)
    { cdKeywords = singleton (Keyword.Incite 2)
    }

-- The Mad Titan's Shadow: Tower Defense

proximasPower :: CardDef
proximasPower = treachery "21106" "Proxima's Power" [Star] TowerDefense 2

corvussCunning :: CardDef
corvussCunning = treachery "21107" "Corvus's Cunning" [Star] TowerDefense 2

boundByBlood :: CardDef
boundByBlood = treachery "21108" "Bound by Blood" [Star] TowerDefense 1

rainFire :: CardDef
rainFire = treachery "21109" "Rain Fire" [Boost, Star] TowerDefense 2

-- The Mad Titan's Shadow: Ebony Maw

agentOfThanos :: CardDef
agentOfThanos = treachery "21080" "Agent of Thanos" [Boost] EbonyMaw 2

channelingTrance :: CardDef
channelingTrance = treachery "21081" "Channeling Trance" [Boost] EbonyMaw 3

-- Black Order

bloodToSpare :: CardDef
bloodToSpare = treachery "21088" "Blood to Spare" [Boost] BlackOrderSet 1

-- The Mad Titan's Shadow: Thanos

avatarOfDeath :: CardDef
avatarOfDeath = treachery "21120" "Avatar of Death" [Boost] Thanos 2

deviantSyndrome :: CardDef
deviantSyndrome = (treachery "21121" "Deviant Syndrome" [Star] Thanos 2) {cdKeywords = singleton (Keyword.Incite 1)}

iAmInevitable :: CardDef
iAmInevitable = treachery "21122" "\"I Am Inevitable\"" [Star] Thanos 2

theMadTitan :: CardDef
theMadTitan = treachery "21123" "The Mad Titan" [Star] Thanos 2

-- The Mad Titan's Shadow: Loki

deviousSorcery :: CardDef
deviousSorcery = treachery "21174" "Devious Sorcery" [Boost, Boost] Loki 2

infiniteMischief :: CardDef
infiniteMischief = treachery "21175" "Infinite Mischief" [Star] Loki 2

theTrickster :: CardDef
theTrickster = treachery "21176" "The Trickster" [Star] Loki 3

-- The Mad Titan's Shadow: Hela

helasDomain :: CardDef
helasDomain = treachery "21149" "Hela's Domain" [Boost, Star] Hela 2

theQueenOfHel :: CardDef
theQueenOfHel = treachery "21150" "The Queen of Hel" [Boost] Hela 2

theWastesOfNiffleheim :: CardDef
theWastesOfNiffleheim = treachery "21151" "The Wastes of Niffleheim" [Star] Hela 2

-- Legions of Hel

noPlaceForTheLiving :: CardDef
noPlaceForTheLiving = treachery "21154" "No Place for the Living" [Boost] LegionsOfHel 1

-- The Mad Titan's Shadow: the campaign set

summonedBack :: CardDef
summonedBack = (treachery "21188" "Summoned Back" [Boost, Boost, Boost] MadTitansShadowCampaign 1) {cdKeywords = fromList [Keyword.Peril, Keyword.Surge]}

-- Nebula

oldRivals :: CardDef
oldRivals = treachery "22031" "Old Rivals" [Boost] NebulaNemesis 2

-- War Machine

laserStrike :: CardDef
laserStrike = treachery "23031" "Laser Strike" [Boost, Star] WarMachineNemesis 3

-- Vision

relentlessAndroid :: CardDef
relentlessAndroid = treachery "26032" "Relentless Android" [Boost, Boost] VisionNemesis 2


-- The Hood

fieldRecruitment :: CardDef
fieldRecruitment = treachery "24012" "Field Recruitment" [Boost, Star] TheHood 1

upperHand :: CardDef
upperHand = treachery "24013" "Upper Hand" [Boost] TheHood 3


-- Sinister Motives

inColdBlood :: CardDef
inColdBlood = treachery "27029" "In Cold Blood" [Boost] GhostSpiderNemesis 2

sliceAndDice :: CardDef
sliceAndDice = treachery "27060" "Slice and Dice" [Boost] SpiderManMoralesNemesis 2

-- Sinister Motives

smTreacheryDef :: CardCode -> Name -> SMSet.EncounterSet -> Natural -> CardDef
smTreacheryDef code name set quantity = CardDef
  { cdCardCode = code
  , cdName = name
  , cdCost = Nothing
  , cdTraits = mempty
  , cdKeywords = mempty
  , cdCardType = TreacheryType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdEncounterSet = Just set
  , cdEncounterSetQuantity = Just quantity
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = NoCost
  }

smSandslide :: CardDef
smSandslide = (smTreacheryDef "27070" "Sandslide" SMSet.Sandman 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Star]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smSandStorm :: CardDef
smSandStorm = (smTreacheryDef "27071" "Sand Storm" SMSet.Sandman 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Boost, SMBoost.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smSandSmash :: CardDef
smSandSmash = (smTreacheryDef "27072" "Sand Smash" SMSet.Sandman 2)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smBitingRetort :: CardDef
smBitingRetort = (smTreacheryDef "27082" "Biting Retort" SMSet.Venom 2)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Boost, SMBoost.Star]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smForWhomTheBellTolls :: CardDef
smForWhomTheBellTolls = (smTreacheryDef "27083" "For Whom the Bell Tolls" SMSet.Venom 2)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Star]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smDejaVu :: CardDef
smDejaVu = (smTreacheryDef "27092" "Déjà Vu" SMSet.Mysterio 2)
  { cdTraits = fromList []
  , cdKeywords = fromList [SMKeyword.Peril]
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Star]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smFearmonger :: CardDef
smFearmonger = (smTreacheryDef "27093" "Fearmonger" SMSet.Mysterio 2)
  { cdTraits = fromList []
  , cdKeywords = fromList [SMKeyword.Surge]
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Star]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smFrequentFlyers :: CardDef
smFrequentFlyers = (smTreacheryDef "27108" "Frequent Flyers" SMSet.SinisterSix 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Boost, SMBoost.Boost, SMBoost.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smHighFashion :: CardDef
smHighFashion = (smTreacheryDef "27109" "High Fashion" SMSet.SinisterSix 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Boost, SMBoost.Boost, SMBoost.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smRoboticEnhancements :: CardDef
smRoboticEnhancements = (smTreacheryDef "27110" "Robotic Enhancements" SMSet.SinisterSix 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Boost, SMBoost.Boost, SMBoost.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smPartnershipOfPain :: CardDef
smPartnershipOfPain = (smTreacheryDef "27111" "Partnership of Pain" SMSet.SinisterSix 3)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Boost, SMBoost.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smSurprise :: CardDef
smSurprise = (smTreacheryDef "27112" "Surprise!" SMSet.SinisterSix 3)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smSpreadingPanic :: CardDef
smSpreadingPanic = (smTreacheryDef "27126" "Spreading Panic" SMSet.VenomGoblin 2)
  { cdTraits = fromList []
  , cdKeywords = fromList [SMKeyword.Surge]
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Star]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smCallingInFavors :: CardDef
smCallingInFavors = (smTreacheryDef "27129" "Calling in Favors" SMSet.CityInChaos 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Boost, SMBoost.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smNowOrNever :: CardDef
smNowOrNever = (smTreacheryDef "27130" "Now or Never" SMSet.CityInChaos 2)
  { cdTraits = fromList []
  , cdKeywords = fromList [SMKeyword.Peril]
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smThreatOrMenace :: CardDef
smThreatOrMenace = (smTreacheryDef "27134" "\"Threat or Menace?\"" SMSet.DownToEarth 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smLooseEnds :: CardDef
smLooseEnds = (smTreacheryDef "27135" "Loose Ends" SMSet.DownToEarth 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smRemoteNavigation :: CardDef
smRemoteNavigation = (smTreacheryDef "27141" "Remote Navigation" SMSet.GoblinGear 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Star]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smFromEveryDirection :: CardDef
smFromEveryDirection = (smTreacheryDef "27146" "From Every Direction" SMSet.GuerrillaTactics 2)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smDeepestFears :: CardDef
smDeepestFears = (smTreacheryDef "27157" "Deepest Fears" SMSet.PersonalNightmare 2)
  { cdTraits = fromList []
  , cdKeywords = fromList [SMKeyword.Peril]
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smSwingingAssault :: CardDef
smSwingingAssault = (smTreacheryDef "27168" "Swinging Assault" SMSet.SymbioticStrength 2)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Boost, SMBoost.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smUnstableSentience :: CardDef
smUnstableSentience = (smTreacheryDef "27169" "Unstable Sentience" SMSet.SymbioticStrength 1)
  { cdTraits = fromList []
  , cdKeywords = fromList [SMKeyword.Surge]
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Star]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smSmearCampaign :: CardDef
smSmearCampaign = (smTreacheryDef "27175" "Smear Campaign" SMSet.BadPublicity 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Boost, SMBoost.Boost, SMBoost.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

-- Nova

warDelivery :: CardDef
warDelivery = treachery "28024" "War Delivery" [Boost, Star] SMSet.NovaNemesis 2

theWarsBeenBrought :: CardDef
theWarsBeenBrought = (treachery "28025" "\"The War's Been Brought\"" [] SMSet.NovaNemesis 1)
  { cdKeywords = fromList [Keyword.Surge]
  }

toughAndTumble :: CardDef
toughAndTumble = treachery "28031" "Tough and Tumble" [Boost] SMSet.Armadillo 1

toughItOut :: CardDef
toughItOut = treachery "28032" "Tough It Out" [] SMSet.Armadillo 2


-- Ironheart

politicalRetribution :: CardDef
politicalRetribution = (treachery "29032" "Political Retribution" [Boost] SMSet.IronheartNemesis 2)
  { cdUnique = False
  }

zzzap :: CardDef
zzzap = (treachery "29040" "Zzzap!" [Boost] SMSet.ZzzaxSet 2)
  { cdUnique = False
  }


-- Spider-Ham

feastOnThis :: CardDef
feastOnThis = (treachery "30028" "\"Feast on This!\"" [Boost] SMSet.SpiderHamNemesis 2)
  { cdUnique = False
  }


-- SP//dr

energyDrain :: CardDef
energyDrain = (treachery "31028" "Energy Drain" [Boost] SpdrSet.SpdrNemesis 3)
  { cdUnique = False
  }

-- Mutant Genesis

slammed :: CardDef
slammed = (treachery "32029" "Slammed" [Boost, Boost, Star] ColossusNemesis 1)

-- Mutant Genesis scenario definitions
mgSabretoothStrikes :: CardDef
mgSabretoothStrikes = (smTreacheryDef "32069" ("Sabretooth Strikes") MGSet.Sabretooth 2)
  { cdTraits = fromList []
  , cdUnique = False
  , cdBoostIcons = [MGBoost.Star]
  , cdKeywords = fromList []
  }

mgUnrelentingSavage :: CardDef
mgUnrelentingSavage = (smTreacheryDef "32070" ("Unrelenting Savage") MGSet.Sabretooth 3)
  { cdTraits = fromList []
  , cdUnique = False
  , cdBoostIcons = [MGBoost.Boost]
  , cdKeywords = fromList []
  }

mgMutantTerrorists :: CardDef
mgMutantTerrorists = (smTreacheryDef "32078" ("Mutant Terrorists") MGSet.Brotherhood 1)
  { cdTraits = fromList []
  , cdUnique = False
  , cdBoostIcons = [MGBoost.Boost, MGBoost.Boost]
  , cdKeywords = fromList []
  }

mgInfiltration :: CardDef
mgInfiltration = (smTreacheryDef "32082" ("Infiltration") MGSet.Mystique 2)
  { cdTraits = fromList [MGTrait.Shapeshifter]
  , cdUnique = False
  , cdBoostIcons = [MGBoost.Boost]
  , cdKeywords = fromList []
  }

mgShapeshifterSurprise :: CardDef
mgShapeshifterSurprise = (smTreacheryDef "32083" ("Shapeshifter Surprise") MGSet.Mystique 1)
  { cdTraits = fromList [MGTrait.Shapeshifter]
  , cdUnique = False
  , cdBoostIcons = [MGBoost.Boost, MGBoost.Boost]
  , cdKeywords = fromList []
  }

mgSelfRepair :: CardDef
mgSelfRepair = (smTreacheryDef "32097" ("Self-Repair") MGSet.ProjectWideawake 1)
  { cdTraits = fromList []
  , cdUnique = False
  , cdBoostIcons = [MGBoost.Star]
  , cdKeywords = fromList []
  }

mgMutantDetected :: CardDef
mgMutantDetected = (smTreacheryDef "32098" ("Mutant Detected") MGSet.ProjectWideawake 2)
  { cdTraits = fromList []
  , cdUnique = False
  , cdBoostIcons = [MGBoost.Boost, MGBoost.Boost]
  , cdKeywords = fromList []
  }

mgMasterMoldsChildren :: CardDef
mgMasterMoldsChildren = (smTreacheryDef "32117" ("Master Mold's Children") MGSet.MasterMold 2)
  { cdTraits = fromList []
  , cdUnique = False
  , cdBoostIcons = [MGBoost.Boost]
  , cdKeywords = fromList []
  }

mgShieldsUp :: CardDef
mgShieldsUp = (smTreacheryDef "32118" ("Shields Up") MGSet.MasterMold 2)
  { cdTraits = fromList []
  , cdUnique = False
  , cdBoostIcons = [MGBoost.Star]
  , cdKeywords = fromList []
  }

mgBrotherhoodBeatdown :: CardDef
mgBrotherhoodBeatdown = (smTreacheryDef "32131" ("Brotherhood Beatdown") MGSet.MansionAttack 3)
  { cdTraits = fromList []
  , cdUnique = False
  , cdBoostIcons = [MGBoost.Boost, MGBoost.Boost]
  , cdKeywords = fromList []
  }

mgGroundSwell :: CardDef
mgGroundSwell = (smTreacheryDef "32132" ("Ground Swell") MGSet.MansionAttack 2)
  { cdTraits = fromList []
  , cdUnique = False
  , cdBoostIcons = [MGBoost.Boost, MGBoost.Star]
  , cdKeywords = fromList []
  }

mgImmovable :: CardDef
mgImmovable = (smTreacheryDef "32133" ("Immovable") MGSet.MansionAttack 2)
  { cdTraits = fromList []
  , cdUnique = False
  , cdBoostIcons = [MGBoost.Boost, MGBoost.Star]
  , cdKeywords = fromList []
  }

mgPyromaniac :: CardDef
mgPyromaniac = (smTreacheryDef "32134" ("Pyromaniac") MGSet.MansionAttack 2)
  { cdTraits = fromList []
  , cdUnique = False
  , cdBoostIcons = [MGBoost.Boost, MGBoost.Star]
  , cdKeywords = fromList []
  }

mgHoppingMad :: CardDef
mgHoppingMad = (smTreacheryDef "32135" ("Hopping Mad") MGSet.MansionAttack 2)
  { cdTraits = fromList []
  , cdUnique = False
  , cdBoostIcons = [MGBoost.Boost, MGBoost.Star]
  , cdKeywords = fromList []
  }

mgMasterOfMagnetism :: CardDef
mgMasterOfMagnetism = (smTreacheryDef "32151" ("Master of Magnetism") MGSet.Magneto 3)
  { cdTraits = fromList []
  , cdUnique = False
  , cdBoostIcons = [MGBoost.Boost, MGBoost.Boost]
  , cdKeywords = fromList []
  }

mgElectricShock :: CardDef
mgElectricShock = (smTreacheryDef "32152" ("Electric Shock") MGSet.Magneto 2)
  { cdTraits = fromList [MGTrait.Magnetic]
  , cdUnique = False
  , cdBoostIcons = [MGBoost.Boost, MGBoost.Boost]
  , cdKeywords = fromList []
  }

mgElectromagneticBlast :: CardDef
mgElectromagneticBlast = (smTreacheryDef "32153" ("Electromagnetic Blast") MGSet.Magneto 2)
  { cdTraits = fromList [MGTrait.Magnetic]
  , cdUnique = False
  , cdBoostIcons = [MGBoost.Star]
  , cdKeywords = fromList []
  }

mgMetalShards :: CardDef
mgMetalShards = (smTreacheryDef "32154" ("Metal Shards") MGSet.Magneto 2)
  { cdTraits = fromList [MGTrait.Magnetic]
  , cdUnique = False
  , cdBoostIcons = [MGBoost.Star]
  , cdKeywords = fromList []
  }

mgMagneticMissile :: CardDef
mgMagneticMissile = (smTreacheryDef "32155" ("Magnetic Missile") MGSet.Magneto 2)
  { cdTraits = fromList [MGTrait.Magnetic]
  , cdUnique = False
  , cdBoostIcons = [MGBoost.Boost]
  , cdKeywords = fromList [MGKeyword.Surge]
  }

mgZealForTheCause :: CardDef
mgZealForTheCause = (smTreacheryDef "32164" ("Zeal for the Cause") MGSet.Acolytes 1)
  { cdTraits = fromList []
  , cdUnique = False
  , cdBoostIcons = [MGBoost.Boost]
  , cdKeywords = fromList []
  }

-- Wolverine

tentacleStrike :: CardDef
tentacleStrike = treachery "35031" "Tentacle Strike" [Star] WolverineNemesis 1

hackNSlash :: CardDef
hackNSlash = treachery "35037" "Hack 'n' Slash" [Star] LadyDeathstrikeSet 2


-- Phoenix

fieryRage :: CardDef
fieryRage = (treachery "34031" "Fiery Rage" [Boost, Boost] PhoenixNemesis 3) {cdKeywords = fromList [Peril]}


-- Cyclops

concussiveForce :: CardDef
concussiveForce = (treachery "33031" "Concussive Force" [Boost, Boost] CyclopsNemesis 1)


-- Storm

knifeFight :: CardDef
knifeFight = treachery "36034" "Knife Fight" [Boost, Boost] StormNemesis 2

astralAttack :: CardDef
astralAttack = treachery "36039" "Astral Attack" [Star] ShadowKingSet 1

-- Gambit

assassinationAttempt :: CardDef
assassinationAttempt = treachery "37029" "Assassination Attempt" [Boost] GambitNemesisSet 1

acolyteFrenzy :: CardDef
acolyteFrenzy = treachery "37035" "Acolyte Frenzy" [Star] ExodusSet 2


-- Rogue

misled :: CardDef
misled = (treachery "38027" "Misled" [Boost] RogueNemesisSet 3) { cdTraits = fromList [MGTrait.Shapeshifter] }


-- Mojo Mania

mojoCardDef :: CardCode -> Name -> MojoSet.EncounterSet -> Natural -> CardDef
mojoCardDef code name set quantity = CardDef
  { cdCardCode = code
  , cdName = name
  , cdCost = Nothing
  , cdTraits = mempty
  , cdKeywords = mempty
  , cdCardType = TreacheryType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdEncounterSet = Just set
  , cdEncounterSetQuantity = Just quantity
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = NoCost
  }

mojo39009 :: CardDef
mojo39009 = (mojoCardDef "39009" "Break a Leg" MojoSet.Magog 1)
  { cdTraits = fromList []
  , cdUnique = False
  , cdKeywords = fromList []
  , cdBoostIcons = [MojoBoost.Boost, MojoBoost.Boost]
  }

mojo39010 :: CardDef
mojo39010 = (mojoCardDef "39010" "Defend the Title" MojoSet.Magog 2)
  { cdTraits = fromList []
  , cdUnique = False
  , cdKeywords = fromList []
  , cdBoostIcons = [MojoBoost.Boost]
  }

mojo39011 :: CardDef
mojo39011 = (mojoCardDef "39011" "Stage Fright" MojoSet.Magog 1)
  { cdTraits = fromList []
  , cdUnique = False
  , cdKeywords = fromList []
  , cdBoostIcons = [MojoBoost.Star]
  }

mojo39017 :: CardDef
mojo39017 = (mojoCardDef "39017" "Cornered!" MojoSet.Spiral 0)
  { cdTraits = fromList []
  , cdUnique = False
  , cdKeywords = fromList []
  , cdBoostIcons = []
  }

mojo39019 :: CardDef
mojo39019 = (mojoCardDef "39019" "Erratic Teleportation" MojoSet.Spiral 2)
  { cdTraits = fromList []
  , cdUnique = False
  , cdKeywords = fromList [MojoKeyword.Surge]
  , cdBoostIcons = [MojoBoost.Boost]
  }

mojo39020 :: CardDef
mojo39020 = (mojoCardDef "39020" "The Show Must Go On" MojoSet.Spiral 1)
  { cdTraits = fromList []
  , cdUnique = False
  , cdKeywords = fromList []
  , cdBoostIcons = [MojoBoost.Boost, MojoBoost.Boost, MojoBoost.Boost]
  }

mojo39021 :: CardDef
mojo39021 = (mojoCardDef "39021" "Well-Armed" MojoSet.Spiral 1)
  { cdTraits = fromList []
  , cdUnique = False
  , cdKeywords = fromList []
  , cdBoostIcons = [MojoBoost.Boost]
  }

mojo39032 :: CardDef
mojo39032 = (mojoCardDef "39032" "Curtain Call" MojoSet.Mojo 2)
  { cdTraits = fromList []
  , cdUnique = False
  , cdKeywords = fromList []
  , cdBoostIcons = [MojoBoost.Star]
  }

mojo39033 :: CardDef
mojo39033 = (mojoCardDef "39033" "Director's Directions" MojoSet.Mojo 2)
  { cdTraits = fromList []
  , cdUnique = False
  , cdKeywords = fromList []
  , cdBoostIcons = [MojoBoost.Boost]
  }

mojo39034 :: CardDef
mojo39034 = (mojoCardDef "39034" "Top Billing" MojoSet.Mojo 1)
  { cdTraits = fromList []
  , cdUnique = False
  , cdKeywords = fromList []
  , cdBoostIcons = [MojoBoost.Star]
  }

mojo39040 :: CardDef
mojo39040 = (mojoCardDef "39040" "Elementary, My Dear Mojo" MojoSet.Crime 1)
  { cdTraits = fromList []
  , cdUnique = False
  , cdKeywords = fromList []
  , cdBoostIcons = []
  }

mojo39046 :: CardDef
mojo39046 = (mojoCardDef "39046" "Mana Drain" MojoSet.Fantasy 1)
  { cdTraits = fromList []
  , cdUnique = False
  , cdKeywords = fromList []
  , cdBoostIcons = [MojoBoost.Star]
  }

mojo39070 :: CardDef
mojo39070 = (mojoCardDef "39070" "A Game of Cards" MojoSet.Western 1)
  { cdTraits = fromList []
  , cdUnique = False
  , cdKeywords = fromList []
  , cdBoostIcons = [MojoBoost.Boost]
  }

-- NeXt Evolution: Cable

cable40035 :: CardDef
cable40035 = (treachery "40035" "Mind Scan" [NE.Star] CableNemesis 1) { cdUnique = False, cdDeckLimit = Just 1 }

cable40036 :: CardDef
cable40036 = (treachery "40036" "Telekinetic Blast" [NE.Star] CableNemesis 1) { cdUnique = False, cdDeckLimit = Just 1 }

-- NeXt Evolution campaign and scenarios

ne40080 :: CardDef
ne40080 = CardDef
  { cdCardCode = "40080"
  , cdName = "Hide!"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList [NEKeyword.Surge]
  , cdCardType = TreacheryType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Star]
  , cdEncounterSet = Just NESet.MorlockSiege
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

ne40088 :: CardDef
ne40088 = CardDef
  { cdCardCode = "40088"
  , cdName = "Back in Action"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = TreacheryType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Star]
  , cdEncounterSet = Just NESet.MorlockSiege
  , cdEncounterSetQuantity = Just 2
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

ne40089 :: CardDef
ne40089 = CardDef
  { cdCardCode = "40089"
  , cdName = "Seek the Weak"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = TreacheryType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Star]
  , cdEncounterSet = Just NESet.MorlockSiege
  , cdEncounterSetQuantity = Just 3
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

ne40102 :: CardDef
ne40102 = CardDef
  { cdCardCode = "40102"
  , cdName = "Bound by Business"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = TreacheryType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost]
  , cdEncounterSet = Just NESet.MutantSlayers
  , cdEncounterSetQuantity = Just 2
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

ne40110 :: CardDef
ne40110 = CardDef
  { cdCardCode = "40110"
  , cdName = "Dizzying Deeds"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = TreacheryType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.OnTheRun
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

ne40111 :: CardDef
ne40111 = CardDef
  { cdCardCode = "40111"
  , cdName = "Tag Team"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = TreacheryType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.OnTheRun
  , cdEncounterSetQuantity = Just 2
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

ne40125 :: CardDef
ne40125 = CardDef
  { cdCardCode = "40125"
  , cdName = "Breakthrough"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = TreacheryType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.NeJuggernaut
  , cdEncounterSetQuantity = Just 2
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

ne40126 :: CardDef
ne40126 = CardDef
  { cdCardCode = "40126"
  , cdName = "Flatten"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = TreacheryType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Star]
  , cdEncounterSet = Just NESet.NeJuggernaut
  , cdEncounterSetQuantity = Just 2
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

ne40127 :: CardDef
ne40127 = CardDef
  { cdCardCode = "40127"
  , cdName = "Ground Pound"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = TreacheryType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Star]
  , cdEncounterSet = Just NESet.NeJuggernaut
  , cdEncounterSetQuantity = Just 2
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

ne40128 :: CardDef
ne40128 = CardDef
  { cdCardCode = "40128"
  , cdName = "Trample"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = TreacheryType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Star]
  , cdEncounterSet = Just NESet.NeJuggernaut
  , cdEncounterSetQuantity = Just 2
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

ne40129 :: CardDef
ne40129 = CardDef
  { cdCardCode = "40129"
  , cdName = "Cyttorak's Exemplar"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = TreacheryType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.NeJuggernaut
  , cdEncounterSetQuantity = Just 3
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

ne40135 :: CardDef
ne40135 = CardDef
  { cdCardCode = "40135"
  , cdName = "A Sound Thrashing"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = TreacheryType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.BlackTomCassidy
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

ne40147 :: CardDef
ne40147 = CardDef
  { cdCardCode = "40147"
  , cdName = "Genetic Mastery"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = TreacheryType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.MisterSinister
  , cdEncounterSetQuantity = Just 2
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

ne40148 :: CardDef
ne40148 = CardDef
  { cdCardCode = "40148"
  , cdName = "Molecular Control"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = TreacheryType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Star]
  , cdEncounterSet = Just NESet.MisterSinister
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

ne40149 :: CardDef
ne40149 = CardDef
  { cdCardCode = "40149"
  , cdName = "Sinister Schemes"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = TreacheryType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Star]
  , cdEncounterSet = Just NESet.MisterSinister
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

ne40150 :: CardDef
ne40150 = CardDef
  { cdCardCode = "40150"
  , cdName = "Sinister Strike"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = TreacheryType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Star]
  , cdEncounterSet = Just NESet.MisterSinister
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

ne40158 :: CardDef
ne40158 = CardDef
  { cdCardCode = "40158"
  , cdName = "\"I'll Take That\""
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = TreacheryType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.SuperStrengthSet
  , cdEncounterSetQuantity = Just 2
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

ne40162 :: CardDef
ne40162 = CardDef
  { cdCardCode = "40162"
  , cdName = "One Step Ahead"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = TreacheryType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Star]
  , cdEncounterSet = Just NESet.TelepathySet
  , cdEncounterSetQuantity = Just 2
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

ne40177 :: CardDef
ne40177 = CardDef
  { cdCardCode = "40177"
  , cdName = "Psionic Surge"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = TreacheryType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.NeStryfe
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

ne40178 :: CardDef
ne40178 = CardDef
  { cdCardCode = "40178"
  , cdName = "Psychic Override"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Psionic]
  , cdKeywords = fromList []
  , cdCardType = TreacheryType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Star]
  , cdEncounterSet = Just NESet.NeStryfe
  , cdEncounterSetQuantity = Just 2
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

ne40179 :: CardDef
ne40179 = CardDef
  { cdCardCode = "40179"
  , cdName = "Telekinetic Wave"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Psionic]
  , cdKeywords = fromList []
  , cdCardType = TreacheryType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Star]
  , cdEncounterSet = Just NESet.NeStryfe
  , cdEncounterSetQuantity = Just 3
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

ne40203 :: CardDef
ne40203 = CardDef
  { cdCardCode = "40203"
  , cdName = "Overburdened"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList [NEKeyword.Surge]
  , cdCardType = TreacheryType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Star]
  , cdEncounterSet = Just NESet.NextEvolutionCampaign
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

ne40154 :: CardDef
ne40154 = CardDef
  { cdCardCode = "40154"
  , cdName = "High Ground"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = TreacheryType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Star]
  , cdEncounterSet = Just NESet.FlightSet
  , cdEncounterSetQuantity = Just 2
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

hood24017 :: CardDef
hood24017 = (hoodCardDef "24017" "Double Trouble" HoodSet.BeastyBoys TreacheryType 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdBoostIcons = [HoodBoost.Star]
  , cdUnique = False
  , cdAcceleration = 0
  , cdHazards = 0
  }

hood24028 :: CardDef
hood24028 = (hoodCardDef "24028" "Caught in the Crossfire" HoodSet.CrossfiresCrew TreacheryType 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdBoostIcons = [HoodBoost.Boost]
  , cdUnique = False
  , cdAcceleration = 0
  , cdHazards = 0
  }

hood24029 :: CardDef
hood24029 = (hoodCardDef "24029" "Cruel Intentions" HoodSet.ExpertII TreacheryType 1)
  { cdTraits = fromList []
  , cdKeywords = fromList [HoodKeyword.Surge, HoodKeyword.Peril]
  , cdBoostIcons = [HoodBoost.Boost, HoodBoost.Star]
  , cdUnique = False
  , cdAcceleration = 0
  , cdHazards = 0
  }

hood24030 :: CardDef
hood24030 = (hoodCardDef "24030" "Ruination" HoodSet.ExpertII TreacheryType 1)
  { cdTraits = fromList []
  , cdKeywords = fromList [HoodKeyword.Peril, HoodKeyword.Incite 1]
  , cdBoostIcons = [HoodBoost.Boost, HoodBoost.Boost, HoodBoost.Boost, HoodBoost.Boost]
  , cdUnique = False
  , cdAcceleration = 0
  , cdHazards = 0
  }

hood24031 :: CardDef
hood24031 = (hoodCardDef "24031" "Seek and Destroy" HoodSet.ExpertII TreacheryType 1)
  { cdTraits = fromList []
  , cdKeywords = fromList [HoodKeyword.Peril, HoodKeyword.Incite 1]
  , cdBoostIcons = [HoodBoost.Boost, HoodBoost.Boost, HoodBoost.Boost]
  , cdUnique = False
  , cdAcceleration = 0
  , cdHazards = 0
  }

hood24032 :: CardDef
hood24032 = (hoodCardDef "24032" "Slug It Out" HoodSet.ExpertII TreacheryType 1)
  { cdTraits = fromList []
  , cdKeywords = fromList [HoodKeyword.Surge, HoodKeyword.Peril]
  , cdBoostIcons = [HoodBoost.Star]
  , cdUnique = False
  , cdAcceleration = 0
  , cdHazards = 0
  }

hood24036 :: CardDef
hood24036 = (hoodCardDef "24036" "Hyde Formula" HoodSet.MisterHydeSet TreacheryType 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdBoostIcons = [HoodBoost.Boost]
  , cdUnique = False
  , cdAcceleration = 0
  , cdHazards = 0
  }

hood24048 :: CardDef
hood24048 = (hoodCardDef "24048" "Sinister Onslaught" HoodSet.SinisterSyndicate TreacheryType 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdBoostIcons = []
  , cdUnique = False
  , cdAcceleration = 0
  , cdHazards = 0
  }

hood24050 :: CardDef
hood24050 = (hoodCardDef "24050" "Dark Dealings" HoodSet.StandardII TreacheryType 2)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdBoostIcons = [HoodBoost.Boost, HoodBoost.Star]
  , cdUnique = False
  , cdAcceleration = 0
  , cdHazards = 0
  }

hood24051 :: CardDef
hood24051 = (hoodCardDef "24051" "Mob Mentality" HoodSet.StandardII TreacheryType 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdBoostIcons = [HoodBoost.Boost, HoodBoost.Boost]
  , cdUnique = False
  , cdAcceleration = 0
  , cdHazards = 0
  }

hood24052 :: CardDef
hood24052 = (hoodCardDef "24052" "Overwhelming Force" HoodSet.StandardII TreacheryType 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdBoostIcons = [HoodBoost.Boost, HoodBoost.Boost, HoodBoost.Star]
  , cdUnique = False
  , cdAcceleration = 0
  , cdHazards = 0
  }

hood24053 :: CardDef
hood24053 = (hoodCardDef "24053" "Shadow of the Past" HoodSet.StandardII TreacheryType 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdBoostIcons = [HoodBoost.Boost, HoodBoost.Boost]
  , cdUnique = False
  , cdAcceleration = 0
  , cdHazards = 0
  }

hood24054 :: CardDef
hood24054 = (hoodCardDef "24054" "Total Annihilation" HoodSet.StandardII TreacheryType 2)
  { cdTraits = fromList []
  , cdKeywords = fromList [HoodKeyword.Surge]
  , cdBoostIcons = [HoodBoost.Boost, HoodBoost.Star]
  , cdUnique = False
  , cdAcceleration = 0
  , cdHazards = 0
  }

hood24059 :: CardDef
hood24059 = (hoodCardDef "24059" "Citywide Crisis" HoodSet.StateOfEmergency TreacheryType 2)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdBoostIcons = [HoodBoost.Star]
  , cdUnique = False
  , cdAcceleration = 0
  , cdHazards = 0
  }

hood24069 :: CardDef
hood24069 = (hoodCardDef "24069" "Combined Effort" HoodSet.WreckingCrewModular TreacheryType 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdBoostIcons = [HoodBoost.Boost, HoodBoost.Star]
  , cdUnique = False
  , cdAcceleration = 0
  , cdHazards = 0
  }

hood24070 :: CardDef
hood24070 = (hoodCardDef "24070" "Magic Muscle" HoodSet.WreckingCrewModular TreacheryType 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdBoostIcons = [HoodBoost.Boost, HoodBoost.Boost]
  , cdUnique = False
  , cdAcceleration = 0
  , cdHazards = 0
  }

-- Psylocke

telekineticDragon :: CardDef
telekineticDragon = CardDef
  { cdCardCode = "41029", cdName = "Telekinetic Dragon", cdCost = Nothing
  , cdTraits = fromList [], cdKeywords = fromList []
  , cdCardType = PsyCard.TreacheryType, cdAbilityType = Nothing, cdAbilitySubTypes = []
  , cdUnique = False, cdAspect = Nothing, cdCriteria = PsyCriteria.NoCriteria
  , cdResources = [], cdBoostIcons = [PsyBoost.Star]
  , cdEncounterSet = Just PsySet.PsylockeNemesis, cdEncounterSetQuantity = Just 2
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 3, cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False, cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False, cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = PsyCost.NoCost
  }

-- Angel

spearShot :: CardDef
spearShot = CardDef
  { cdCardCode = "42028"
  , cdName = "Spear Shot"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = AnCard.TreacheryType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = AnCriteria.NoCriteria
  , cdResources = []
  , cdBoostIcons = [AnBoost.Star]
  , cdEncounterSet = Just AnSet.AngelNemesis
  , cdEncounterSetQuantity = Just 2
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = AnCost.NoCost
  }

-- X-23

hackNSlashX23 :: CardDef
hackNSlashX23 = CardDef
  { cdCardCode = "43033"
  , cdName = "Hack 'n' Slash"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = AnCard.TreacheryType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = AnCriteria.NoCriteria
  , cdResources = []
  , cdBoostIcons = [AnBoost.Star]
  , cdEncounterSet = Just AnSet.X23Nemesis
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = AnCost.NoCost
  }

-- Deadpool

crisisOfInfiniteDeadpools :: CardDef
crisisOfInfiniteDeadpools = DpCard.CardDef
  { cdCardCode = "44037"
  , cdName = DpName.Name "Crisis of Infinite Deadpools" (Nothing)
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = DpCard.TreacheryType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = DpCriteria.NoCriteria
  , cdResources = []
  , cdBoostIcons = [DpBoost.Boost, DpBoost.Boost]
  , cdEncounterSet = Just DpSet.DreadpoolSet
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = DpCost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

metacidalTendencies :: CardDef
metacidalTendencies = DpCard.CardDef
  { cdCardCode = "44042"
  , cdName = DpName.Name "Metacidal Tendencies" (Nothing)
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = DpCard.TreacheryType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = DpCriteria.NoCriteria
  , cdResources = []
  , cdBoostIcons = [DpBoost.Boost, DpBoost.Boost]
  , cdEncounterSet = Just DpSet.DreadpoolSet
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = DpCost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

-- Bishop

temporalTrickery :: CardDef
temporalTrickery = CardDef
  { cdCardCode = "45029"
  , cdName = "Temporal Trickery"
  , cdCost = Nothing
  , cdTraits = fromList [BpTrait.Temporal]
  , cdKeywords = fromList []
  , cdCardType = BpCard.TreacheryType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = BpCriteria.NoCriteria
  , cdResources = []
  , cdBoostIcons = [BpBoost.Boost, BpBoost.Boost]
  , cdEncounterSet = Just BpSet.BishopNemesis
  , cdEncounterSetQuantity = Just 2
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = BpCost.NoCost
  }

-- Age of Apocalypse: Magik
battleForLimbo :: CardDef
battleForLimbo = CardDef
  { cdCardCode = "45058"
  , cdName = "Battle for Limbo"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = TreacheryType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [AoaMB.Star]
  , cdEncounterSet = Just AoaMS.MagikNemesis
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = NoCost
  }

-- Age of Apocalypse campaign and scenarios

infinitePrelate :: CardDef
infinitePrelate = CardDef
  { cdCardCode = "45067"
  , cdName = "Infinite Prelate"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = TreacheryType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.AoaUnus
  , cdEncounterSetQuantity = Just 2
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

cullingTheWeak :: CardDef
cullingTheWeak = CardDef
  { cdCardCode = "45070"
  , cdName = "Culling the Weak"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = TreacheryType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Star]
  , cdEncounterSet = Just NESet.AoaInfinites
  , cdEncounterSetQuantity = Just 2
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

warWearyDystopianNightmare :: CardDef
warWearyDystopianNightmare = CardDef
  { cdCardCode = "45073"
  , cdName = "War-Weary"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = TreacheryType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Star]
  , cdEncounterSet = Just NESet.AoaDystopianNightmare
  , cdEncounterSetQuantity = Just 2
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

darkDesigns :: CardDef
darkDesigns = CardDef
  { cdCardCode = "45076"
  , cdName = "Dark Designs"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = TreacheryType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Star]
  , cdEncounterSet = Just NESet.StandardIII
  , cdEncounterSetQuantity = Just 2
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

sinisterStrike :: CardDef
sinisterStrike = CardDef
  { cdCardCode = "45077"
  , cdName = "Sinister Strike"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = TreacheryType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost]
  , cdEncounterSet = Just NESet.StandardIII
  , cdEncounterSetQuantity = Just 2
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

evilAlliance :: CardDef
evilAlliance = CardDef
  { cdCardCode = "45078"
  , cdName = "Evil Alliance"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = TreacheryType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Star]
  , cdEncounterSet = Just NESet.StandardIII
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

nowhereIsSafe :: CardDef
nowhereIsSafe = CardDef
  { cdCardCode = "45079"
  , cdName = "Nowhere is Safe"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = TreacheryType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Star]
  , cdEncounterSet = Just NESet.StandardIII
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

horsemanOfWar :: CardDef
horsemanOfWar = CardDef
  { cdCardCode = "45092"
  , cdName = "Horseman of War"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = TreacheryType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Star]
  , cdEncounterSet = Just NESet.AoaFourHorsemen
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

horsemanOfFamine :: CardDef
horsemanOfFamine = CardDef
  { cdCardCode = "45093"
  , cdName = "Horseman of Famine"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = TreacheryType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Star]
  , cdEncounterSet = Just NESet.AoaFourHorsemen
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

horsemanOfPestilence :: CardDef
horsemanOfPestilence = CardDef
  { cdCardCode = "45094"
  , cdName = "Horseman of Pestilence"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = TreacheryType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Star]
  , cdEncounterSet = Just NESet.AoaFourHorsemen
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

horsemanOfDeath :: CardDef
horsemanOfDeath = CardDef
  { cdCardCode = "45095"
  , cdName = "Horseman of Death"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = TreacheryType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Star]
  , cdEncounterSet = Just NESet.AoaFourHorsemen
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

roughRiders :: CardDef
roughRiders = CardDef
  { cdCardCode = "45096"
  , cdName = "Rough Riders"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = TreacheryType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.AoaFourHorsemen
  , cdEncounterSetQuantity = Just 3
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

wolfAmongSheep :: CardDef
wolfAmongSheep = CardDef
  { cdCardCode = "45110"
  , cdName = "Wolf Among Sheep"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = TreacheryType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Star]
  , cdEncounterSet = Just NESet.AoaApocalypse
  , cdEncounterSetQuantity = Just 3
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

evilGenius :: CardDef
evilGenius = CardDef
  { cdCardCode = "45125"
  , cdName = "Evil Genius"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = TreacheryType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost]
  , cdEncounterSet = Just NESet.AoaDarkBeast
  , cdEncounterSetQuantity = Just 3
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

landOutOfTime :: CardDef
landOutOfTime = CardDef
  { cdCardCode = "45131"
  , cdName = "Land Out of Time"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = TreacheryType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.AoaSavageLand
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

biomorphicBlast :: CardDef
biomorphicBlast = CardDef
  { cdCardCode = "45150"
  , cdName = "Biomorphic Blast"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Superpower]
  , cdKeywords = fromList []
  , cdCardType = TreacheryType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Star]
  , cdEncounterSet = Just NESet.AoaEnSabahNur
  , cdEncounterSetQuantity = Just 2
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

technologicalInterface :: CardDef
technologicalInterface = CardDef
  { cdCardCode = "45151"
  , cdName = "Technological Interface"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Superpower]
  , cdKeywords = fromList []
  , cdCardType = TreacheryType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Star]
  , cdEncounterSet = Just NESet.AoaEnSabahNur
  , cdEncounterSetQuantity = Just 2
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

giantSizedDespot :: CardDef
giantSizedDespot = CardDef
  { cdCardCode = "45152"
  , cdName = "Giant-Sized Despot"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Superpower]
  , cdKeywords = fromList []
  , cdCardType = TreacheryType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Star]
  , cdEncounterSet = Just NESet.AoaEnSabahNur
  , cdEncounterSetQuantity = Just 2
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

celestialTech :: CardDef
celestialTech = CardDef
  { cdCardCode = "45158"
  , cdName = "Celestial Tech"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = TreacheryType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost]
  , cdEncounterSet = Just NESet.AoaCelestialTech
  , cdEncounterSetQuantity = Just 2
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

tyrantWorship :: CardDef
tyrantWorship = CardDef
  { cdCardCode = "45162"
  , cdName = "Tyrant Worship"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = TreacheryType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Star]
  , cdEncounterSet = Just NESet.AoaClanAkkaba
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

worldwideCrisis :: CardDef
worldwideCrisis = CardDef
  { cdCardCode = "45165"
  , cdName = "Worldwide Crisis"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = TreacheryType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Star]
  , cdEncounterSet = Just NESet.AgeOfApocalypse
  , cdEncounterSetQuantity = Just 2
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

-- Magneto

angryAcolyte :: CardDef
angryAcolyte = CardDef
  { cdCardCode = "49032"
  , cdName = "Angry Acolyte"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = MgCard.TreacheryType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = MgCriteria.NoCriteria
  , cdResources = []
  , cdBoostIcons = [MgBoost.Boost, MgBoost.Boost]
  , cdEncounterSet = Just MgSet.MagnetoNemesis
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = MgCost.NoCost
  }

powerAndDecadence :: CardDef
powerAndDecadence = CardDef
  { cdCardCode = "49042"
  , cdName = "Power and Decadence"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = MgCard.TreacheryType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = MgCriteria.NoCriteria
  , cdResources = []
  , cdBoostIcons = [MgBoost.Star]
  , cdEncounterSet = Just MgSet.HellfireSet
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = MgCost.NoCost
  }

-- Iceman
burnIceman :: CardDef
burnIceman = IcemanPack.burnIceman

-- Iceman
theEyeOfSauron :: CardDef
theEyeOfSauron = IcemanPack.theEyeOfSauron

-- Nightcrawler
nc48030 :: CardDef
nc48030 = CardDef
  { cdCardCode = "48030"
  , cdName = "Brimstone Strike"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = TreacheryType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NcCriteria.NoCriteria
  , cdResources = []
  , cdBoostIcons = [NcBoost.Boost]
  , cdEncounterSet = Just NcSet.NightcrawlerNemesis
  , cdEncounterSetQuantity = Just 2
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NcCost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

-- Nightcrawler
nc48038 :: CardDef
nc48038 = CardDef
  { cdCardCode = "48038"
  , cdName = "\"Off with His Head!\""
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = TreacheryType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NcCriteria.NoCriteria
  , cdResources = []
  , cdBoostIcons = [NcBoost.Boost]
  , cdEncounterSet = Just NcSet.CrazyGangSet
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NcCost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

-- Agents of Shield players

aosP50033 :: CardDef
aosP50033 = CardDef
  { cdCardCode = "50033"
  , cdName = "Diabolical Discs"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList [AospKeyword.Surge]
  , cdCardType = AospCard.TreacheryType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = AospCriteria.NoCriteria
  , cdResources = []
  , cdBoostIcons = [AospBoost.Boost, AospBoost.Boost]
  , cdEncounterSet = Just AospSet.MariaHillNemesis
  , cdEncounterSetQuantity = Just 2
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = AospCost.NoCost
  }

aosP50063 :: CardDef
aosP50063 = CardDef
  { cdCardCode = "50063"
  , cdName = "Cold Storage"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = AospCard.TreacheryType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = AospCriteria.NoCriteria
  , cdResources = []
  , cdBoostIcons = [AospBoost.Boost, AospBoost.Star]
  , cdEncounterSet = Just AospSet.NickFuryNemesis
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = AospCost.NoCost
  }

aos50076 :: CardDef
aos50076 = AosEncounter.aos50076

aos50077 :: CardDef
aos50077 = AosEncounter.aos50077

aos50078 :: CardDef
aos50078 = AosEncounter.aos50078

aos50079 :: CardDef
aos50079 = AosEncounter.aos50079

aos50082 :: CardDef
aos50082 = AosEncounter.aos50082

aos50096 :: CardDef
aos50096 = AosEncounter.aos50096

aos50097 :: CardDef
aos50097 = AosEncounter.aos50097

aos50102 :: CardDef
aos50102 = AosEncounter.aos50102

aos50123 :: CardDef
aos50123 = AosEncounter.aos50123

aos50124 :: CardDef
aos50124 = AosEncounter.aos50124

aos50128 :: CardDef
aos50128 = AosEncounter.aos50128

aos50137 :: CardDef
aos50137 = AosEncounter.aos50137

aos50138 :: CardDef
aos50138 = AosEncounter.aos50138

aos50141 :: CardDef
aos50141 = AosEncounter.aos50141

aos50142 :: CardDef
aos50142 = AosEncounter.aos50142

aos50147 :: CardDef
aos50147 = AosEncounter.aos50147

aos50151 :: CardDef
aos50151 = AosEncounter.aos50151

aos50155 :: CardDef
aos50155 = AosEncounter.aos50155

aos50160 :: CardDef
aos50160 = AosEncounter.aos50160

aos50163 :: CardDef
aos50163 = AosEncounter.aos50163

aos50164 :: CardDef
aos50164 = AosEncounter.aos50164

aos50175 :: CardDef
aos50175 = AosEncounter.aos50175

aos50176 :: CardDef
aos50176 = AosEncounter.aos50176

aos50177 :: CardDef
aos50177 = AosEncounter.aos50177

aos50184a :: CardDef
aos50184a = AosEncounter.aos50184a

aos50184b :: CardDef
aos50184b = AosEncounter.aos50184b

aos50184c :: CardDef
aos50184c = AosEncounter.aos50184c

bringTheHammerDown :: CardDef
bringTheHammerDown = treachery "90004" "Bring the Hammer Down" [Star, Boost, Boost, Boost] KreeFanatic 2

youDareOpposeMe :: CardDef
youDareOpposeMe = treachery "90005" "You Dare Oppose Me?" [Star, Boost, Boost] KreeFanatic 2
