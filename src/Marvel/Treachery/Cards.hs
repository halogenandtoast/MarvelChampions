module Marvel.Treachery.Cards where

import Marvel.EncounterSet qualified as SpdrSet
import Marvel.Resource
import Marvel.EncounterSet qualified as SMSet
import Marvel.Trait qualified as SMTrait
import Marvel.Keyword qualified as SMKeyword
import Marvel.GameValue.Types qualified as SMValue
import Marvel.Boost qualified as SMBoost

import Marvel.Keyword
import Marvel.Ability.Types qualified as Ability
import Marvel.Cost.Types
import Marvel.Window
import Marvel.Matchers
import Marvel.Trait qualified as MGTrait
import Marvel.EncounterSet qualified as MGSet
import Marvel.Keyword qualified as MGKeyword
import Marvel.GameValue.Types qualified as MGValue
import Marvel.Boost qualified as MGBoost
import Marvel.Prelude

import Marvel.Cost.Types (Cost (NoCost))

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
  (toFst toCardCode)
  [ misled
  , assassinationAttempt, acolyteFrenzy
  , knifeFight, astralAttack
  , tentacleStrike, hackNSlash
  , concussiveForce
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

-- Wolverine

tentacleStrike :: CardDef
tentacleStrike = treachery "35031" "Tentacle Strike" [Star] WolverineNemesis 1

hackNSlash :: CardDef
hackNSlash = treachery "35037" "Hack 'n' Slash" [Star] LadyDeathstrikeSet 2

-- Gambit

assassinationAttempt :: CardDef
assassinationAttempt = treachery "37029" "Assassination Attempt" [Boost] GambitNemesisSet 1

acolyteFrenzy :: CardDef
acolyteFrenzy = treachery "37035" "Acolyte Frenzy" [Star] ExodusSet 2


-- Rogue

misled :: CardDef
misled = treachery "38027" "Misled" [Shapeshifter] [Boost] RogueNemesisSet 3
