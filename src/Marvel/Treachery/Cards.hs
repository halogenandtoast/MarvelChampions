module Marvel.Treachery.Cards where

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
  [ hardToKeepDown
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

  -- The Once and Future Kang
  , kangsChosen
  , ancientGrudge

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
  , cdAbilitySubType = Nothing
  , cdUnique = False
  , cdAspect = Nothing
  , cdEncounterSet = Just encounterSet
  , cdEncounterSetQuantity = Just quantity
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdResponseWindow = Nothing
  , cdBoostIcons = boostIcons
  , cdHazards = 0
  , cdAcceleration = 0
  , cdLimit = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
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

-- The Green Goblin

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
