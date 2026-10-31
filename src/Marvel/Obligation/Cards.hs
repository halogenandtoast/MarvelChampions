module Marvel.Obligation.Cards where

import Marvel.EncounterSet qualified as MGSet
import Marvel.Keyword qualified as MGKeyword
import Marvel.Trait qualified as MGTrait
import Marvel.Resource qualified as MGResource
import Marvel.Boost qualified as MGBoost
import Marvel.EncounterSet qualified as SMSet
import Marvel.Trait qualified as SMTrait
import Marvel.Keyword qualified as SMKeyword
import Marvel.GameValue.Types qualified as SMValue
import Marvel.Boost qualified as SMBoost

import Marvel.Keyword
import Marvel.EncounterSet
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

import Marvel.Boost
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Criteria
import Marvel.EncounterSet (EncounterSet (Kang, MadTitansShadowCampaign, MasterOfTime))
import Marvel.Name
import Marvel.Trait qualified as Trait

allObligations :: HashMap CardCode CardDef
allObligations = fromList $ map
  (toFst toCardCode)
    [ claustrophobia
  , pastDemons
  , lostVisor
  , mgReactiveDefense
  , mgWarnTheOthers, smFriendsAndFamily
  , affairsOfState
    , legalWork
    , evictionNotice
    , businessProblems
    , familyEmergency
    , manOutOfTime
    , homeByDawn
    , burnNotice
    , odinsAnger
    , physicalToll
    , innerDemons
    -- The Green Goblin

    -- The Wrecking Crew

    -- The Rise of Red Skull
    , criminalPast
    , uncertainLoyalties

    -- The Rise of Red Skull: an Expert Campaign Set
    , zolasAlgorithm
    , medicalEmergency
    , martialLaw
    , antiHeroPropaganda
    -- The Once and Future Kang
    , fearOfKang
    , weakened
    , stolenMemories
    , depowered
    , timeTravelHijinks


    -- Ant-Man
    , careForCassie

    -- Wasp
    , redDreams

    -- Quicksilver
    , needForSpeed

    -- Scarlet Witch
    , slippingSanity

    -- Star-Lord
    , banishment

    -- Gamora
    , unfulfilledDestiny

    -- Groot
    , wilt

    -- Rocket Raccoon
    , crisisOnHalfworld
    -- Drax
    , memoriesOfAnotherLife
    -- Venom
    , struggleForControl
    -- Spectrum
    , lossOfControl
    -- Adam Warlock
    , regenerationCycle
    -- The Mad Titan's Shadow: the campaign set
    , systemShock
    , inferiorityComplex
    , equipmentMalfunction
    , troubleInOtherworld
    , corruptedProgramming
  , worriedFather
  , keepingSecrets
  , weightOfTheWorld
  , aMinorSetback
  , iReallyWantAHotDog
  , inheritedBurden
  , homesick
  , permanentlyPhased
  , burningHunger
    ]

obligation :: CardCode -> Name -> [BoostIcon] -> CardDef
obligation code name boostIcons = CardDef
  { cdCardCode = code
  , cdName = name
  , cdCost = Nothing
  , cdTraits = mempty
  , cdKeywords = mempty
  , cdCardType = ObligationType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
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

affairsOfState :: CardDef
affairsOfState = obligation "01155" "Affairs of State" [Boost, Boost]

legalWork :: CardDef
legalWork = obligation "01160" "Legal Work" [Boost, Boost]

evictionNotice :: CardDef
evictionNotice = obligation "01165" "Eviction Notice" [Boost, Boost]

businessProblems :: CardDef
businessProblems = obligation "01170" "Business Problems" [Boost, Boost]

familyEmergency :: CardDef
familyEmergency = obligation "01175" "Family Emergency" [Boost, Boost]

manOutOfTime :: CardDef
manOutOfTime = obligation "03026" "Man Out of Time" [Boost, Boost]

homeByDawn :: CardDef
homeByDawn = obligation "05025" "Home by Dawn" [Boost, Boost]
innerDemons :: CardDef
innerDemons = obligation "10025" "Inner Demons" [Boost, Boost, Boost]

burnNotice :: CardDef
burnNotice = obligation "08025" "Burn Notice" [Boost, Boost]

odinsAnger :: CardDef
odinsAnger = obligation "06026" "Odin's Anger" [Boost, Boost]

physicalToll :: CardDef
physicalToll = obligation "09027" "Physical Toll" [Boost, Boost]

-- * The Rise of Red Skull: an Expert Campaign Set, obligations with player-card backs
-- that go into a player's deck

zolasAlgorithm :: CardDef
zolasAlgorithm = (obligation "04163" "Zola's Algorithm" []) {cdAcceleration = 1}

medicalEmergency :: CardDef
medicalEmergency = obligation "04164" "Medical Emergency" []

martialLaw :: CardDef
martialLaw = obligation "04165" "Martial Law" []

antiHeroPropaganda :: CardDef
antiHeroPropaganda = obligation "04166" "Anti-Hero Propaganda" []

-- | The obligations that go into a player's deck, not the encounter deck.
playerDeckObligations :: HashMap CardCode CardDef
playerDeckObligations = fromList $ map (toFst toCardCode) [zolasAlgorithm, medicalEmergency, martialLaw, antiHeroPropaganda, systemShock]
criminalPast :: CardDef
criminalPast = obligation "04026" "Criminal Past" [Boost, Boost]

uncertainLoyalties :: CardDef
uncertainLoyalties = obligation "04053" "Uncertain Loyalties" [Boost, Boost]

-- The Once and Future Kang

-- | An obligation of an encounter set: the player who reveals it keeps it.
fearOfKang :: CardDef
fearOfKang =
  (obligation "11049" "Fear of Kang" [Boost, Boost])
    { cdTraits = fromList [Trait.Temporal]
    , cdEncounterSet = Just MasterOfTime
    , cdEncounterSetQuantity = Just 2
    }

-- | The Kang set's obligations, each the revealer's to keep.
kangObligation :: CardCode -> Name -> CardDef
kangObligation code name =
  (obligation code name [Boost, Boost])
    { cdTraits = fromList [Trait.Temporal]
    , cdEncounterSet = Just Kang
    , cdEncounterSetQuantity = Just 2
    }

weakened :: CardDef
weakened = kangObligation "11018" "Weakened"

stolenMemories :: CardDef
stolenMemories = kangObligation "11019" "Stolen Memories"

depowered :: CardDef
depowered = kangObligation "11020" "Depowered"

timeTravelHijinks :: CardDef
timeTravelHijinks = kangObligation "11021" "Time-Travel Hijinks"

-- Ant-Man

careForCassie :: CardDef
careForCassie = obligation "12025" "Care for Cassie" [Boost, Boost]

-- Wasp

redDreams :: CardDef
redDreams = obligation "13026" "Red Dreams" [Boost, Boost]

-- Quicksilver

needForSpeed :: CardDef
needForSpeed = obligation "14024" "Need for Speed" [Boost, Boost]

-- Scarlet Witch

slippingSanity :: CardDef
slippingSanity = obligation "15023" "Slipping Sanity" [Boost, Boost, Boost]

-- Star-Lord

banishment :: CardDef
banishment = obligation "17024" "Banishment" [Boost, Boost]

-- Gamora

unfulfilledDestiny :: CardDef
unfulfilledDestiny = obligation "18024" "Unfulfilled Destiny" [Boost, Boost]

-- Groot

wilt :: CardDef
wilt = obligation "16025" "Wilt" [Boost, Boost]

-- Rocket Raccoon

crisisOnHalfworld :: CardDef
crisisOnHalfworld = obligation "16053" "Crisis on Halfworld" [Boost, Boost]
-- Drax

-- boost icons from the printed card (MarvelCDB has none for this pack)
memoriesOfAnotherLife :: CardDef
memoriesOfAnotherLife = obligation "19025" "Memories of Another Life" [Boost, Boost]

-- Venom

struggleForControl :: CardDef
struggleForControl = obligation "20023" "Struggle for Control" [Boost, Boost]

-- Spectrum

lossOfControl :: CardDef
lossOfControl = obligation "21026" "Loss of Control" [Boost, Boost]

-- Adam Warlock

regenerationCycle :: CardDef
regenerationCycle = obligation "21066" "Regeneration Cycle" [Boost, Boost]

-- * The Mad Titan's Shadow: the campaign set, an obligation with a player-card back the
-- campaign shuffles into a player's deck or puts in their hand

systemShock :: CardDef
systemShock = (obligation "21185" "System Shock" []) {cdEncounterSet = Just MadTitansShadowCampaign, cdEncounterSetQuantity = Just 4}

-- Nebula

inferiorityComplex :: CardDef
inferiorityComplex = obligation "22027" "Inferiority Complex" [Boost, Boost]

-- War Machine

equipmentMalfunction :: CardDef
equipmentMalfunction = obligation "23028" "Equipment Malfunction" [Boost, Boost]

-- Valkyrie

troubleInOtherworld :: CardDef
troubleInOtherworld = obligation "25028" "Trouble in Otherworld" [Boost, Boost]

-- Vision

corruptedProgramming :: CardDef
corruptedProgramming = obligation "26028" "Corrupted Programming" [Boost, Boost]


-- Sinister Motives

worriedFather :: CardDef
worriedFather = obligation "27025" "Worried Father" [Boost, Boost]

keepingSecrets :: CardDef
keepingSecrets = obligation "27056" "Keeping Secrets" [Boost, Boost]

-- Sinister Motives

smObligationDef :: CardCode -> Name -> SMSet.EncounterSet -> Natural -> CardDef
smObligationDef code name set quantity = CardDef
  { cdCardCode = code
  , cdName = name
  , cdCost = Nothing
  , cdTraits = mempty
  , cdKeywords = mempty
  , cdCardType = ObligationType
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

smFriendsAndFamily :: CardDef
smFriendsAndFamily = (smObligationDef "27132" "Friends and Family" SMSet.DownToEarth 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Boost, SMBoost.Boost, SMBoost.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

-- Nova

weightOfTheWorld :: CardDef
weightOfTheWorld = obligation "28021" "Weight of the World" [Boost, Boost]


-- Ironheart

aMinorSetback :: CardDef
aMinorSetback = (obligation "29028" "A Minor Setback" [Boost, Boost])
  { cdUnique = False
  }


-- Spider-Ham

iReallyWantAHotDog :: CardDef
iReallyWantAHotDog = obligation "30024" "\"I Really Want a Hot Dog!\"" [Boost, Boost]


-- SP//dr

inheritedBurden :: CardDef
inheritedBurden = (obligation "31025" "Inherited Burden" [Boost, Boost])
  { cdUnique = False
  }

-- Mutant Genesis

homesick :: CardDef
homesick = (obligation "32025" "Homesick" [Boost, Boost])

-- Mutant Genesis

permanentlyPhased :: CardDef
permanentlyPhased = (obligation "32055" "Permanently Phased" [Boost, Boost])

-- Mutant Genesis scenario definitions
mgWarnTheOthers :: CardDef
mgWarnTheOthers = (smObligationDef "32099" ("Warn the Others") MGSet.ProjectWideawake 2)
  { cdTraits = fromList []
  , cdUnique = False
  , cdBoostIcons = [MGBoost.Boost, MGBoost.Boost]
  , cdKeywords = fromList []
  }


mgReactiveDefense :: CardDef
mgReactiveDefense = (obligation "32174b" "Reactive Defense" [MGBoost.Boost, MGBoost.Boost]) {cdEncounterSet = Just MGSet.MutantGenesisCampaign, cdEncounterSetQuantity = Just 1}


-- Phoenix

burningHunger :: CardDef
burningHunger = obligation "34028" "Burning Hunger" [Boost, Boost]


-- Cyclops

lostVisor :: CardDef
lostVisor = (obligation "33027" "Lost Visor" [Boost, Boost])


-- Storm

claustrophobia :: CardDef
claustrophobia = obligation "36030" "Claustrophobia" [Boost, Boost]

-- Wolverine

pastDemons :: CardDef
pastDemons = obligation "35027" "Past Demons" [Boost, Boost]
