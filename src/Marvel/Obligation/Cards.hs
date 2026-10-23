module Marvel.Obligation.Cards where

import Marvel.Prelude

import Marvel.Cost.Types (Cost (NoCost))

import Marvel.Boost
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Criteria
import Marvel.EncounterSet (EncounterSet (Kang, MasterOfTime))
import Marvel.Name
import Marvel.Trait qualified as Trait

allObligations :: HashMap CardCode CardDef
allObligations = fromList $ map
  (toFst toCardCode)
    [ affairsOfState
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
  , cdAbilitySubType = Nothing
  , cdUnique = False
  , cdAspect = Nothing
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
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
playerDeckObligations = fromList $ map (toFst toCardCode) [zolasAlgorithm, medicalEmergency, martialLaw, antiHeroPropaganda]
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
