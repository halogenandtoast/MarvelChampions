module Marvel.Obligation.Cards where

import Marvel.CampaignPlayers.EncounterCards qualified as CN
import Marvel.JessicaJones.Cards qualified as JL
import Marvel.AgentsOfShield.EncounterCards qualified as AosEncounter
import Marvel.FalconWinter.Cards qualified as FalconWinter

import Marvel.Ability.Types qualified as AospAbility ()
import Marvel.Aspect qualified as AospAspect ()
import Marvel.Boost qualified as AospBoost
import Marvel.Card.Def qualified as AospCard
import Marvel.Cost.Types qualified as AospCost
import Marvel.Criteria qualified as AospCriteria
import Marvel.EncounterSet qualified as AospSet ()
import Marvel.Keyword qualified as AospKeyword ()
import Marvel.Matchers qualified as AospMatch ()
import Marvel.Name qualified as AospName ()
import Marvel.Resource.Types qualified as AospResource ()
import Marvel.Trait qualified as AospTrait ()
import Marvel.Window.Types qualified as AospWindow ()

import Marvel.BlackPantherShuri.Cards qualified as BlackPantherShuriPack
import Marvel.Silk.Cards qualified as SilkPack
import Marvel.Window.Types qualified as NcWindow ()

import Marvel.Trait.Types qualified as NcTrait ()

import Marvel.Resource.Types qualified as NcResource ()

import Marvel.Name qualified as NcName ()

import Marvel.Matchers qualified as NcMatch ()

import Marvel.Keyword qualified as NcKeyword ()

import Marvel.EncounterSet qualified as NcSet ()

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
import Marvel.EncounterSet qualified as MgSet ()
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

import Marvel.Ability.Types qualified as DpAbility ()
import Marvel.Aspect qualified as DpAspect ()
import Marvel.Boost qualified as DpBoost
import Marvel.Card.Def qualified as DpCard
import Marvel.Cost.Types qualified as DpCost
import Marvel.Criteria qualified as DpCriteria
import Marvel.EncounterSet qualified as DpSet ()
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
import Marvel.EncounterSet qualified as AnSet ()
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
import Marvel.EncounterSet qualified as PsySet ()
import Marvel.Keyword qualified as PsyKeyword ()
import Marvel.Matchers qualified as PsyMatch ()
import Marvel.Name qualified as PsyName ()
import Marvel.Resource.Types qualified as PsyResource ()
import Marvel.Trait qualified as PsyTrait ()
import Marvel.Window.Types qualified as PsyWindow ()



import Marvel.Resource.Types qualified as NEResource ()

import Marvel.Boost qualified as NEBoost

import Marvel.GameValue.Types qualified as NEValue ()

import Marvel.EncounterSet qualified as NESet

import Marvel.Keyword qualified as NEKeyword ()

import Marvel.Trait qualified as NETrait ()

import Marvel.Aspect qualified as NEAspect ()

import Marvel.Boost qualified as NEB
import Marvel.EncounterSet qualified as NES ()
import Marvel.Aspect qualified as NEA ()
import Marvel.Resource.Types qualified as NER ()
import Marvel.Keyword qualified as NEK ()
import Marvel.Trait qualified as NET ()
import Marvel.Boost qualified as NE
import Marvel.Ability.Types qualified as NEAbility ()

import Marvel.Trait qualified as MojoTrait ()
import Marvel.EncounterSet qualified as MojoSet
import Marvel.Keyword qualified as MojoKeyword
import Marvel.GameValue.Types qualified as MojoValue
import Marvel.Boost qualified as MojoBoost

import Marvel.EncounterSet qualified as MGSet
import Marvel.Keyword qualified as MGKeyword ()
import Marvel.Trait qualified as MGTrait ()
import Marvel.Resource qualified as MGResource ()
import Marvel.Boost qualified as MGBoost
import Marvel.EncounterSet qualified as SMSet
import Marvel.Trait qualified as SMTrait ()
import Marvel.Keyword qualified as SMKeyword ()
import Marvel.GameValue.Types qualified as SMValue ()
import Marvel.Boost qualified as SMBoost

import Marvel.Keyword ()
import Marvel.EncounterSet
import Marvel.Cost.Types
import Marvel.Window ()
import Marvel.Matchers ()
import Marvel.GameValue.Types qualified as MGValue ()
import Marvel.WonderMan.Cards qualified as WM

import Marvel.Hercules.Cards qualified as HC

import Marvel.CivilWar.Cards qualified as CW
import Marvel.Prelude


import Marvel.Boost
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Criteria
import Marvel.Name
import Marvel.Trait qualified as Trait

allObligations :: HashMap CardCode CardDef
allObligations = fromList $ map
  (toFst toCardCode)
    [ CW.cw56106,CW.cw56114,CN.enc56023, CN.enc56054, CN.enc60032, CN.enc60060
  , WM.wm58025, HC.hc59004, HC.hc59035
  , JL.jl61030, JL.jl61033a, JL.jl61033b, JL.jl61033c, JL.jl62028
  , aos50134, aos50179
  , FalconWinter.fw53029, FalconWinter.fw54027
  , aosP50029, aosP50059
  , BlackPantherShuriPack.tChallasShadowShuri, SilkPack.silkSenseOverloadSilk
  , nc48026
  , hotHeaded
  , Jubilee.jubilee47023
  , oldGrievances
  , hunted, drawingNear, panickedRefugees
  , darkchilde
  , fearTheFuture
  , theMercWithTheMouth
  , selfIsolation
  , apocalypticInfluence
  , bodySwapped
  , ne40160, ne40161
  , memoriesofArmageddon
  , cable40031
  , mojo39030, mojo39036, mojo39061, mojo39062, mojo39063, mojo39064, mojo39065
  , deadlyTouch
  , guildBusiness
  , claustrophobia
  , lostVisor
  , pastDemons
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
playerDeckObligations = fromList $ map (toFst toCardCode) [zolasAlgorithm, medicalEmergency, martialLaw, antiHeroPropaganda, systemShock, panickedRefugees]
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

-- Wolverine

pastDemons :: CardDef
pastDemons = obligation "35027" "Past Demons" [Boost, Boost]


-- Phoenix

burningHunger :: CardDef
burningHunger = obligation "34028" "Burning Hunger" [Boost, Boost]


-- Cyclops

lostVisor :: CardDef
lostVisor = (obligation "33027" "Lost Visor" [Boost, Boost])


-- Storm

claustrophobia :: CardDef
claustrophobia = obligation "36030" "Claustrophobia" [Boost, Boost]

-- Gambit

guildBusiness :: CardDef
guildBusiness = (obligation "37025" "Guild Business" [Boost, Boost]) { cdHazards = 1 }


-- Rogue

deadlyTouch :: CardDef
deadlyTouch = obligation "38024" "Deadly Touch" [Boost, Boost]


-- Mojo Mania

mojoCardDef :: CardCode -> Name -> MojoSet.EncounterSet -> Natural -> CardDef
mojoCardDef code name set quantity = CardDef
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

mojo39030 :: CardDef
mojo39030 = (mojoCardDef "39030" "Paparazzi" MojoSet.Mojo 1)
  { cdTraits = fromList []
  , cdUnique = False
  , cdKeywords = fromList [MojoKeyword.Hinder (MojoValue.Static 10)]
  , cdBoostIcons = [MojoBoost.Boost, MojoBoost.Boost]
  }

mojo39036 :: CardDef
mojo39036 = (mojoCardDef "39036" "Build the Case" MojoSet.Crime 1)
  { cdTraits = fromList []
  , cdUnique = False
  , cdKeywords = fromList []
  , cdBoostIcons = [MojoBoost.Boost]
  , cdAcceleration = 1
  }

mojo39061 :: CardDef
mojo39061 = (mojoCardDef "39061" "Family Matters" MojoSet.Sitcom 1)
  { cdTraits = fromList []
  , cdUnique = False
  , cdKeywords = fromList []
  , cdBoostIcons = [MojoBoost.Boost, MojoBoost.Boost, MojoBoost.Boost]
  }

mojo39062 :: CardDef
mojo39062 = (mojoCardDef "39062" "Growing Pains" MojoSet.Sitcom 1)
  { cdTraits = fromList []
  , cdUnique = False
  , cdKeywords = fromList []
  , cdBoostIcons = []
  }

mojo39063 :: CardDef
mojo39063 = (mojoCardDef "39063" "The Odd Couple" MojoSet.Sitcom 1)
  { cdTraits = fromList []
  , cdUnique = False
  , cdKeywords = fromList []
  , cdBoostIcons = [MojoBoost.Boost]
  }

mojo39064 :: CardDef
mojo39064 = (mojoCardDef "39064" "The One with the Breakup" MojoSet.Sitcom 1)
  { cdTraits = fromList []
  , cdUnique = False
  , cdKeywords = fromList []
  , cdBoostIcons = [MojoBoost.Boost, MojoBoost.Boost]
  , cdHazards = 1
  }

mojo39065 :: CardDef
mojo39065 = (mojoCardDef "39065" "Watch Me Play" MojoSet.Sitcom 1)
  { cdTraits = fromList []
  , cdUnique = False
  , cdKeywords = fromList [MojoKeyword.Peril, MojoKeyword.Incite (3)]
  , cdBoostIcons = [MojoBoost.Boost, MojoBoost.Boost]
  }

-- NeXt Evolution: Cable

cable40031 :: CardDef
cable40031 = (obligation "40031" "Technovirus Resurgence" [NE.Boost, NE.Boost]) { cdUnique = False, cdDeckLimit = Just 1 }

-- NeXt Evolution: Domino
memoriesofArmageddon :: CardDef
memoriesofArmageddon = CardDef
  { cdCardCode = "40065", cdName = "Memories of Armageddon", cdCost = Nothing
  , cdTraits = fromList [], cdKeywords = fromList []
  , cdCardType = ObligationType, cdAbilityType = Nothing, cdAbilitySubTypes = []
  , cdUnique = False, cdAspect = Nothing, cdCriteria = NoCriteria
  , cdResources = [], cdBoostIcons = [NEB.Boost, NEB.Boost]
  , cdEncounterSet = Nothing, cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing, cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Nothing, cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False, cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False, cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = NoCost
  }

-- NeXt Evolution campaign and scenarios

ne40160 :: CardDef
ne40160 = CardDef
  { cdCardCode = "40160"
  , cdName = "Manufactured Drama"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = ObligationType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.TelepathySet
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

ne40161 :: CardDef
ne40161 = CardDef
  { cdCardCode = "40161"
  , cdName = "Sowing Discord"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = ObligationType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.TelepathySet
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

-- Psylocke

bodySwapped :: CardDef
bodySwapped = CardDef
  { cdCardCode = "41025", cdName = "Body Swapped", cdCost = Nothing
  , cdTraits = fromList [], cdKeywords = fromList []
  , cdCardType = PsyCard.ObligationType, cdAbilityType = Nothing, cdAbilitySubTypes = []
  , cdUnique = False, cdAspect = Nothing, cdCriteria = PsyCriteria.NoCriteria
  , cdResources = [], cdBoostIcons = [PsyBoost.Boost, PsyBoost.Boost]
  , cdEncounterSet = Nothing, cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 3, cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False, cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False, cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = PsyCost.NoCost
  }

-- Angel

apocalypticInfluence :: CardDef
apocalypticInfluence = CardDef
  { cdCardCode = "42024"
  , cdName = "Apocalyptic Influence"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = AnCard.ObligationType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = AnCriteria.NoCriteria
  , cdResources = []
  , cdBoostIcons = [AnBoost.Boost, AnBoost.Boost]
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 1
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

selfIsolation :: CardDef
selfIsolation = CardDef
  { cdCardCode = "43028"
  , cdName = "Self-Isolation"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = AnCard.ObligationType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = AnCriteria.NoCriteria
  , cdResources = []
  , cdBoostIcons = [AnBoost.Boost, AnBoost.Boost]
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
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

theMercWithTheMouth :: CardDef
theMercWithTheMouth = DpCard.CardDef
  { cdCardCode = "44032"
  , cdName = DpName.Name "The Merc with the Mouth" (Nothing)
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = DpCard.ObligationType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = DpCriteria.NoCriteria
  , cdResources = []
  , cdBoostIcons = [DpBoost.Boost, DpBoost.Boost]
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
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

fearTheFuture :: CardDef
fearTheFuture = CardDef
  { cdCardCode = "45025"
  , cdName = "Fear the Future"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = BpCard.ObligationType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = BpCriteria.NoCriteria
  , cdResources = []
  , cdBoostIcons = [BpBoost.Boost, BpBoost.Boost]
  , cdEncounterSet = Just BpSet.BishopObligation
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
  , cdAbilityCost = BpCost.NoCost
  }

-- Age of Apocalypse: Magik
darkchilde :: CardDef
darkchilde = CardDef
  { cdCardCode = "45053"
  , cdName = "Darkchilde"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = ObligationType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [AoaMB.Boost, AoaMB.Boost]
  , cdEncounterSet = Just AoaMS.MagikObligation
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

hunted :: CardDef
hunted = CardDef
  { cdCardCode = "45072"
  , cdName = "Hunted"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = ObligationType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.AoaDystopianNightmare
  , cdEncounterSetQuantity = Just 2
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 1
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

drawingNear :: CardDef
drawingNear = CardDef
  { cdCardCode = "45080"
  , cdName = "Drawing Near"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = ObligationType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost]
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

panickedRefugees :: CardDef
panickedRefugees = CardDef
  { cdCardCode = "45178"
  , cdName = "Panicked Refugees"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = ObligationType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.AoaCampaign
  , cdEncounterSetQuantity = Just 4
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 1
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

oldGrievances :: CardDef
oldGrievances = CardDef
  { cdCardCode = "49027"
  , cdName = "Old Grievances"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = MgCard.ObligationType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = MgCriteria.NoCriteria
  , cdResources = []
  , cdBoostIcons = [MgBoost.Boost, MgBoost.Boost]
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
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
hotHeaded :: CardDef
hotHeaded = IcemanPack.hotHeaded

-- Nightcrawler
nc48026 :: CardDef
nc48026 = CardDef
  { cdCardCode = "48026"
  , cdName = "Crisis of Faith"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = ObligationType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NcCriteria.NoCriteria
  , cdResources = []
  , cdBoostIcons = [NcBoost.Boost, NcBoost.Boost]
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
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

aosP50029 :: CardDef
aosP50029 = CardDef
  { cdCardCode = "50029"
  , cdName = "Press Conference"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = AospCard.ObligationType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = AospCriteria.NoCriteria
  , cdResources = []
  , cdBoostIcons = [AospBoost.Boost, AospBoost.Boost]
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
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

aosP50059 :: CardDef
aosP50059 = CardDef
  { cdCardCode = "50059"
  , cdName = "Discovered"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = AospCard.ObligationType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = AospCriteria.NoCriteria
  , cdResources = []
  , cdBoostIcons = [AospBoost.Boost, AospBoost.Boost]
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
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

aos50134 :: CardDef
aos50134 = AosEncounter.aos50134

aos50179 :: CardDef
aos50179 = AosEncounter.aos50179
