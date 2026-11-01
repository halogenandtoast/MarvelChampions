module Marvel.Iceman.Cards where
import Marvel.Prelude
import Marvel.Card.Def
import Marvel.Ability.Types qualified as A
import Marvel.Aspect qualified as Aspect
import Marvel.Boost qualified as B
import Marvel.Cost.Types qualified as Cost
import Marvel.Criteria
import Marvel.EncounterSet qualified as Set
import Marvel.Keyword qualified as K
import Marvel.Matchers
import Marvel.Name
import Marvel.Resource.Types qualified as R
import Marvel.Trait.Types qualified as T
import Marvel.Window.Types qualified as W

icemanHero :: CardDef
icemanHero = CardDef
  { cdCardCode = "46001a"
  , cdName = "Iceman"
  , cdCost = Nothing
  , cdTraits = fromList [T.Ice, T.XMen]
  , cdKeywords = fromList []
  , cdCardType = HeroType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
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
  , cdAbilityCost = Cost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

bobbyDrake :: CardDef
bobbyDrake = CardDef
  { cdCardCode = "46001b"
  , cdName = "Bobby Drake"
  , cdCost = Nothing
  , cdTraits = fromList [T.Mutant]
  , cdKeywords = fromList []
  , cdCardType = AlterEgoType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
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
  , cdAbilityCost = Cost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

frostbite :: CardDef
frostbite = CardDef
  { cdCardCode = "46002"
  , cdName = "Frostbite"
  , cdCost = Nothing
  , cdTraits = fromList [T.Condition, T.Ice]
  , cdKeywords = fromList [K.Permanent]
  , cdCardType = UpgradeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 6
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = Cost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

snowClone :: CardDef
snowClone = CardDef
  { cdCardCode = "46003"
  , cdName = "Snow Clone"
  , cdCost = Just 2
  , cdTraits = fromList [T.Ice, T.XMen]
  , cdKeywords = fromList []
  , cdCardType = AllyType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = [(PrintedResource, R.Physical)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 2
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = Cost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

powerBelt :: CardDef
powerBelt = CardDef
  { cdCardCode = "46004"
  , cdName = "Power Belt"
  , cdCost = Just 2
  , cdTraits = fromList [T.Item, T.Tech]
  , cdKeywords = fromList []
  , cdCardType = UpgradeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = [(PrintedResource, R.Energy)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 1
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = Cost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

cryokineticPerception :: CardDef
cryokineticPerception = CardDef
  { cdCardCode = "46005"
  , cdName = "Cryokinetic Perception"
  , cdCost = Just 2
  , cdTraits = fromList [T.Superpower]
  , cdKeywords = fromList []
  , cdCardType = UpgradeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = [(PrintedResource, R.Mental)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 1
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = Cost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

iceSlide :: CardDef
iceSlide = CardDef
  { cdCardCode = "46006"
  , cdName = "Ice Slide"
  , cdCost = Just 2
  , cdTraits = fromList [T.Ice, T.Superpower]
  , cdKeywords = fromList []
  , cdCardType = UpgradeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = [(PrintedResource, R.Energy)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 1
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = Cost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

frozenSolid :: CardDef
frozenSolid = CardDef
  { cdCardCode = "46007"
  , cdName = "Frozen Solid"
  , cdCost = Just 3
  , cdTraits = fromList [T.Condition, T.Ice]
  , cdKeywords = fromList []
  , cdCardType = UpgradeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = SelfMatches HeroIdentity
  , cdResources = [(PrintedResource, R.Mental)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Just (MaxPerEnemy 1)
  , cdDeckLimit = Just 2
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = Cost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

iceWall :: CardDef
iceWall = CardDef
  { cdCardCode = "46008"
  , cdName = "Ice Wall"
  , cdCost = Just 4
  , cdTraits = fromList [T.Ice]
  , cdKeywords = fromList []
  , cdCardType = SupportType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = [(PrintedResource, R.Physical)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 1
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = Cost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

arcticAttack :: CardDef
arcticAttack = CardDef
  { cdCardCode = "46009"
  , cdName = "Arctic Attack"
  , cdCost = Just 2
  , cdTraits = fromList [T.Attack, T.Ice, T.Superpower]
  , cdKeywords = fromList []
  , cdCardType = EventType
  , cdAbilityType = Just A.HeroAction
  , cdAbilitySubTypes = [A.Attack]
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = [(PrintedResource, R.Energy)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 2
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = Cost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

iceBlast :: CardDef
iceBlast = CardDef
  { cdCardCode = "46010"
  , cdName = "Ice Blast"
  , cdCost = Just 3
  , cdTraits = fromList [T.Ice, T.Superpower]
  , cdKeywords = fromList []
  , cdCardType = EventType
  , cdAbilityType = Just A.HeroAction
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = [(PrintedResource, R.Physical)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 2
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = Cost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

chillOut :: CardDef
chillOut = CardDef
  { cdCardCode = "46011"
  , cdName = "Chill Out!"
  , cdCost = Just 2
  , cdTraits = fromList [T.Ice, T.Superpower, T.Thwart]
  , cdKeywords = fromList []
  , cdCardType = EventType
  , cdAbilityType = Just A.HeroAction
  , cdAbilitySubTypes = [A.Thwart]
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = [(PrintedResource, R.Mental)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 3
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = Cost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

sharkGirl :: CardDef
sharkGirl = CardDef
  { cdCardCode = "46012"
  , cdName = ("Shark-Girl" <:> "Iara Dos Santos")
  , cdCost = Just 2
  , cdTraits = fromList [T.XMen]
  , cdKeywords = fromList []
  , cdCardType = AllyType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Just Aspect.Aggression
  , cdCriteria = NoCriteria
  , cdResources = [(PrintedResource, R.Energy)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 1
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = Cost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

glob :: CardDef
glob = CardDef
  { cdCardCode = "46013"
  , cdName = ("Glob" <:> "Robert Herman")
  , cdCost = Just 3
  , cdTraits = fromList [T.XMen]
  , cdKeywords = fromList []
  , cdCardType = AllyType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Just Aspect.Aggression
  , cdCriteria = SelfMatches (IdentityWithTrait T.XMen)
  , cdResources = [(PrintedResource, R.Energy)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 1
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = Cost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

suppressingFire :: CardDef
suppressingFire = CardDef
  { cdCardCode = "46014"
  , cdName = "Suppressing Fire"
  , cdCost = Just 0
  , cdTraits = fromList [T.Tactic]
  , cdKeywords = fromList []
  , cdCardType = UpgradeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Just Aspect.Aggression
  , cdCriteria = NoCriteria
  , cdResources = [(PrintedResource, R.Mental)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Just (MaxPerEnemy 1)
  , cdDeckLimit = Just 3
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = Cost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

surpriseMove :: CardDef
surpriseMove = CardDef
  { cdCardCode = "46015"
  , cdName = "Surprise Move"
  , cdCost = Just 1
  , cdTraits = fromList [T.Tactic]
  , cdKeywords = fromList []
  , cdCardType = EventType
  , cdAbilityType = Just A.HeroInterrupt
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Just Aspect.Aggression
  , cdCriteria = EnemyExists (EnemyWithUpgrade (UpgradeMatches []))
  , cdResources = [(PrintedResource, R.Mental)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Just (W.MakesBasicAttackAgainst W.When You (EnemyWithUpgrade (UpgradeMatches [])))
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 3
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = Cost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

takeThat :: CardDef
takeThat = CardDef
  { cdCardCode = "46016"
  , cdName = "Take That!"
  , cdCost = Just 3
  , cdTraits = fromList [T.Attack]
  , cdKeywords = fromList []
  , cdCardType = EventType
  , cdAbilityType = Just A.HeroAction
  , cdAbilitySubTypes = [A.Attack]
  , cdUnique = False
  , cdAspect = Just Aspect.Aggression
  , cdCriteria = EnemyExists (AttackableEnemy <> EnemyWithUpgrade (UpgradeMatches []))
  , cdResources = [(PrintedResource, R.Energy)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 3
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = Cost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

keepUpThePressure :: CardDef
keepUpThePressure = CardDef
  { cdCardCode = "46018"
  , cdName = "Keep Up the Pressure"
  , cdCost = Just 0
  , cdTraits = fromList []
  , cdKeywords = fromList [K.Victory 0]
  , cdCardType = PlayerSideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Just Aspect.Aggression
  , cdCriteria = NoCriteria
  , cdResources = [(PrintedResource, R.Energy)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 1
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = Cost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

shadowcatIceman :: CardDef
shadowcatIceman = CardDef
  { cdCardCode = "46019"
  , cdName = ("Shadowcat" <:> "Kitty Pryde")
  , cdCost = Just 3
  , cdTraits = fromList [T.XMen]
  , cdKeywords = fromList []
  , cdCardType = AllyType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Just Aspect.Neutral
  , cdCriteria = SelfMatches (IdentityWithTrait T.XMen)
  , cdResources = [(PrintedResource, R.Mental)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 1
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = Cost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

beak :: CardDef
beak = CardDef
  { cdCardCode = "46020"
  , cdName = ("Beak" <:> "Barnell Bohusk")
  , cdCost = Just 2
  , cdTraits = fromList [T.XMen]
  , cdKeywords = fromList []
  , cdCardType = AllyType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Just Aspect.Neutral
  , cdCriteria = NoCriteria
  , cdResources = [(PrintedResource, R.Energy)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 1
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = Cost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

hotHeaded :: CardDef
hotHeaded = CardDef
  { cdCardCode = "46024"
  , cdName = "Hot-Headed"
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
  , cdBoostIcons = [B.Boost, B.Boost]
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
  , cdAbilityCost = Cost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

pyroIceman :: CardDef
pyroIceman = CardDef
  { cdCardCode = "46025"
  , cdName = "Pyro"
  , cdCost = Nothing
  , cdTraits = fromList [T.BrotherhoodOfMutants]
  , cdKeywords = fromList [K.Quickstrike]
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [B.Boost, B.Boost, B.Boost]
  , cdEncounterSet = Just Set.IcemanNemesis
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
  , cdAbilityCost = Cost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

playingWithFire :: CardDef
playingWithFire = CardDef
  { cdCardCode = "46026"
  , cdName = "Playing with Fire"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = SideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [B.Boost, B.Boost, B.Boost]
  , cdEncounterSet = Just Set.IcemanNemesis
  , cdEncounterSetQuantity = Just 1
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
  , cdAbilityCost = Cost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

pyrosFlamethrower :: CardDef
pyrosFlamethrower = CardDef
  { cdCardCode = "46027"
  , cdName = "Pyro's Flamethrower"
  , cdCost = Nothing
  , cdTraits = fromList [T.Weapon]
  , cdKeywords = fromList []
  , cdCardType = AttachmentType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [B.Boost, B.Boost]
  , cdEncounterSet = Just Set.IcemanNemesis
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
  , cdAbilityCost = Cost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

burnIceman :: CardDef
burnIceman = CardDef
  { cdCardCode = "46028"
  , cdName = "Burn!"
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
  , cdBoostIcons = [B.Star]
  , cdEncounterSet = Just Set.IcemanNemesis
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
  , cdAbilityCost = Cost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

sauron :: CardDef
sauron = CardDef
  { cdCardCode = "46029"
  , cdName = "Sauron"
  , cdCost = Nothing
  , cdTraits = fromList [T.BrotherhoodOfMutants]
  , cdKeywords = fromList []
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [B.Star]
  , cdEncounterSet = Just Set.SauronSet
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
  , cdAbilityCost = Cost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

sauronLives :: CardDef
sauronLives = CardDef
  { cdCardCode = "46030"
  , cdName = "Sauron Lives!"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = SideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [B.Boost, B.Boost, B.Boost]
  , cdEncounterSet = Just Set.SauronSet
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
  , cdAbilityCost = Cost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

lifeDrain :: CardDef
lifeDrain = CardDef
  { cdCardCode = "46031"
  , cdName = "Life Drain"
  , cdCost = Nothing
  , cdTraits = fromList [T.Superpower]
  , cdKeywords = fromList []
  , cdCardType = AttachmentType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [B.Boost, B.Boost]
  , cdEncounterSet = Just Set.SauronSet
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
  , cdAbilityCost = Cost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

theEyeOfSauron :: CardDef
theEyeOfSauron = CardDef
  { cdCardCode = "46032"
  , cdName = "The Eye of Sauron"
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
  , cdBoostIcons = [B.Boost]
  , cdEncounterSet = Just Set.SauronSet
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
  , cdAbilityCost = Cost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }
