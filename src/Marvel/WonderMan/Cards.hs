module Marvel.WonderMan.Cards where
import Marvel.Prelude
import Marvel.Card.Def
import Marvel.Name
import Marvel.Trait qualified as T
import Marvel.Resource.Types qualified as R
import Marvel.Keyword qualified as K
import Marvel.EncounterSet qualified as S
import Marvel.Ability.Types qualified as A
import Marvel.Criteria qualified as C
import Marvel.Cost.Types qualified as Cost
import Marvel.Aspect qualified as P
import Marvel.Boost qualified as B
import Marvel.Matchers qualified as M
import Marvel.Window qualified as W

wm58001a :: CardDef
wm58001a = CardDef
  { cdCardCode = "58001a", cdName = Name "Wonder Man" (Nothing)
  , cdCost = Nothing
  , cdTraits = fromList [T.Aerial, T.Avenger]
  , cdKeywords = fromList []
  , cdCardType = HeroType, cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True, cdCriteria = C.NoCriteria
  , cdAspect = Nothing
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 3
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False
  , cdDoubleWildResources = False, cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = Cost.NoCost
  }

wm58001b :: CardDef
wm58001b = CardDef
  { cdCardCode = "58001b", cdName = Name "Simon Williams" (Nothing)
  , cdCost = Nothing
  , cdTraits = fromList [T.Civilian]
  , cdKeywords = fromList []
  , cdCardType = AlterEgoType, cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True, cdCriteria = C.NoCriteria
  , cdAspect = Nothing
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 3
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False
  , cdDoubleWildResources = False, cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = Cost.NoCost
  }

wm58002 :: CardDef
wm58002 = CardDef
  { cdCardCode = "58002", cdName = Name "Ionic Physiology" (Nothing)
  , cdCost = Nothing
  , cdTraits = fromList [T.Superpower]
  , cdKeywords = fromList [K.Permanent]
  , cdCardType = UpgradeType, cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False, cdCriteria = C.NoCriteria
  , cdAspect = Nothing
  , cdResources = [(PrintedResource, R.Energy)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 1
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False
  , cdDoubleWildResources = False, cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = Cost.NoCost
  }

wm58003 :: CardDef
wm58003 = CardDef
  { cdCardCode = "58003", cdName = Name "Active Altruism" (Nothing)
  , cdCost = Just 0
  , cdTraits = fromList [T.Thwart]
  , cdKeywords = fromList []
  , cdCardType = EventType, cdAbilityType = Just A.HeroAction
  , cdAbilitySubTypes = [A.Thwart]
  , cdUnique = False, cdCriteria = C.AnyCriteria [C.SelfMatches M.ConfusedIdentity, C.SchemeExists M.SchemeYouCanThwart]
  , cdAspect = Nothing
  , cdResources = [(PrintedResource, R.Energy)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 3
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False
  , cdDoubleWildResources = False, cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = Cost.MayOverpay
  }

wm58004 :: CardDef
wm58004 = CardDef
  { cdCardCode = "58004", cdName = Name "Ionic Blast" (Nothing)
  , cdCost = Just 1
  , cdTraits = fromList [T.Attack, T.Superpower]
  , cdKeywords = fromList []
  , cdCardType = EventType, cdAbilityType = Just A.HeroAction
  , cdAbilitySubTypes = [A.Attack]
  , cdUnique = False, cdCriteria = C.canAttack
  , cdAspect = Nothing
  , cdResources = [(PrintedResource, R.Energy)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 2
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False
  , cdDoubleWildResources = False, cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = Cost.MayOverpay
  }

wm58005 :: CardDef
wm58005 = CardDef
  { cdCardCode = "58005", cdName = Name "Starstruck" (Nothing)
  , cdCost = Just 1
  , cdTraits = fromList [T.Attack, T.Superpower]
  , cdKeywords = fromList []
  , cdCardType = EventType, cdAbilityType = Just A.HeroAction
  , cdAbilitySubTypes = [A.Attack]
  , cdUnique = False, cdCriteria = C.canAttack
  , cdAspect = Nothing
  , cdResources = [(PrintedResource, R.Energy)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 3
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False
  , cdDoubleWildResources = False, cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = Cost.MayOverpay
  }

wm58006 :: CardDef
wm58006 = CardDef
  { cdCardCode = "58006", cdName = Name "Energy Siphon" (Nothing)
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = ResourceType, cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False, cdCriteria = C.NoCriteria
  , cdAspect = Nothing
  , cdResources = [(PrintedResource, R.Energy)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 2
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False
  , cdDoubleWildResources = False, cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = Cost.NoCost
  }

wm58007 :: CardDef
wm58007 = CardDef
  { cdCardCode = "58007", cdName = Name "Wonder Fans" (Nothing)
  , cdCost = Just 1
  , cdTraits = fromList [T.Civilian]
  , cdKeywords = fromList []
  , cdCardType = SupportType, cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False, cdCriteria = C.NoCriteria
  , cdAspect = Nothing
  , cdResources = [(PrintedResource, R.Wild)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 1
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False
  , cdDoubleWildResources = False, cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = Cost.NoCost
  }

wm58008 :: CardDef
wm58008 = CardDef
  { cdCardCode = "58008", cdName = Name "Jet Belt" (Nothing)
  , cdCost = Just 2
  , cdTraits = fromList [T.Item, T.Tech]
  , cdKeywords = fromList []
  , cdCardType = UpgradeType, cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True, cdCriteria = C.NoCriteria
  , cdAspect = Nothing
  , cdResources = [(PrintedResource, R.Mental)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 1
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False
  , cdDoubleWildResources = False, cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = Cost.NoCost
  }

wm58009 :: CardDef
wm58009 = CardDef
  { cdCardCode = "58009", cdName = Name "Mr. Hollywood" (Nothing)
  , cdCost = Just 1
  , cdTraits = fromList [T.Title]
  , cdKeywords = fromList []
  , cdCardType = UpgradeType, cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True, cdCriteria = C.NoCriteria
  , cdAspect = Nothing
  , cdResources = [(PrintedResource, R.Wild)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 1
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False
  , cdDoubleWildResources = False, cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = Cost.MayOverpay
  }

wm58010 :: CardDef
wm58010 = CardDef
  { cdCardCode = "58010", cdName = Name "Signature Sunglasses" (Nothing)
  , cdCost = Just 1
  , cdTraits = fromList [T.Item]
  , cdKeywords = fromList []
  , cdCardType = UpgradeType, cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True, cdCriteria = C.NoCriteria
  , cdAspect = Nothing
  , cdResources = [(PrintedResource, R.Physical)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 1
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False
  , cdDoubleWildResources = False, cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = Cost.NoCost
  }

wm58011 :: CardDef
wm58011 = CardDef
  { cdCardCode = "58011", cdName = Name "\"What Are You?\"" (Nothing)
  , cdCost = Just 2
  , cdTraits = fromList [T.Superpower]
  , cdKeywords = fromList []
  , cdCardType = UpgradeType, cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False, cdCriteria = C.NoCriteria
  , cdAspect = Nothing
  , cdResources = [(PrintedResource, R.Energy)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 1
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False
  , cdDoubleWildResources = False, cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = Cost.NoCost
  }

wm58012 :: CardDef
wm58012 = CardDef
  { cdCardCode = "58012", cdName = Name "Firebird" (Just "Bonita Juarez")
  , cdCost = Just 2
  , cdTraits = fromList [T.Aerial, T.Avenger]
  , cdKeywords = fromList []
  , cdCardType = AllyType, cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True, cdCriteria = C.NoCriteria
  , cdAspect = Just P.Justice
  , cdResources = [(PrintedResource, R.Energy)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 1
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False
  , cdDoubleWildResources = False, cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = Cost.MayOverpay
  }

wm58013 :: CardDef
wm58013 = CardDef
  { cdCardCode = "58013", cdName = Name "Hawkeye" (Just "Clint Barton")
  , cdCost = Just 3
  , cdTraits = fromList [T.Avenger, T.Shield]
  , cdKeywords = fromList []
  , cdCardType = AllyType, cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True, cdCriteria = C.NoCriteria
  , cdAspect = Just P.Justice
  , cdResources = [(PrintedResource, R.Energy)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 1
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False
  , cdDoubleWildResources = False, cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = Cost.NoCost
  }

wm58014 :: CardDef
wm58014 = CardDef
  { cdCardCode = "58014", cdName = Name "Scarlet Witch" (Just "Wanda Maximoff")
  , cdCost = Just 2
  , cdTraits = fromList [T.Avenger, T.Mystic]
  , cdKeywords = fromList []
  , cdCardType = AllyType, cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True, cdCriteria = C.NoCriteria
  , cdAspect = Just P.Justice
  , cdResources = [(PrintedResource, R.Mental)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 1
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False
  , cdDoubleWildResources = False, cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = Cost.NoCost
  }

wm58015 :: CardDef
wm58015 = CardDef
  { cdCardCode = "58015", cdName = Name "Sentry" (Just "Robert Reynolds")
  , cdCost = Just 3
  , cdTraits = fromList [T.Aerial, T.Avenger]
  , cdKeywords = fromList []
  , cdCardType = AllyType, cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True, cdCriteria = C.NoCriteria
  , cdAspect = Just P.Justice
  , cdResources = [(PrintedResource, R.Energy)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 1
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False
  , cdDoubleWildResources = False, cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = Cost.NoCost
  }

wm58016 :: CardDef
wm58016 = CardDef
  { cdCardCode = "58016", cdName = Name "Battlefield Benevolence" (Nothing)
  , cdCost = Just 1
  , cdTraits = fromList [T.Tactic]
  , cdKeywords = fromList []
  , cdCardType = EventType, cdAbilityType = Just A.HeroAction
  , cdAbilitySubTypes = []
  , cdUnique = False, cdCriteria = C.NoCriteria
  , cdAspect = Just P.Justice
  , cdResources = [(PrintedResource, R.Physical)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 3
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False
  , cdDoubleWildResources = False, cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = Cost.HealEnemyCost 2 M.EnemyCanBeConfused
  }

wm58018 :: CardDef
wm58018 = CardDef
  { cdCardCode = "58018", cdName = Name "Everywhere All at Once" (Nothing)
  , cdCost = Just (-1)
  , cdTraits = fromList [T.Aerial, T.Thwart]
  , cdKeywords = fromList []
  , cdCardType = EventType, cdAbilityType = Just A.HeroAction
  , cdAbilitySubTypes = [A.Thwart]
  , cdUnique = False, cdCriteria = C.AnyCriteria [C.SelfMatches M.ConfusedIdentity, C.SchemeExists M.SchemeYouCanThwart] <> C.SelfMatches (M.IdentityWithTrait T.Aerial)
  , cdAspect = Just P.Justice
  , cdResources = [(PrintedResource, R.Energy)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 3
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False
  , cdDoubleWildResources = False, cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = Cost.MayOverpay
  }

wm58019 :: CardDef
wm58019 = CardDef
  { cdCardCode = "58019", cdName = Name "Stronger Together" (Nothing)
  , cdCost = Just 0
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = EventType, cdAbilityType = Just A.HeroInterrupt
  , cdAbilitySubTypes = []
  , cdUnique = False, cdCriteria = C.NoCriteria
  , cdAspect = Just P.Protection
  , cdResources = [(PrintedResource, R.Energy)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Just $ W.CharacterSharingHeroTraitWouldTakeDamage M.You, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 3
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False
  , cdDoubleWildResources = False, cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = Cost.NoCost
  }

wm58020 :: CardDef
wm58020 = CardDef
  { cdCardCode = "58020", cdName = Name "Unified Strike" (Nothing)
  , cdCost = Just 0
  , cdTraits = fromList [T.Tactic]
  , cdKeywords = fromList []
  , cdCardType = EventType, cdAbilityType = Just A.HeroInterrupt
  , cdAbilitySubTypes = []
  , cdUnique = False, cdCriteria = C.NoCriteria
  , cdAspect = Just P.Leadership
  , cdResources = [(PrintedResource, R.Energy)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Just $ W.WindowOneOf [W.AllyAttacked W.When M.AnyAlly M.AnyEnemy, W.AllyThwarted W.When M.AnyAlly M.AnyScheme], cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 3
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False
  , cdDoubleWildResources = False, cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = Cost.ExhaustCost
  }

wm58021 :: CardDef
wm58021 = CardDef
  { cdCardCode = "58021", cdName = Name "Heroic Conditioning" (Nothing)
  , cdCost = Just 3
  , cdTraits = fromList [T.Condition]
  , cdKeywords = fromList []
  , cdCardType = UpgradeType, cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False, cdCriteria = C.NoCriteria
  , cdAspect = Just P.Justice
  , cdResources = [(PrintedResource, R.Physical)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Just (MaxPerPlayerUnderAnyControl 1), cdDeckLimit = Just 3
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False
  , cdDoubleWildResources = False, cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = Cost.NoCost
  }

wm58022 :: CardDef
wm58022 = CardDef
  { cdCardCode = "58022", cdName = Name "Swordsman" (Just "Jacques Duquesne")
  , cdCost = Just 4
  , cdTraits = fromList [T.Avenger]
  , cdKeywords = fromList []
  , cdCardType = AllyType, cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True, cdCriteria = C.NoCriteria
  , cdAspect = Just P.Neutral
  , cdResources = [(PrintedResource, R.Physical)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 1
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False
  , cdDoubleWildResources = False, cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = Cost.NoCost
  }

wm58024 :: CardDef
wm58024 = CardDef
  { cdCardCode = "58024", cdName = Name "Jarvis" (Nothing)
  , cdCost = Just 2
  , cdTraits = fromList [T.Avenger, T.Persona]
  , cdKeywords = fromList []
  , cdCardType = SupportType, cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True, cdCriteria = C.NoCriteria
  , cdAspect = Just P.Neutral
  , cdResources = [(PrintedResource, R.Energy)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 1
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False
  , cdDoubleWildResources = False, cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = Cost.NoCost
  }

wm58025 :: CardDef
wm58025 = CardDef
  { cdCardCode = "58025", cdName = Name "Pacifism" (Nothing)
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = ObligationType, cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False, cdCriteria = C.NoCriteria
  , cdAspect = Nothing
  , cdResources = []
  , cdBoostIcons = [B.Boost, B.Boost]
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 3
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False
  , cdDoubleWildResources = False, cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = Cost.NoCost
  }

wm58026 :: CardDef
wm58026 = CardDef
  { cdCardCode = "58026", cdName = Name "Grim Reaper" (Nothing)
  , cdCost = Nothing
  , cdTraits = fromList [T.Cyborg]
  , cdKeywords = fromList [K.Quickstrike]
  , cdCardType = MinionType, cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True, cdCriteria = C.NoCriteria
  , cdAspect = Nothing
  , cdResources = []
  , cdBoostIcons = [B.Boost, B.Boost]
  , cdEncounterSet = Just S.WonderManNemesisSet
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 3
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False
  , cdDoubleWildResources = False, cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = Cost.NoCost
  }

wm58027 :: CardDef
wm58027 = CardDef
  { cdCardCode = "58027", cdName = Name "Brother vs. Brother" (Nothing)
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = SideSchemeType, cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False, cdCriteria = C.NoCriteria
  , cdAspect = Nothing
  , cdResources = []
  , cdBoostIcons = [B.Boost, B.Boost, B.Boost]
  , cdEncounterSet = Just S.WonderManNemesisSet
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 1
  , cdLimit = Nothing, cdDeckLimit = Just 3
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False
  , cdDoubleWildResources = False, cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = Cost.NoCost
  }

wm58028 :: CardDef
wm58028 = CardDef
  { cdCardCode = "58028", cdName = Name "Scythe Strike" (Nothing)
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = TreacheryType, cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False, cdCriteria = C.NoCriteria
  , cdAspect = Nothing
  , cdResources = []
  , cdBoostIcons = [B.Boost, B.Boost]
  , cdEncounterSet = Just S.WonderManNemesisSet
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 3
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False
  , cdDoubleWildResources = False, cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = Cost.NoCost
  }

wm58029 :: CardDef
wm58029 = CardDef
  { cdCardCode = "58029", cdName = Name "Death Cannot Die" (Nothing)
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = TreacheryType, cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False, cdCriteria = C.NoCriteria
  , cdAspect = Nothing
  , cdResources = []
  , cdBoostIcons = [B.Star]
  , cdEncounterSet = Just S.WonderManNemesisSet
  , cdEncounterSetQuantity = Just 2
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 3
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False
  , cdDoubleWildResources = False, cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = Cost.NoCost
  }

wm58030 :: CardDef
wm58030 = CardDef
  { cdCardCode = "58030", cdName = Name "Caught in the Crossfire" (Nothing)
  , cdCost = Just 0
  , cdTraits = fromList [T.Tactic]
  , cdKeywords = fromList []
  , cdCardType = UpgradeType, cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False, cdCriteria = C.NoCriteria
  , cdAspect = Just P.Aggression
  , cdResources = [(PrintedResource, R.Energy)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 3
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False
  , cdDoubleWildResources = False, cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = Cost.NoCost
  }

wm58031 :: CardDef
wm58031 = CardDef
  { cdCardCode = "58031", cdName = Name "Cameo" (Nothing)
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList [K.Setup]
  , cdCardType = SupportType, cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False, cdCriteria = C.NoCriteria
  , cdAspect = Just P.Leadership
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 1
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False
  , cdDoubleWildResources = False, cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = Cost.NoCost
  }

wm58032 :: CardDef
wm58032 = CardDef
  { cdCardCode = "58032", cdName = Name "Coordinated Effort" (Nothing)
  , cdCost = Just 0
  , cdTraits = fromList [T.Tactic]
  , cdKeywords = fromList []
  , cdCardType = UpgradeType, cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False, cdCriteria = C.NoCriteria
  , cdAspect = Just P.Leadership
  , cdResources = [(PrintedResource, R.Wild)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 3
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False
  , cdDoubleWildResources = False, cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = Cost.NoCost
  }

wm58033 :: CardDef
wm58033 = CardDef
  { cdCardCode = "58033", cdName = Name "Disarming Defense" (Nothing)
  , cdCost = Just 1
  , cdTraits = fromList [T.Defense]
  , cdKeywords = fromList []
  , cdCardType = EventType, cdAbilityType = Just A.HeroInterrupt
  , cdAbilitySubTypes = [A.Defense]
  , cdUnique = False, cdCriteria = C.NoCriteria
  , cdAspect = Just P.Protection
  , cdResources = [(PrintedResource, R.Energy)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Just $ W.HeroDefended W.When M.You M.AnyEnemy, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 3
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False
  , cdDoubleWildResources = False, cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = Cost.NoCost
  }

wm58034 :: CardDef
wm58034 = reprintedAs ["59032"] $ CardDef
  { cdCardCode = "58034", cdName = Name "Avengers Compound" (Nothing)
  , cdCost = Just 0
  , cdTraits = fromList [T.Avenger, T.Location]
  , cdKeywords = fromList []
  , cdCardType = SupportType, cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False, cdCriteria = C.SelfMatches (M.IdentityWithTrait T.Avenger)
  , cdAspect = Just P.Neutral
  , cdResources = [(PrintedResource, R.Mental)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 1
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False
  , cdDoubleWildResources = False, cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = Cost.NoCost
  }
