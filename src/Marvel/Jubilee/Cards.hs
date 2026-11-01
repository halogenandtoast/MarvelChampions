module Marvel.Jubilee.Cards where
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
import Marvel.GameValue qualified as V
import Marvel.Window qualified as W
import Marvel.Matchers qualified as M

jubilee47001a :: CardDef
jubilee47001a = CardDef
  { cdCardCode = "47001a", cdName = Name "Jubilee" (Nothing)
  , cdCost = Nothing
  , cdTraits = fromList [T.XMen]
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
  , cdLimit = Nothing, cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False
  , cdDoubleWildResources = False, cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = Cost.NoCost
  }

jubilee47001b :: CardDef
jubilee47001b = CardDef
  { cdCardCode = "47001b", cdName = Name "Jubilation Lee" (Nothing)
  , cdCost = Nothing
  , cdTraits = fromList [T.Mutant]
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
  , cdLimit = Nothing, cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False
  , cdDoubleWildResources = False, cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = Cost.NoCost
  }

jubilee47002 :: CardDef
jubilee47002 = CardDef
  { cdCardCode = "47002", cdName = Name "Wolverine" (Just "Logan")
  , cdCost = Just 4
  , cdTraits = fromList [T.XMen]
  , cdKeywords = fromList []
  , cdCardType = AllyType, cdAbilityType = Nothing
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
  , cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = Cost.NoCost
  }

jubilee47003 :: CardDef
jubilee47003 = CardDef
  { cdCardCode = "47003", cdName = Name "Shopping Spree" (Nothing)
  , cdCost = Just 0
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = PlayerSideSchemeType, cdAbilityType = Nothing
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
  , cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = Cost.NoCost
  }

jubilee47004 :: CardDef
jubilee47004 = CardDef
  { cdCardCode = "47004", cdName = Name "Jubilee's Coat" (Nothing)
  , cdCost = Just 2
  , cdTraits = fromList [T.Item]
  , cdKeywords = fromList []
  , cdCardType = UpgradeType, cdAbilityType = Just A.HeroResponse
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
  , cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = Cost.NoCost
  }

jubilee47005 :: CardDef
jubilee47005 = CardDef
  { cdCardCode = "47005", cdName = Name "Jubilee's Sunglasses" (Nothing)
  , cdCost = Just 2
  , cdTraits = fromList [T.Item]
  , cdKeywords = fromList []
  , cdCardType = UpgradeType, cdAbilityType = Just A.HeroResponse
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
  , cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = Cost.NoCost
  }

jubilee47006 :: CardDef
jubilee47006 = CardDef
  { cdCardCode = "47006", cdName = Name "Blinding Flash" (Nothing)
  , cdCost = Just 3
  , cdTraits = fromList [T.Superpower]
  , cdKeywords = fromList []
  , cdCardType = EventType, cdAbilityType = Just A.HeroAction
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

jubilee47007a :: CardDef
jubilee47007a = CardDef
  { cdCardCode = "47007a", cdName = Name "Firecracker" (Nothing)
  , cdCost = Just 2
  , cdTraits = fromList [T.Attack, T.Superpower]
  , cdKeywords = fromList []
  , cdCardType = EventType, cdAbilityType = Just A.HeroAction
  , cdAbilitySubTypes = [A.Attack]
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

jubilee47007b :: CardDef
jubilee47007b = CardDef
  { cdCardCode = "47007b", cdName = Name "Firecracker" (Nothing)
  , cdCost = Just 2
  , cdTraits = fromList [T.Attack, T.Superpower]
  , cdKeywords = fromList []
  , cdCardType = EventType, cdAbilityType = Just A.HeroAction
  , cdAbilitySubTypes = [A.Attack]
  , cdUnique = False, cdCriteria = C.NoCriteria
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

jubilee47007c :: CardDef
jubilee47007c = CardDef
  { cdCardCode = "47007c", cdName = Name "Firecracker" (Nothing)
  , cdCost = Just 2
  , cdTraits = fromList [T.Attack, T.Superpower]
  , cdKeywords = fromList []
  , cdCardType = EventType, cdAbilityType = Just A.HeroAction
  , cdAbilitySubTypes = [A.Attack]
  , cdUnique = False, cdCriteria = C.NoCriteria
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

jubilee47008a :: CardDef
jubilee47008a = CardDef
  { cdCardCode = "47008a", cdName = Name "Flash of Light" (Nothing)
  , cdCost = Just 2
  , cdTraits = fromList [T.Superpower, T.Thwart]
  , cdKeywords = fromList []
  , cdCardType = EventType, cdAbilityType = Just A.HeroAction
  , cdAbilitySubTypes = [A.Thwart]
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

jubilee47008b :: CardDef
jubilee47008b = CardDef
  { cdCardCode = "47008b", cdName = Name "Flash of Light" (Nothing)
  , cdCost = Just 2
  , cdTraits = fromList [T.Superpower, T.Thwart]
  , cdKeywords = fromList []
  , cdCardType = EventType, cdAbilityType = Just A.HeroAction
  , cdAbilitySubTypes = [A.Thwart]
  , cdUnique = False, cdCriteria = C.NoCriteria
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

jubilee47008c :: CardDef
jubilee47008c = CardDef
  { cdCardCode = "47008c", cdName = Name "Flash of Light" (Nothing)
  , cdCost = Just 2
  , cdTraits = fromList [T.Superpower, T.Thwart]
  , cdKeywords = fromList []
  , cdCardType = EventType, cdAbilityType = Just A.HeroAction
  , cdAbilitySubTypes = [A.Thwart]
  , cdUnique = False, cdCriteria = C.NoCriteria
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

jubilee47009 :: CardDef
jubilee47009 = CardDef
  { cdCardCode = "47009", cdName = Name "Grande Finale" (Nothing)
  , cdCost = Just 3
  , cdTraits = fromList [T.Attack, T.Superpower]
  , cdKeywords = fromList []
  , cdCardType = EventType, cdAbilityType = Just A.HeroAction
  , cdAbilitySubTypes = [A.Attack]
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

jubilee47010a :: CardDef
jubilee47010a = CardDef
  { cdCardCode = "47010a", cdName = Name "Plasmoid Energy" (Nothing)
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = ResourceType, cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False, cdCriteria = C.NoCriteria
  , cdAspect = Nothing
  , cdResources = [(PrintedResource, R.Energy), (PrintedResource, R.Mental)]
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

jubilee47010b :: CardDef
jubilee47010b = CardDef
  { cdCardCode = "47010b", cdName = Name "Plasmoid Energy" (Nothing)
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = ResourceType, cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False, cdCriteria = C.NoCriteria
  , cdAspect = Nothing
  , cdResources = [(PrintedResource, R.Energy), (PrintedResource, R.Physical)]
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

jubilee47010c :: CardDef
jubilee47010c = CardDef
  { cdCardCode = "47010c", cdName = Name "Plasmoid Energy" (Nothing)
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = ResourceType, cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False, cdCriteria = C.NoCriteria
  , cdAspect = Nothing
  , cdResources = [(PrintedResource, R.Physical), (PrintedResource, R.Mental)]
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

jubilee47011 :: CardDef
jubilee47011 = CardDef
  { cdCardCode = "47011", cdName = Name "Chamber" (Just "Jono Starsmore")
  , cdCost = Just 4
  , cdTraits = fromList [T.Psionic, T.XMen]
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

jubilee47012 :: CardDef
jubilee47012 = CardDef
  { cdCardCode = "47012", cdName = Name "Husk" (Just "Paige Guthrie")
  , cdCost = Just 4
  , cdTraits = fromList [T.XMen]
  , cdKeywords = fromList []
  , cdCardType = AllyType, cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True, cdCriteria = C.NoCriteria
  , cdAspect = Just P.Justice
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

jubilee47013 :: CardDef
jubilee47013 = CardDef
  { cdCardCode = "47013", cdName = Name "Disguise" (Nothing)
  , cdCost = Just 1
  , cdTraits = fromList [T.Item]
  , cdKeywords = fromList []
  , cdCardType = UpgradeType, cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False, cdCriteria = C.NoCriteria
  , cdAspect = Just P.Justice
  , cdResources = [(PrintedResource, R.Mental)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Just (MaxPerPlayer 1), cdDeckLimit = Just 3
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False
  , cdDoubleWildResources = False, cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = Cost.NoCost
  }

jubilee47014 :: CardDef
jubilee47014 = CardDef
  { cdCardCode = "47014", cdName = Name "Waylay" (Nothing)
  , cdCost = Just 3
  , cdTraits = fromList [T.Attack]
  , cdKeywords = fromList []
  , cdCardType = EventType, cdAbilityType = Just A.HeroResponse
  , cdAbilitySubTypes = [A.Attack]
  , cdUnique = False, cdCriteria = C.NoCriteria
  , cdAspect = Just P.Justice
  , cdResources = [(PrintedResource, R.Physical)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Just (W.ThreatRemovedByThwart W.After (M.SourceIsHero M.You) M.AnyScheme), cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 3
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False
  , cdDoubleWildResources = False, cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = Cost.NoCost
  }

jubilee47015 :: CardDef
jubilee47015 = CardDef
  { cdCardCode = "47015", cdName = Name "Three Steps Ahead" (Nothing)
  , cdCost = Just 3
  , cdTraits = fromList [T.Thwart]
  , cdKeywords = fromList []
  , cdCardType = EventType, cdAbilityType = Just A.HeroAction
  , cdAbilitySubTypes = [A.Thwart]
  , cdUnique = False, cdCriteria = C.NoCriteria
  , cdAspect = Just P.Justice
  , cdResources = [(PrintedResource, R.Mental)]
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

jubilee47016 :: CardDef
jubilee47016 = CardDef
  { cdCardCode = "47016", cdName = Name "Generation X" (Nothing)
  , cdCost = Just 0
  , cdTraits = fromList []
  , cdKeywords = fromList [K.Victory 0]
  , cdCardType = PlayerSideSchemeType, cdAbilityType = Nothing
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

jubilee47018 :: CardDef
jubilee47018 = CardDef
  { cdCardCode = "47018", cdName = Name "Synch" (Just "Everett Thomas")
  , cdCost = Just 3
  , cdTraits = fromList [T.XMen]
  , cdKeywords = fromList []
  , cdCardType = AllyType, cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True, cdCriteria = C.SelfMatches (M.IdentityWithTrait T.XMen)
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

jubilee47019 :: CardDef
jubilee47019 = CardDef
  { cdCardCode = "47019", cdName = Name "Cell Phone" (Nothing)
  , cdCost = Just 2
  , cdTraits = fromList [T.Item]
  , cdKeywords = fromList []
  , cdCardType = UpgradeType, cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False, cdCriteria = C.NoCriteria
  , cdAspect = Just P.Neutral
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

jubilee47021 :: CardDef
jubilee47021 = CardDef
  { cdCardCode = "47021", cdName = Name "Multitalented" (Nothing)
  , cdCost = Just 3
  , cdTraits = fromList [T.Attack, T.Thwart]
  , cdKeywords = fromList []
  , cdCardType = EventType, cdAbilityType = Just A.HeroAction
  , cdAbilitySubTypes = [A.Attack, A.Thwart]
  , cdUnique = False, cdCriteria = C.NoCriteria
  , cdAspect = Just P.Neutral
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
  , cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = Cost.NoCost
  }

jubilee47022 :: CardDef
jubilee47022 = CardDef
  { cdCardCode = "47022", cdName = Name "Unlikely Duo" (Nothing)
  , cdCost = Just 2
  , cdTraits = fromList [T.Attack]
  , cdKeywords = fromList []
  , cdCardType = EventType, cdAbilityType = Just A.HeroAction
  , cdAbilitySubTypes = [A.Attack]
  , cdUnique = False, cdCriteria = jubileeTeamUp "Jubilee" "Wolverine"
  , cdAspect = Just P.Neutral
  , cdResources = [(PrintedResource, R.Wild)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 1
  , cdTeamUp = Just ("Jubilee", "Wolverine")
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False
  , cdDoubleWildResources = False, cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = Cost.NoCost
  }

jubilee47023 :: CardDef
jubilee47023 = CardDef
  { cdCardCode = "47023", cdName = Name "Grounded" (Nothing)
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
  , cdLimit = Nothing, cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False
  , cdDoubleWildResources = False, cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = Cost.NoCost
  }

jubilee47024 :: CardDef
jubilee47024 = CardDef
  { cdCardCode = "47024", cdName = Name "Nanny" (Nothing)
  , cdCost = Nothing
  , cdTraits = fromList [T.Mutant]
  , cdKeywords = fromList [K.Toughness]
  , cdCardType = MinionType, cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True, cdCriteria = C.NoCriteria
  , cdAspect = Nothing
  , cdResources = []
  , cdBoostIcons = [B.Boost, B.Boost]
  , cdEncounterSet = Just S.JubileeNemesis
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False
  , cdDoubleWildResources = False, cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = Cost.NoCost
  }

jubilee47025 :: CardDef
jubilee47025 = CardDef
  { cdCardCode = "47025", cdName = Name "Naughty Children" (Nothing)
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = SideSchemeType, cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False, cdCriteria = C.NoCriteria
  , cdAspect = Nothing
  , cdResources = []
  , cdBoostIcons = [B.Boost, B.Boost]
  , cdEncounterSet = Just S.JubileeNemesis
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False
  , cdDoubleWildResources = False, cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = Cost.NoCost
  }

jubilee47026 :: CardDef
jubilee47026 = CardDef
  { cdCardCode = "47026", cdName = Name "Battle Suit" (Nothing)
  , cdCost = Nothing
  , cdTraits = fromList [T.Armor]
  , cdKeywords = fromList []
  , cdCardType = AttachmentType, cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False, cdCriteria = C.NoCriteria
  , cdAspect = Nothing
  , cdResources = []
  , cdBoostIcons = [B.Boost, B.Boost]
  , cdEncounterSet = Just S.JubileeNemesis
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False
  , cdDoubleWildResources = False, cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = Cost.NoCost
  }

jubilee47027 :: CardDef
jubilee47027 = CardDef
  { cdCardCode = "47027", cdName = Name "\"Lost\" Child" (Nothing)
  , cdCost = Nothing
  , cdTraits = fromList [T.Armor]
  , cdKeywords = fromList []
  , cdCardType = AttachmentType, cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False, cdCriteria = C.NoCriteria
  , cdAspect = Nothing
  , cdResources = []
  , cdBoostIcons = [B.Boost]
  , cdEncounterSet = Just S.JubileeNemesis
  , cdEncounterSetQuantity = Just 2
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False
  , cdDoubleWildResources = False, cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = Cost.NoCost
  }

jubilee47028 :: CardDef
jubilee47028 = CardDef
  { cdCardCode = "47028", cdName = Name "Mutant Mayhem" (Nothing)
  , cdCost = Just 3
  , cdTraits = fromList []
  , cdKeywords = fromList [K.Alliance]
  , cdCardType = EventType, cdAbilityType = Just A.HeroAction
  , cdAbilitySubTypes = []
  , cdUnique = False, cdCriteria = C.NoCriteria
  , cdAspect = Just P.Leadership
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
  , cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = Cost.Costs [Cost.ReturnAnyAllyToHandCost (M.AllyWithTrait T.XForce), Cost.ReturnAnyAllyToHandCost (M.AllyWithTrait T.XMen)]
  }

jubilee47029 :: CardDef
jubilee47029 = CardDef
  { cdCardCode = "47029", cdName = Name "Serve and Protect" (Nothing)
  , cdCost = Just 2
  , cdTraits = fromList []
  , cdKeywords = fromList [K.Alliance]
  , cdCardType = EventType, cdAbilityType = Just A.HeroInterrupt
  , cdAbilitySubTypes = []
  , cdUnique = False, cdCriteria = C.NoCriteria
  , cdAspect = Just P.Protection
  , cdResources = [(PrintedResource, R.Mental)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Just (W.ThreatWouldBePlaced W.AnyThreatSource M.EncounterMainScheme), cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 3
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False
  , cdDoubleWildResources = False, cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = Cost.Costs [Cost.ExhaustCharacterWithTraitCost T.XForce, Cost.ExhaustCharacterWithTraitCost T.XMen]
  }

jubilee47030 :: CardDef
jubilee47030 = CardDef
  { cdCardCode = "47030", cdName = Name "Arcade" (Nothing)
  , cdCost = Nothing
  , cdTraits = fromList [T.Assassin]
  , cdKeywords = fromList []
  , cdCardType = MinionType, cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True, cdCriteria = C.NoCriteria
  , cdAspect = Nothing
  , cdResources = []
  , cdBoostIcons = [B.Boost, B.Boost, B.Boost]
  , cdEncounterSet = Just S.ArcadeSet
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False
  , cdDoubleWildResources = False, cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = Cost.NoCost
  }

jubilee47031 :: CardDef
jubilee47031 = CardDef
  { cdCardCode = "47031", cdName = Name "Welcome to Murderworld" (Nothing)
  , cdCost = Nothing
  , cdTraits = fromList [T.Trap]
  , cdKeywords = fromList [K.Hinder (V.PerPlayer 1)]
  , cdCardType = SideSchemeType, cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False, cdCriteria = C.NoCriteria
  , cdAspect = Nothing
  , cdResources = []
  , cdBoostIcons = [B.Boost, B.Boost]
  , cdEncounterSet = Just S.ArcadeSet
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False
  , cdDoubleWildResources = False, cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = Cost.NoCost
  }

jubilee47032 :: CardDef
jubilee47032 = CardDef
  { cdCardCode = "47032", cdName = Name "Arcade's Funhouse" (Nothing)
  , cdCost = Nothing
  , cdTraits = fromList [T.Trap]
  , cdKeywords = fromList [K.Hinder (V.PerPlayer 1)]
  , cdCardType = SideSchemeType, cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False, cdCriteria = C.NoCriteria
  , cdAspect = Nothing
  , cdResources = []
  , cdBoostIcons = [B.Boost, B.Boost]
  , cdEncounterSet = Just S.ArcadeSet
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False
  , cdDoubleWildResources = False, cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = Cost.NoCost
  }

jubilee47033 :: CardDef
jubilee47033 = CardDef
  { cdCardCode = "47033", cdName = Name "Hall of Mirrors" (Nothing)
  , cdCost = Nothing
  , cdTraits = fromList [T.Trap]
  , cdKeywords = fromList [K.Hinder (V.PerPlayer 1)]
  , cdCardType = SideSchemeType, cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False, cdCriteria = C.NoCriteria
  , cdAspect = Nothing
  , cdResources = []
  , cdBoostIcons = [B.Boost, B.Boost]
  , cdEncounterSet = Just S.ArcadeSet
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False
  , cdDoubleWildResources = False, cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = Cost.NoCost
  }

jubilee47034 :: CardDef
jubilee47034 = CardDef
  { cdCardCode = "47034", cdName = Name "Elaborate Trap" (Nothing)
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = TreacheryType, cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False, cdCriteria = C.NoCriteria
  , cdAspect = Nothing
  , cdResources = []
  , cdBoostIcons = [B.Boost]
  , cdEncounterSet = Just S.ArcadeSet
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False
  , cdDoubleWildResources = False, cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = Cost.NoCost
  }

jubileeTeamUp :: Text -> Text -> C.Criteria
jubileeTeamUp a b = inPlay a <> inPlay b
 where
  inPlay name = C.AnyCriteria [C.CharacterExists (M.IdentityCharacters $ M.IdentityWithTitle name <> M.HeroIdentity), C.AllyExists (M.AllyNamedAs name)]
