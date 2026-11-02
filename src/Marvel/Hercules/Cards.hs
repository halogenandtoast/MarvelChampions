module Marvel.Hercules.Cards where
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
import Marvel.GameValue qualified as V
import Marvel.Aspect qualified as P
import Marvel.Boost qualified as B
import Marvel.Matchers qualified as M

hc59001a :: CardDef
hc59001a = CardDef
  { cdCardCode = "59001a", cdName = Name "Hercules" (Nothing)
  , cdCost = Nothing
  , cdTraits = fromList [T.Avenger, T.Olympus]
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

hc59001b :: CardDef
hc59001b = CardDef
  { cdCardCode = "59001b", cdName = Name "Hercules" (Nothing)
  , cdCost = Nothing
  , cdTraits = fromList [T.Olympus]
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

hc59002 :: CardDef
hc59002 = CardDef
  { cdCardCode = "59002", cdName = Name "Defeat the Hydra" (Nothing)
  , cdCost = Nothing
  , cdTraits = fromList [T.Labor]
  , cdKeywords = fromList [K.Victory 0]
  , cdCardType = AttachmentType, cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False, cdCriteria = C.NoCriteria
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

hc59003 :: CardDef
hc59003 = CardDef
  { cdCardCode = "59003", cdName = Name "Embody Pathos" (Nothing)
  , cdCost = Nothing
  , cdTraits = fromList [T.Labor]
  , cdKeywords = fromList [K.Victory 0]
  , cdCardType = AttachmentType, cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False, cdCriteria = C.NoCriteria
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

hc59004 :: CardDef
hc59004 = CardDef
  { cdCardCode = "59004", cdName = Name "Protect Humanity" (Nothing)
  , cdCost = Nothing
  , cdTraits = fromList [T.Labor]
  , cdKeywords = fromList [K.Victory 0]
  , cdCardType = ObligationType, cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False, cdCriteria = C.NoCriteria
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

hc59005 :: CardDef
hc59005 = CardDef
  { cdCardCode = "59005", cdName = Name "Nemean Lion Skin" (Nothing)
  , cdCost = Nothing
  , cdTraits = fromList [T.Artifact, T.Gift]
  , cdKeywords = fromList [K.Permanent]
  , cdCardType = UpgradeType, cdAbilityType = Nothing
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

hc59006 :: CardDef
hc59006 = CardDef
  { cdCardCode = "59006", cdName = Name "Shield of Perseus" (Nothing)
  , cdCost = Nothing
  , cdTraits = fromList [T.Artifact, T.Gift]
  , cdKeywords = fromList [K.Permanent]
  , cdCardType = UpgradeType, cdAbilityType = Nothing
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

hc59007 :: CardDef
hc59007 = CardDef
  { cdCardCode = "59007", cdName = Name "Sword of Peleus" (Nothing)
  , cdCost = Nothing
  , cdTraits = fromList [T.Artifact, T.Gift, T.Weapon]
  , cdKeywords = fromList [K.Permanent, K.Restricted]
  , cdCardType = UpgradeType, cdAbilityType = Nothing
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

hc59008 :: CardDef
hc59008 = CardDef
  { cdCardCode = "59008", cdName = Name "Amadeus Cho" (Nothing)
  , cdCost = Just 2
  , cdTraits = fromList [T.Genius]
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

hc59009 :: CardDef
hc59009 = CardDef
  { cdCardCode = "59009", cdName = Name "The Gift of Battle" (Nothing)
  , cdCost = Just 3
  , cdTraits = fromList [T.Attack]
  , cdKeywords = fromList []
  , cdCardType = EventType, cdAbilityType = Just A.HeroAction
  , cdAbilitySubTypes = [A.Attack]
  , cdUnique = False, cdCriteria = C.canAttack
  , cdAspect = Nothing
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

hc59010 :: CardDef
hc59010 = CardDef
  { cdCardCode = "59010", cdName = Name "Son of Zeus" (Nothing)
  , cdCost = Just 2
  , cdTraits = fromList [T.Olympus]
  , cdKeywords = fromList []
  , cdCardType = EventType, cdAbilityType = Just A.HeroAction
  , cdAbilitySubTypes = []
  , cdUnique = False, cdCriteria = C.AnyCriteria [C.SelfMatches M.IdentityCanReady, C.EffectiveHerculesGiftCount (M.AtLeast $ V.Static 1) <> C.UpgradeExists (M.UpgradeControlledBy M.You <> M.UpgradeWithCard HeroSpecificCard <> M.NotUpgrade M.UnexhaustedUpgrade), C.EffectiveHerculesGiftCount (M.AtLeast $ V.Static 2) <> C.SelfMatches M.IdentityWithoutToughStatus, C.EffectiveHerculesGiftCount (M.AtLeast $ V.Static 3)]
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

hc59011 :: CardDef
hc59011 = CardDef
  { cdCardCode = "59011", cdName = Name "Wisdom of Athena" (Nothing)
  , cdCost = Just 3
  , cdTraits = fromList [T.Thwart]
  , cdKeywords = fromList []
  , cdCardType = EventType, cdAbilityType = Just A.HeroAction
  , cdAbilitySubTypes = [A.Thwart]
  , cdUnique = False, cdCriteria = C.AnyCriteria [C.SelfMatches M.ConfusedIdentity, C.SchemeExists M.SchemeYouCanThwart]
  , cdAspect = Nothing
  , cdResources = [(PrintedResource, R.Mental)]
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

hc59012 :: CardDef
hc59012 = CardDef
  { cdCardCode = "59012", cdName = Name "Olympus" (Nothing)
  , cdCost = Just 0
  , cdTraits = fromList [T.Location, T.Olympus]
  , cdKeywords = fromList []
  , cdCardType = SupportType, cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True, cdCriteria = C.NoCriteria
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

hc59013 :: CardDef
hc59013 = CardDef
  { cdCardCode = "59013", cdName = Name "Gauntlets of Hercules" (Nothing)
  , cdCost = Just 0
  , cdTraits = fromList [T.Armor, T.Olympus]
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

hc59014 :: CardDef
hc59014 = CardDef
  { cdCardCode = "59014", cdName = Name "Golden Mace" (Nothing)
  , cdCost = Just 1
  , cdTraits = fromList [T.Olympus, T.Weapon]
  , cdKeywords = fromList [K.Restricted]
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

hc59015 :: CardDef
hc59015 = CardDef
  { cdCardCode = "59015", cdName = Name "Herc's Helm" (Nothing)
  , cdCost = Just 0
  , cdTraits = fromList [T.Armor, T.Olympus]
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

hc59016 :: CardDef
hc59016 = CardDef
  { cdCardCode = "59016", cdName = Name "Lion of Olympus" (Nothing)
  , cdCost = Just 0
  , cdTraits = fromList [T.Olympus]
  , cdKeywords = fromList [K.Temporary]
  , cdCardType = UpgradeType, cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False, cdCriteria = C.NoCriteria
  , cdAspect = Nothing
  , cdResources = [(PrintedResource, R.Physical)]
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

hc59017 :: CardDef
hc59017 = CardDef
  { cdCardCode = "59017", cdName = Name "Prince of Power" (Nothing)
  , cdCost = Just 1
  , cdTraits = fromList [T.Olympus, T.Title]
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

hc59018 :: CardDef
hc59018 = CardDef
  { cdCardCode = "59018", cdName = Name "Deathcry" (Just "Sharra Neramani")
  , cdCost = Just 3
  , cdTraits = fromList [T.Avenger]
  , cdKeywords = fromList []
  , cdCardType = AllyType, cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True, cdCriteria = C.NoCriteria
  , cdAspect = Just P.Leadership
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

hc59019 :: CardDef
hc59019 = CardDef
  { cdCardCode = "59019", cdName = Name "Namora" (Just "Aquaria Neptunia")
  , cdCost = Just 4
  , cdTraits = fromList [T.AgentOfAtlas, T.Atlantis]
  , cdKeywords = fromList []
  , cdCardType = AllyType, cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True, cdCriteria = C.NoCriteria
  , cdAspect = Just P.Leadership
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

hc59020 :: CardDef
hc59020 = CardDef
  { cdCardCode = "59020", cdName = Name "Thor" (Just "Odinson")
  , cdCost = Just 4
  , cdTraits = fromList [T.Asgard, T.Avenger]
  , cdKeywords = fromList []
  , cdCardType = AllyType, cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True, cdCriteria = C.NoCriteria
  , cdAspect = Just P.Leadership
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

hc59023 :: CardDef
hc59023 = CardDef
  { cdCardCode = "59023", cdName = Name "Recruitment Drive" (Nothing)
  , cdCost = Just 3
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = SupportType, cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False, cdCriteria = C.InAlterEgoForm
  , cdAspect = Just P.Leadership
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

hc59024 :: CardDef
hc59024 = CardDef
  { cdCardCode = "59024", cdName = Name "\"Avenge Me!\"" (Nothing)
  , cdCost = Just 0
  , cdTraits = fromList [T.Avenger]
  , cdKeywords = fromList []
  , cdCardType = UpgradeType, cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False, cdCriteria = C.SelfMatches (M.IdentityWithTrait T.Avenger)
  , cdAspect = Just P.Leadership
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

hc59025 :: CardDef
hc59025 = CardDef
  { cdCardCode = "59025", cdName = Name "Gilgamesh" (Nothing)
  , cdCost = Just 3
  , cdTraits = fromList [T.Eternal]
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
  , cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = Cost.ConfuseSelfUnlessTraitCost T.Eternal
  }

hc59026 :: CardDef
hc59026 = CardDef
  { cdCardCode = "59026", cdName = Name "Ancient Rivalry" (Nothing)
  , cdCost = Just 1
  , cdTraits = fromList [T.Asgard, T.Olympus]
  , cdKeywords = fromList []
  , cdCardType = EventType, cdAbilityType = Just A.HeroAction
  , cdAbilitySubTypes = []
  , cdUnique = False, cdCriteria = C.AnyCriteria [C.CharacterExists (M.IdentityCharacters $ M.IdentityWithTitle "Hercules" <> M.HeroIdentity), C.AllyExists (M.AllyNamedAs "Hercules")] <> C.AnyCriteria [C.CharacterExists (M.IdentityCharacters $ M.IdentityWithTitle "Thor" <> M.HeroIdentity), C.AllyExists (M.AllyNamedAs "Thor")]
  , cdAspect = Just P.Neutral
  , cdResources = [(PrintedResource, R.Wild)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 1
  , cdTeamUp = Just ("Hercules", "Thor")
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False
  , cdDoubleWildResources = False, cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = Cost.NoCost
  }

hc59028 :: CardDef
hc59028 = CardDef
  { cdCardCode = "59028", cdName = Name "Evaluate Threat" (Nothing)
  , cdCost = Just 1
  , cdTraits = fromList []
  , cdKeywords = fromList [K.Victory 0]
  , cdCardType = PlayerSideSchemeType, cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True, cdCriteria = C.AnyCriteria [C.SelfMatches (M.IdentityWithTrait T.Avenger), C.SelfMatches (M.IdentityWithTrait T.Shield)]
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

hc59035 :: CardDef
hc59035 = CardDef
  { cdCardCode = "59035", cdName = Name "Appeal to Athena" (Nothing)
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

hc59036 :: CardDef
hc59036 = CardDef
  { cdCardCode = "59036", cdName = Name "Ares" (Nothing)
  , cdCost = Nothing
  , cdTraits = fromList [T.Elite, T.Olympus]
  , cdKeywords = fromList [K.Retaliate 1]
  , cdCardType = MinionType, cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True, cdCriteria = C.NoCriteria
  , cdAspect = Nothing
  , cdResources = []
  , cdBoostIcons = [B.Boost, B.Boost]
  , cdEncounterSet = Just S.HerculesNemesisSet
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 3
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False
  , cdDoubleWildResources = False, cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = Cost.NoCost
  }

hc59037 :: CardDef
hc59037 = CardDef
  { cdCardCode = "59037", cdName = Name "Lernean Hydra" (Nothing)
  , cdCost = Nothing
  , cdTraits = fromList [T.Creature, T.Hydra]
  , cdKeywords = fromList []
  , cdCardType = MinionType, cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True, cdCriteria = C.NoCriteria
  , cdAspect = Nothing
  , cdResources = []
  , cdBoostIcons = [B.Boost]
  , cdEncounterSet = Just S.HerculesNemesisSet
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 3
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False
  , cdDoubleWildResources = False, cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = Cost.NoCost
  }

hc59038 :: CardDef
hc59038 = CardDef
  { cdCardCode = "59038", cdName = Name "Olympic Feud" (Nothing)
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = SideSchemeType, cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False, cdCriteria = C.NoCriteria
  , cdAspect = Nothing
  , cdResources = []
  , cdBoostIcons = [B.Boost, B.Boost, B.Boost]
  , cdEncounterSet = Just S.HerculesNemesisSet
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 1, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 3
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False
  , cdDoubleWildResources = False, cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = Cost.NoCost
  }

hc59039 :: CardDef
hc59039 = CardDef
  { cdCardCode = "59039", cdName = Name "Ares's Axe" (Nothing)
  , cdCost = Nothing
  , cdTraits = fromList [T.Weapon]
  , cdKeywords = fromList []
  , cdCardType = AttachmentType, cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False, cdCriteria = C.NoCriteria
  , cdAspect = Nothing
  , cdResources = []
  , cdBoostIcons = [B.Boost, B.Boost]
  , cdEncounterSet = Just S.HerculesNemesisSet
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 3
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False
  , cdDoubleWildResources = False, cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = Cost.NoCost
  }

hc59040 :: CardDef
hc59040 = CardDef
  { cdCardCode = "59040", cdName = Name "God of War" (Nothing)
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = TreacheryType, cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False, cdCriteria = C.NoCriteria
  , cdAspect = Nothing
  , cdResources = []
  , cdBoostIcons = [B.Boost, B.Boost, B.Boost]
  , cdEncounterSet = Just S.HerculesNemesisSet
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 3
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False
  , cdDoubleWildResources = False, cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = Cost.NoCost
  }

hc59041 :: CardDef
hc59041 = CardDef
  { cdCardCode = "59041", cdName = Name "All Versus All" (Nothing)
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList [K.Hinder (V.PerPlayer 2)]
  , cdCardType = SideSchemeType, cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False, cdCriteria = C.NoCriteria
  , cdAspect = Nothing
  , cdResources = []
  , cdBoostIcons = [B.Boost, B.Boost]
  , cdEncounterSet = Just S.AllVersusAllSet
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 1, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 3
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False
  , cdDoubleWildResources = False, cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = Cost.NoCost
  }

hc59042 :: CardDef
hc59042 = CardDef
  { cdCardCode = "59042", cdName = Name "Hecate" (Nothing)
  , cdCost = Nothing
  , cdTraits = fromList [T.Olympus]
  , cdKeywords = fromList []
  , cdCardType = MinionType, cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True, cdCriteria = C.NoCriteria
  , cdAspect = Nothing
  , cdResources = []
  , cdBoostIcons = [B.Boost, B.Boost]
  , cdEncounterSet = Just S.AllVersusAllSet
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 3
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False
  , cdDoubleWildResources = False, cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = Cost.NoCost
  }

hc59043 :: CardDef
hc59043 = CardDef
  { cdCardCode = "59043", cdName = Name "Kyknos" (Nothing)
  , cdCost = Nothing
  , cdTraits = fromList [T.Brute, T.Olympus]
  , cdKeywords = fromList [K.Toughness, K.Retaliate 1, K.Victory 0]
  , cdCardType = MinionType, cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True, cdCriteria = C.NoCriteria
  , cdAspect = Nothing
  , cdResources = []
  , cdBoostIcons = [B.Boost, B.Boost, B.Boost]
  , cdEncounterSet = Just S.AllVersusAllSet
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 3
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False
  , cdDoubleWildResources = False, cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = Cost.NoCost
  }

hc59044 :: CardDef
hc59044 = CardDef
  { cdCardCode = "59044", cdName = Name "Bewitched Officer" (Nothing)
  , cdCost = Nothing
  , cdTraits = fromList [T.Aerial, T.Dragon, T.Enthralled]
  , cdKeywords = fromList [K.Retaliate 1]
  , cdCardType = MinionType, cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False, cdCriteria = C.NoCriteria
  , cdAspect = Nothing
  , cdResources = []
  , cdBoostIcons = [B.Boost, B.Boost]
  , cdEncounterSet = Just S.AllVersusAllSet
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 3
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False
  , cdDoubleWildResources = False, cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = Cost.NoCost
  }

hc59045 :: CardDef
hc59045 = CardDef
  { cdCardCode = "59045", cdName = Name "Roving Mobs" (Nothing)
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList [K.Victory 0]
  , cdCardType = SideSchemeType, cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False, cdCriteria = C.NoCriteria
  , cdAspect = Nothing
  , cdResources = []
  , cdBoostIcons = [B.Boost, B.Boost]
  , cdEncounterSet = Just S.AllVersusAllSet
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 3
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False
  , cdDoubleWildResources = False, cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = Cost.NoCost
  }
