module Marvel.Silk.Cards where
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

silkHero :: CardDef
silkHero = CardDef
  { cdCardCode = "52001a"
  , cdName = "Silk"
  , cdCost = Nothing
  , cdTraits = fromList [T.WebWarrior]
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
  , cdDeckLimit = Just 3
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = Cost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

cindyMoon :: CardDef
cindyMoon = CardDef
  { cdCardCode = "52001b"
  , cdName = "Cindy Moon"
  , cdCost = Nothing
  , cdTraits = fromList [T.Genius]
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
  , cdDeckLimit = Just 3
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = Cost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

smoothasSilkSilk :: CardDef
smoothasSilkSilk = CardDef
  { cdCardCode = "52002"
  , cdName = "Smooth as Silk"
  , cdCost = Just 0
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = EventType
  , cdAbilityType = Just A.HeroAction
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
  , cdDeckLimit = Just 2
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = Cost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

swingingSilkKickSilk :: CardDef
swingingSilkKickSilk = CardDef
  { cdCardCode = "52003"
  , cdName = "Swinging Silk Kick"
  , cdCost = Just 3
  , cdTraits = fromList [T.Aerial, T.Attack, T.Superpower]
  , cdKeywords = fromList []
  , cdCardType = EventType
  , cdAbilityType = Just A.HeroAction
  , cdAbilitySubTypes = [A.Attack]
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = canAttack
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
  , cdDeckLimit = Just 3
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = Cost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

wallcrawlSilk :: CardDef
wallcrawlSilk = CardDef
  { cdCardCode = "52004"
  , cdName = "Wallcrawl"
  , cdCost = Just 1
  , cdTraits = fromList [T.Superpower, T.Thwart]
  , cdKeywords = fromList []
  , cdCardType = EventType
  , cdAbilityType = Just A.HeroAction
  , cdAbilitySubTypes = [A.Thwart]
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = SchemeExists ThwartableScheme
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
  , cdDeckLimit = Just 2
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = Cost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

gettheScoopSilk :: CardDef
gettheScoopSilk = CardDef
  { cdCardCode = "52005"
  , cdName = "Get the Scoop"
  , cdCost = Just 0
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = PlayerSideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = [(PrintedResource, R.Wild)]
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

albertMoonSilk :: CardDef
albertMoonSilk = CardDef
  { cdCardCode = "52006"
  , cdName = "Albert Moon"
  , cdCost = Just 2
  , cdTraits = fromList [T.Persona]
  , cdKeywords = fromList []
  , cdCardType = SupportType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
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

jJonahJamesonSilk :: CardDef
jJonahJamesonSilk = CardDef
  { cdCardCode = "52007"
  , cdName = "J. Jonah Jameson"
  , cdCost = Just 3
  , cdTraits = fromList [T.Persona]
  , cdKeywords = fromList []
  , cdCardType = SupportType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
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

eideticMemorySilk :: CardDef
eideticMemorySilk = CardDef
  { cdCardCode = "52008"
  , cdName = "Eidetic Memory"
  , cdCost = Just 1
  , cdTraits = fromList [T.Superpower]
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

organicWebbingSilk :: CardDef
organicWebbingSilk = CardDef
  { cdCardCode = "52009"
  , cdName = "Organic Webbing"
  , cdCost = Just 2
  , cdTraits = fromList [T.Superpower]
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

outwitSilk :: CardDef
outwitSilk = CardDef
  { cdCardCode = "52010"
  , cdName = "Outwit"
  , cdCost = Just 2
  , cdTraits = fromList [T.Skill]
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

spiderClawsSilk :: CardDef
spiderClawsSilk = CardDef
  { cdCardCode = "52011"
  , cdName = "Spider Claws"
  , cdCost = Just 2
  , cdTraits = fromList [T.Weapon]
  , cdKeywords = fromList []
  , cdCardType = UpgradeType
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

spiderReflexesSilk :: CardDef
spiderReflexesSilk = CardDef
  { cdCardCode = "52012"
  , cdName = "Spider Reflexes"
  , cdCost = Just 2
  , cdTraits = fromList [T.Superpower]
  , cdKeywords = fromList []
  , cdCardType = UpgradeType
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

scarletSpiderSilk :: CardDef
scarletSpiderSilk = CardDef
  { cdCardCode = "52013"
  , cdName = Name "Scarlet Spider" (Just "Ben Reilly")
  , cdCost = Just 3
  , cdTraits = fromList [T.WebWarrior]
  , cdKeywords = fromList []
  , cdCardType = AllyType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Just Aspect.Protection
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

spiderByteSilk :: CardDef
spiderByteSilk = CardDef
  { cdCardCode = "52014"
  , cdName = Name "Spider-Byte" (Just "Margo Kess")
  , cdCost = Just 3
  , cdTraits = fromList [T.WebWarrior]
  , cdKeywords = fromList []
  , cdCardType = AllyType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Just Aspect.Protection
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

stopHittingYourselfSilk :: CardDef
stopHittingYourselfSilk = CardDef
  { cdCardCode = "52016"
  , cdName = "\"Stop Hitting Yourself\""
  , cdCost = Just 2
  , cdTraits = fromList [T.Attack]
  , cdKeywords = fromList []
  , cdCardType = EventType
  , cdAbilityType = Just A.HeroResponse
  , cdAbilitySubTypes = [A.Attack]
  , cdUnique = False
  , cdAspect = Just Aspect.Protection
  , cdCriteria = canAttack
  , cdResources = [(PrintedResource, R.Physical)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Just (W.HeroDefendedWithoutDamageAgainst W.After You AnyEnemy)
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

drSinclairSilk :: CardDef
drSinclairSilk = CardDef
  { cdCardCode = "52017"
  , cdName = "Dr. Sinclair"
  , cdCost = Just 2
  , cdTraits = fromList [T.Persona, T.Therapist]
  , cdKeywords = fromList []
  , cdCardType = SupportType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Just Aspect.Protection
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

energyShieldSilk :: CardDef
energyShieldSilk = CardDef
  { cdCardCode = "52018"
  , cdName = "Energy Shield"
  , cdCost = Just 0
  , cdTraits = fromList [T.Item, T.Tech]
  , cdKeywords = fromList []
  , cdCardType = UpgradeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Just Aspect.Protection
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
  , cdLimit = Just (MaxPerCharacter 1)
  , cdDeckLimit = Just 3
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = Cost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

readyforaFightSilk :: CardDef
readyforaFightSilk = CardDef
  { cdCardCode = "52019"
  , cdName = "Ready for a Fight"
  , cdCost = Just 1
  , cdTraits = fromList [T.Preparation]
  , cdKeywords = fromList [K.Requirement [R.Physical]]
  , cdCardType = UpgradeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Just Aspect.Protection
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
  , cdLimit = Just (MaxPerPlayer 1)
  , cdDeckLimit = Just 3
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = Cost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

stunGunSilk :: CardDef
stunGunSilk = CardDef
  { cdCardCode = "52020"
  , cdName = "Stun Gun"
  , cdCost = Just 2
  , cdTraits = fromList [T.Tech, T.Weapon]
  , cdKeywords = fromList [K.Restricted]
  , cdCardType = UpgradeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Just Aspect.Protection
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
  , cdDeckLimit = Just 3
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = Cost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

madameWebSilk :: CardDef
madameWebSilk = CardDef
  { cdCardCode = "52021"
  , cdName = Name "Madame Web" (Just "Julia Carpenter")
  , cdCost = Just 3
  , cdTraits = fromList [T.WebWarrior]
  , cdKeywords = fromList []
  , cdCardType = AllyType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Just Aspect.Neutral
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

investigativeJournalismSilk :: CardDef
investigativeJournalismSilk = CardDef
  { cdCardCode = "52024"
  , cdName = "Investigative Journalism"
  , cdCost = Just 2
  , cdTraits = fromList [T.Skill]
  , cdKeywords = fromList []
  , cdCardType = EventType
  , cdAbilityType = Just A.AlterEgoInterrupt
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Just Aspect.Neutral
  , cdCriteria = AnyCriteria [SelfMatches (IdentityWithTitle "Cindy Moon"), SelfMatches (IdentityWithTitle "Peter Parker")] <> AnyCriteria [AllyExists (AllyNamedAs "Peter Parker"), CharacterExists (IdentityCharacters (IdentityWithTitle "Peter Parker"))] <> AnyCriteria [AllyExists (AllyNamedAs "Cindy Moon"), CharacterExists (IdentityCharacters (IdentityWithTitle "Cindy Moon"))]
  , cdResources = [(PrintedResource, R.Wild)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Just (W.EnemyWouldScheme AnyEnemy AnyIdentity)
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 1
  , cdTeamUp = Just ("Cindy Moon", "Peter Parker")
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = Cost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

silkSenseOverloadSilk :: CardDef
silkSenseOverloadSilk = CardDef
  { cdCardCode = "52028"
  , cdName = "Silk Sense Overload"
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

morlunSilk :: CardDef
morlunSilk = CardDef
  { cdCardCode = "52029"
  , cdName = "Morlun"
  , cdCost = Nothing
  , cdTraits = fromList [T.Elite, T.Inheritor]
  , cdKeywords = fromList []
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [B.Boost, B.Boost, B.Boost]
  , cdEncounterSet = Just Set.SilkNemesis
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

theGreatHuntSilk :: CardDef
theGreatHuntSilk = CardDef
  { cdCardCode = "52030"
  , cdName = "The Great Hunt"
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
  , cdBoostIcons = [B.Boost, B.Boost]
  , cdEncounterSet = Just Set.SilkNemesis
  , cdEncounterSetQuantity = Just 1
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
  , cdAbilityCost = Cost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

huntingtheSpiderBrideSilk :: CardDef
huntingtheSpiderBrideSilk = CardDef
  { cdCardCode = "52031"
  , cdName = "Hunting the Spider-Bride"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList [K.Surge]
  , cdCardType = TreacheryType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [B.Boost]
  , cdEncounterSet = Just Set.SilkNemesis
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

spiderMan2099Silk :: CardDef
spiderMan2099Silk = CardDef
  { cdCardCode = "52032"
  , cdName = Name "Spider-Man 2099" (Just "Miguel O'Hara")
  , cdCost = Just 4
  , cdTraits = fromList [T.Aerial, T.WebWarrior]
  , cdKeywords = fromList []
  , cdCardType = AllyType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Just Aspect.Leadership
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

spiderWomanSilk :: CardDef
spiderWomanSilk = CardDef
  { cdCardCode = "52033"
  , cdName = Name "Spider-Woman" (Just "Jessica Drew")
  , cdCost = Just 3
  , cdTraits = fromList [T.Shield, T.WebWarrior]
  , cdKeywords = fromList []
  , cdCardType = AllyType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Just Aspect.Aggression
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

quickQuipSilk :: CardDef
quickQuipSilk = CardDef
  { cdCardCode = "52034"
  , cdName = "Quick Quip"
  , cdCost = Just 2
  , cdTraits = fromList [T.Skill]
  , cdKeywords = fromList [K.Requirement [R.Mental]]
  , cdCardType = EventType
  , cdAbilityType = Just A.HeroAction
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Just Aspect.Justice
  , cdCriteria = EnemyExists EnemyCanBeConfused
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
  , cdAbilityCost = Cost.DamageCharacterCost (CharacterWithTrait T.WebWarrior <> CharacterControlledBy You) 1
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

atlasSilk :: CardDef
atlasSilk = CardDef
  { cdCardCode = "52035"
  , cdName = "Atlas"
  , cdCost = Nothing
  , cdTraits = fromList [T.Elite, T.Thunderbolt]
  , cdKeywords = fromList [K.Villainous, K.Victory 1]
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [B.Boost, B.Boost, B.Boost, B.Boost]
  , cdEncounterSet = Just Set.GrowingStrongSet
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

growInvulnerableSilk :: CardDef
growInvulnerableSilk = CardDef
  { cdCardCode = "52036"
  , cdName = "Grow Invulnerable"
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
  , cdEncounterSet = Just Set.GrowingStrongSet
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

growingStrongSilk :: CardDef
growingStrongSilk = CardDef
  { cdCardCode = "52037"
  , cdName = "Growing Strong"
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
  , cdBoostIcons = [B.Boost, B.Star]
  , cdEncounterSet = Just Set.GrowingStrongSet
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

titanicProportionsSilk :: CardDef
titanicProportionsSilk = CardDef
  { cdCardCode = "52038"
  , cdName = "Titanic Proportions"
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
  , cdBoostIcons = [B.Boost, B.Star]
  , cdEncounterSet = Just Set.GrowingStrongSet
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
