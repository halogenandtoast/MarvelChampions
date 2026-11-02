module Marvel.JessicaJones.Cards where
import Marvel.Prelude
import Marvel.Card.Def
import Marvel.FalconWinter.Cards qualified as Base
import Marvel.Trait qualified as T
import Marvel.Resource qualified as R
import Marvel.Keyword qualified as K
import Marvel.Aspect qualified as A
import Marvel.Ability.Types qualified as Ab
import Marvel.Boost qualified as B
import Marvel.EncounterSet qualified as Set
import Marvel.Cost.Types qualified
import Marvel.Criteria
import Marvel.Matchers
import Marvel.GameValue
import Marvel.Window qualified as W
import Marvel.Name ((<:>))

jl61001a :: CardDef
jl61001a = Base.fw53002
  { cdCardCode = "61001a"
  , cdName = "Jessica Jones"
  , cdCost = Nothing
  , cdTraits = fromList [T.Defender]
  , cdKeywords = fromList []
  , cdCardType = HeroType
  , cdUnique = True
  , cdAspect = Nothing
  , cdResources = []
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdBoostIcons = []
  , cdDeckLimit = Just 1
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = NoCriteria
  }

jl61001b :: CardDef
jl61001b = Base.fw53002
  { cdCardCode = "61001b"
  , cdName = "Jessica Jones"
  , cdCost = Nothing
  , cdTraits = fromList [T.Civilian]
  , cdKeywords = fromList []
  , cdCardType = AlterEgoType
  , cdUnique = True
  , cdAspect = Nothing
  , cdResources = []
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdBoostIcons = []
  , cdDeckLimit = Just 1
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = NoCriteria
  }

jl61002 :: CardDef
jl61002 = Base.fw53002
  { cdCardCode = "61002"
  , cdName = "Alias Investigations"
  , cdCost = Nothing
  , cdTraits = fromList [T.Location]
  , cdKeywords = fromList [K.Permanent]
  , cdCardType = SupportType
  , cdUnique = True
  , cdAspect = Nothing
  , cdResources = [(PrintedResource, R.Mental)]
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdBoostIcons = []
  , cdDeckLimit = Just 1
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = NoCriteria
  }

jl61003 :: CardDef
jl61003 = Base.fw53002
  { cdCardCode = "61003"
  , cdName = "Luke Cage"
  , cdCost = Just 3
  , cdTraits = fromList [T.Defender]
  , cdKeywords = fromList [K.Toughness]
  , cdCardType = AllyType
  , cdUnique = True
  , cdAspect = Nothing
  , cdResources = [(PrintedResource, R.Wild)]
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdBoostIcons = []
  , cdDeckLimit = Just 1
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = NoCriteria
  }

jl61004 :: CardDef
jl61004 = Base.fw53002
  { cdCardCode = "61004"
  , cdName = "\"Big Mistake\""
  , cdCost = Just 1
  , cdTraits = fromList [T.Attack]
  , cdKeywords = fromList []
  , cdCardType = EventType
  , cdUnique = False
  , cdAspect = Nothing
  , cdResources = [(PrintedResource, R.Physical)]
  , cdAbilityType = Just Ab.HeroAction
  , cdAbilitySubTypes = [Ab.Attack]
  , cdBoostIcons = []
  , cdDeckLimit = Nothing
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = EnemyExists AttackableEnemy
  }

jl61005 :: CardDef
jl61005 = Base.fw53002
  { cdCardCode = "61005"
  , cdName = "Breakthrough"
  , cdCost = Just 0
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = EventType
  , cdUnique = False
  , cdAspect = Nothing
  , cdResources = [(PrintedResource, R.Mental)]
  , cdAbilityType = Just Ab.Response
  , cdAbilitySubTypes = []
  , cdBoostIcons = []
  , cdDeckLimit = Nothing
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = NoCriteria
  , cdResponseWindow = Just (W.SupportCountersPlaced W.After (SupportNamed "Alias Investigations" <> SupportControlledBy You))
  }

jl61006 :: CardDef
jl61006 = Base.fw53002
  { cdCardCode = "61006"
  , cdName = "Snooping Around"
  , cdCost = Just 0
  , cdTraits = fromList [T.Thwart]
  , cdKeywords = fromList []
  , cdCardType = EventType
  , cdUnique = False
  , cdAspect = Nothing
  , cdResources = [(PrintedResource, R.Energy)]
  , cdAbilityType = Just Ab.AlterEgoAction
  , cdAbilitySubTypes = [Ab.Thwart]
  , cdBoostIcons = []
  , cdDeckLimit = Nothing
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = NoCriteria
  }

jl61007 :: CardDef
jl61007 = Base.fw53002
  { cdCardCode = "61007"
  , cdName = "Piecing It All Together"
  , cdCost = Just 0
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = PlayerSideSchemeType
  , cdUnique = True
  , cdAspect = Nothing
  , cdResources = [(PrintedResource, R.Mental)]
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdBoostIcons = []
  , cdDeckLimit = Just 1
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = NoCriteria
  }

jl61008 :: CardDef
jl61008 = Base.fw53002
  { cdCardCode = "61008"
  , cdName = "Calling in Favors"
  , cdCost = Just 1
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = SupportType
  , cdUnique = False
  , cdAspect = Nothing
  , cdResources = [(PrintedResource, R.Mental)]
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdBoostIcons = []
  , cdDeckLimit = Nothing
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = NoCriteria
  }

jl61009 :: CardDef
jl61009 = Base.fw53002
  { cdCardCode = "61009"
  , cdName = "4K Digital Camcorder"
  , cdCost = Just 2
  , cdTraits = fromList [T.Item]
  , cdKeywords = fromList []
  , cdCardType = UpgradeType
  , cdUnique = False
  , cdAspect = Nothing
  , cdResources = [(PrintedResource, R.Mental)]
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdBoostIcons = []
  , cdDeckLimit = Nothing
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = NoCriteria
  }

jl61010 :: CardDef
jl61010 = Base.fw53002
  { cdCardCode = "61010"
  , cdName = "Circumstantial Evidence"
  , cdCost = Just 1
  , cdTraits = fromList [T.Item]
  , cdKeywords = fromList []
  , cdCardType = UpgradeType
  , cdUnique = False
  , cdAspect = Nothing
  , cdResources = [(PrintedResource, R.Energy)]
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdBoostIcons = []
  , cdDeckLimit = Nothing
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = NoCriteria
  }

jl61011 :: CardDef
jl61011 = Base.fw53002
  { cdCardCode = "61011"
  , cdName = "Leather Jacket"
  , cdCost = Just 2
  , cdTraits = fromList [T.Item]
  , cdKeywords = fromList []
  , cdCardType = UpgradeType
  , cdUnique = False
  , cdAspect = Nothing
  , cdResources = [(PrintedResource, R.Physical)]
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdBoostIcons = []
  , cdDeckLimit = Nothing
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = NoCriteria
  }

jl61012 :: CardDef
jl61012 = Base.fw53002
  { cdCardCode = "61012"
  , cdName = "\"Now I'm Ticked Off!\""
  , cdCost = Just 2
  , cdTraits = fromList [T.Condition]
  , cdKeywords = fromList []
  , cdCardType = UpgradeType
  , cdUnique = False
  , cdAspect = Nothing
  , cdResources = [(PrintedResource, R.Physical)]
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = [Ab.Attack]
  , cdBoostIcons = []
  , cdDeckLimit = Nothing
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = NoCriteria
  }

jl61013 :: CardDef
jl61013 = Base.fw53002
  { cdCardCode = "61013"
  , cdName = "Reluctant Flier"
  , cdCost = Just 1
  , cdTraits = fromList [T.Aerial, T.Preparation]
  , cdKeywords = fromList []
  , cdCardType = UpgradeType
  , cdUnique = False
  , cdAspect = Nothing
  , cdResources = [(PrintedResource, R.Energy)]
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = [Ab.Defense]
  , cdBoostIcons = []
  , cdDeckLimit = Nothing
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = NoCriteria
  }

jl61014 :: CardDef
jl61014 = Base.fw53002
  { cdCardCode = "61014"
  , cdName = "Stakeout"
  , cdCost = Just 0
  , cdTraits = fromList [T.Tactic]
  , cdKeywords = fromList []
  , cdCardType = UpgradeType
  , cdUnique = False
  , cdAspect = Nothing
  , cdResources = [(PrintedResource, R.Mental)]
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdBoostIcons = []
  , cdDeckLimit = Nothing
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = NoCriteria
  }

jl61015 :: CardDef
jl61015 = Base.fw53002
  { cdCardCode = "61015"
  , cdName = ("Captain Marvel" <:> "Carol Danvers")
  , cdCost = Just 4
  , cdTraits = fromList [T.Aerial, T.Avenger]
  , cdKeywords = fromList []
  , cdCardType = AllyType
  , cdUnique = True
  , cdAspect = Just A.Justice
  , cdResources = [(PrintedResource, R.Energy)]
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdBoostIcons = []
  , cdDeckLimit = Just 1
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = NoCriteria
  }

jl61016 :: CardDef
jl61016 = Base.fw53002
  { cdCardCode = "61016"
  , cdName = ("Spider-Woman" <:> "Mattie Franklin")
  , cdCost = Just 4
  , cdTraits = fromList [T.WebWarrior]
  , cdKeywords = fromList []
  , cdCardType = AllyType
  , cdUnique = True
  , cdAspect = Just A.Justice
  , cdResources = [(PrintedResource, R.Physical)]
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdBoostIcons = []
  , cdDeckLimit = Just 1
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = NoCriteria
  }

jl61017 :: CardDef
jl61017 = Base.fw53002
  { cdCardCode = "61017"
  , cdName = ("Squirrel Girl" <:> "Doreen Green")
  , cdCost = Just 2
  , cdTraits = fromList [T.Avenger]
  , cdKeywords = fromList []
  , cdCardType = AllyType
  , cdUnique = True
  , cdAspect = Just A.Justice
  , cdResources = [(PrintedResource, R.Mental)]
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdBoostIcons = []
  , cdDeckLimit = Just 1
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = NoCriteria
  }

jl61019 :: CardDef
jl61019 = Base.fw53002
  { cdCardCode = "61019"
  , cdName = "Strategy Session"
  , cdCost = Just 1
  , cdTraits = fromList [T.Tactic, T.Thwart]
  , cdKeywords = fromList []
  , cdCardType = EventType
  , cdUnique = False
  , cdAspect = Just A.Justice
  , cdResources = [(PrintedResource, R.Energy)]
  , cdAbilityType = Just Ab.Action
  , cdAbilitySubTypes = [Ab.Thwart]
  , cdBoostIcons = []
  , cdDeckLimit = Nothing
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = NoCriteria
  }

jl61020 :: CardDef
jl61020 = Base.fw53002
  { cdCardCode = "61020"
  , cdName = "Run Them to Ground"
  , cdCost = Just 0
  , cdTraits = fromList []
  , cdKeywords = fromList [K.Victory 0]
  , cdCardType = PlayerSideSchemeType
  , cdUnique = True
  , cdAspect = Just A.Justice
  , cdResources = [(PrintedResource, R.Physical)]
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdBoostIcons = []
  , cdDeckLimit = Just 1
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = NoCriteria
  }

jl61023 :: CardDef
jl61023 = Base.fw53002
  { cdCardCode = "61023"
  , cdName = "Grapnel Launcher"
  , cdCost = Just 2
  , cdTraits = fromList [T.Item]
  , cdKeywords = fromList [K.Restricted]
  , cdCardType = UpgradeType
  , cdUnique = False
  , cdAspect = Just A.Justice
  , cdResources = [(PrintedResource, R.Energy)]
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdBoostIcons = []
  , cdDeckLimit = Nothing
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = NoCriteria
  }

jl61024 :: CardDef
jl61024 = Base.fw53002
  { cdCardCode = "61024"
  , cdName = "Entrapment"
  , cdCost = Just 1
  , cdTraits = fromList [T.Preparation]
  , cdKeywords = fromList []
  , cdCardType = UpgradeType
  , cdUnique = False
  , cdAspect = Just A.Justice
  , cdResources = [(PrintedResource, R.Mental)]
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = [Ab.Attack]
  , cdBoostIcons = []
  , cdDeckLimit = Nothing
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = NoCriteria
  , cdLimit = Just (MaxPerPlayer 1)
  }

jl61025 :: CardDef
jl61025 = Base.fw53002
  { cdCardCode = "61025"
  , cdName = ("Hellcat" <:> "Patsy Walker")
  , cdCost = Just 4
  , cdTraits = fromList [T.Defender, T.MartialArtist]
  , cdKeywords = fromList []
  , cdCardType = AllyType
  , cdUnique = True
  , cdAspect = Just A.Neutral
  , cdResources = [(PrintedResource, R.Physical)]
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdBoostIcons = []
  , cdDeckLimit = Just 1
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = NoCriteria
  }

jl61026 :: CardDef
jl61026 = Base.fw53002
  { cdCardCode = "61026"
  , cdName = "Joys of Life"
  , cdCost = Just 0
  , cdTraits = fromList []
  , cdKeywords = fromList [K.Alliance]
  , cdCardType = EventType
  , cdUnique = False
  , cdAspect = Just A.Neutral
  , cdResources = [(PrintedResource, R.Energy)]
  , cdAbilityType = Just Ab.Action
  , cdAbilitySubTypes = []
  , cdBoostIcons = []
  , cdDeckLimit = Nothing
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = AnyCriteria
      [ CharacterExists (IdentityCharacters (AlterEgoIdentity <> IdentityWithTrait T.Civilian <> UnexhaustedIdentity))
          <> CharacterExists (CharacterOneOf [IdentityCharacters HeroIdentity, AllyCharacters AnyAlly] <> CharacterCanBeReadied)
      , CharacterExists (CharacterOneOf [IdentityCharacters (HeroIdentity <> UnexhaustedIdentity), AllyCharacters UnexhaustedAlly])
          <> CharacterExists (IdentityCharacters (AlterEgoIdentity <> IdentityWithTrait T.Civilian) <> CharacterCanBeReadied)
      ]
  , cdAbilityCost = Marvel.Cost.Types.ExhaustCharacterCost (CharacterOneOf
      [ IdentityCharacters (AlterEgoIdentity <> IdentityWithTrait T.Civilian)
      , IdentityCharacters HeroIdentity
      , AllyCharacters AnyAlly
      ])
  }

jl61027 :: CardDef
jl61027 = Base.fw53002
  { cdCardCode = "61027"
  , cdName = "Unbreakable Bond"
  , cdCost = Just 1
  , cdTraits = fromList [T.Thwart]
  , cdKeywords = fromList []
  , cdCardType = EventType
  , cdUnique = False
  , cdAspect = Just A.Neutral
  , cdResources = [(PrintedResource, R.Physical)]
  , cdAbilityType = Just Ab.HeroAction
  , cdAbilitySubTypes = [Ab.Thwart]
  , cdBoostIcons = []
  , cdDeckLimit = Just 1
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = namedTeam "Jessica Jones" "Luke Cage"
  , cdAlternateCardCodes = ["62024"]
  , cdTeamUp = Just ("Jessica Jones", "Luke Cage")
  }

jl61028 :: CardDef
jl61028 = Base.fw53002
  { cdCardCode = "61028"
  , cdName = "Second Chance"
  , cdCost = Just 0
  , cdTraits = fromList []
  , cdKeywords = fromList [K.Victory 0]
  , cdCardType = PlayerSideSchemeType
  , cdUnique = True
  , cdAspect = Just A.Neutral
  , cdResources = [(PrintedResource, R.Mental)]
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdBoostIcons = []
  , cdDeckLimit = Just 1
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = NoCriteria
  }

jl61029 :: CardDef
jl61029 = Base.fw53002
  { cdCardCode = "61029"
  , cdName = "Defend Our City"
  , cdCost = Just 0
  , cdTraits = fromList []
  , cdKeywords = fromList [K.Victory 0, K.Hinder (PerPlayer 1)]
  , cdCardType = PlayerSideSchemeType
  , cdUnique = True
  , cdAspect = Just A.Neutral
  , cdResources = [(PrintedResource, R.Physical)]
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdBoostIcons = []
  , cdDeckLimit = Just 1
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = SelfMatches (IdentityWithTrait T.Defender)
  }

jl61030 :: CardDef
jl61030 = Base.fw53002
  { cdCardCode = "61030"
  , cdName = "Work-Life Balance"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = ObligationType
  , cdUnique = False
  , cdAspect = Nothing
  , cdResources = []
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdBoostIcons = [B.Boost, B.Boost]
  , cdDeckLimit = Nothing
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = NoCriteria
  }

jl61031 :: CardDef
jl61031 = Base.fw53002
  { cdCardCode = "61031"
  , cdName = "Purple Man"
  , cdCost = Nothing
  , cdTraits = fromList [T.Psionic]
  , cdKeywords = fromList []
  , cdCardType = MinionType
  , cdUnique = True
  , cdAspect = Nothing
  , cdResources = []
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdBoostIcons = [B.Boost, B.Boost, B.Boost]
  , cdDeckLimit = Just 1
  , cdEncounterSet = Just Set.JessicaJonesNemesis
  , cdEncounterSetQuantity = Just 1
  , cdCriteria = NoCriteria
  }

jl61032 :: CardDef
jl61032 = Base.fw53002
  { cdCardCode = "61032"
  , cdName = "Indomitable Will"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList [K.Hinder (PerPlayer 3)]
  , cdCardType = SideSchemeType
  , cdUnique = False
  , cdAspect = Nothing
  , cdResources = []
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdBoostIcons = [B.Boost]
  , cdDeckLimit = Nothing
  , cdEncounterSet = Just Set.JessicaJonesNemesis
  , cdEncounterSetQuantity = Just 1
  , cdCriteria = NoCriteria
  }

jl61033a :: CardDef
jl61033a = Base.fw53002
  { cdCardCode = "61033a"
  , cdName = "Suggestion ([energy])"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = ObligationType
  , cdUnique = False
  , cdAspect = Nothing
  , cdResources = []
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdBoostIcons = [B.Boost, B.Boost]
  , cdDeckLimit = Nothing
  , cdEncounterSet = Just Set.JessicaJonesNemesis
  , cdEncounterSetQuantity = Just 1
  , cdCriteria = NoCriteria
  }

jl61033b :: CardDef
jl61033b = Base.fw53002
  { cdCardCode = "61033b"
  , cdName = "Suggestion ([mental])"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = ObligationType
  , cdUnique = False
  , cdAspect = Nothing
  , cdResources = []
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdBoostIcons = [B.Boost, B.Boost]
  , cdDeckLimit = Nothing
  , cdEncounterSet = Just Set.JessicaJonesNemesis
  , cdEncounterSetQuantity = Just 1
  , cdCriteria = NoCriteria
  }

jl61033c :: CardDef
jl61033c = Base.fw53002
  { cdCardCode = "61033c"
  , cdName = "Suggestion ([physical])"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = ObligationType
  , cdUnique = False
  , cdAspect = Nothing
  , cdResources = []
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdBoostIcons = [B.Boost, B.Boost]
  , cdDeckLimit = Nothing
  , cdEncounterSet = Just Set.JessicaJonesNemesis
  , cdEncounterSetQuantity = Just 1
  , cdCriteria = NoCriteria
  }

jl61034 :: CardDef
jl61034 = Base.fw53002
  { cdCardCode = "61034"
  , cdName = "Innate Aggression"
  , cdCost = Just 2
  , cdTraits = fromList [T.Condition]
  , cdKeywords = fromList [K.Starting]
  , cdCardType = UpgradeType
  , cdUnique = False
  , cdAspect = Just A.Aggression
  , cdResources = []
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdBoostIcons = []
  , cdDeckLimit = Nothing
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = NoCriteria
  }

jl61035 :: CardDef
jl61035 = Base.fw53002
  { cdCardCode = "61035"
  , cdName = "Shakedown"
  , cdCost = Just 1
  , cdTraits = fromList [T.Tactic]
  , cdKeywords = fromList []
  , cdCardType = UpgradeType
  , cdUnique = False
  , cdAspect = Just A.Aggression
  , cdResources = [(PrintedResource, R.Physical)]
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdBoostIcons = []
  , cdDeckLimit = Nothing
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = NoCriteria
  , cdLimit = Just (MaxPerPlayerUnderAnyControl 1)
  }

jl61036 :: CardDef
jl61036 = Base.fw53002
  { cdCardCode = "61036"
  , cdName = "Innate Perception"
  , cdCost = Just 2
  , cdTraits = fromList [T.Condition]
  , cdKeywords = fromList [K.Starting]
  , cdCardType = UpgradeType
  , cdUnique = False
  , cdAspect = Just A.Justice
  , cdResources = []
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdBoostIcons = []
  , cdDeckLimit = Nothing
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = NoCriteria
  }

jl61037 :: CardDef
jl61037 = Base.fw53002
  { cdCardCode = "61037"
  , cdName = "Innate Inspiration"
  , cdCost = Just 2
  , cdTraits = fromList [T.Condition]
  , cdKeywords = fromList [K.Starting]
  , cdCardType = UpgradeType
  , cdUnique = False
  , cdAspect = Just A.Leadership
  , cdResources = []
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdBoostIcons = []
  , cdDeckLimit = Nothing
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = NoCriteria
  }

jl61038 :: CardDef
jl61038 = Base.fw53002
  { cdCardCode = "61038"
  , cdName = ("Echo" <:> "Maya Lopez")
  , cdCost = Just 3
  , cdTraits = fromList [T.Defender, T.MartialArtist]
  , cdKeywords = fromList []
  , cdCardType = AllyType
  , cdUnique = True
  , cdAspect = Just A.Protection
  , cdResources = [(PrintedResource, R.Physical)]
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdBoostIcons = []
  , cdDeckLimit = Just 1
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = NoCriteria
  }

jl61039 :: CardDef
jl61039 = Base.fw53002
  { cdCardCode = "61039"
  , cdName = "Lay Low"
  , cdCost = Just 0
  , cdTraits = fromList []
  , cdKeywords = fromList [K.Victory 0]
  , cdCardType = PlayerSideSchemeType
  , cdUnique = True
  , cdAspect = Just A.Protection
  , cdResources = [(PrintedResource, R.Energy)]
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdBoostIcons = []
  , cdDeckLimit = Just 1
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = NoCriteria
  }

jl61040 :: CardDef
jl61040 = Base.fw53002
  { cdCardCode = "61040"
  , cdName = "Mitigated Threat"
  , cdCost = Just 0
  , cdTraits = fromList [T.Condition]
  , cdKeywords = fromList []
  , cdCardType = UpgradeType
  , cdUnique = False
  , cdAspect = Just A.Protection
  , cdResources = [(PrintedResource, R.Physical)]
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdBoostIcons = []
  , cdDeckLimit = Nothing
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = NoCriteria
  }

jl62001a :: CardDef
jl62001a = Base.fw53002
  { cdCardCode = "62001a"
  , cdName = "Luke Cage"
  , cdCost = Nothing
  , cdTraits = fromList [T.Defender]
  , cdKeywords = fromList []
  , cdCardType = HeroType
  , cdUnique = True
  , cdAspect = Nothing
  , cdResources = []
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdBoostIcons = []
  , cdDeckLimit = Just 1
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = NoCriteria
  }

jl62001b :: CardDef
jl62001b = Base.fw53002
  { cdCardCode = "62001b"
  , cdName = "Luke Cage"
  , cdCost = Nothing
  , cdTraits = fromList [T.Civilian]
  , cdKeywords = fromList []
  , cdCardType = AlterEgoType
  , cdUnique = True
  , cdAspect = Nothing
  , cdResources = []
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdBoostIcons = []
  , cdDeckLimit = Just 1
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = NoCriteria
  }

jl62002 :: CardDef
jl62002 = Base.fw53002
  { cdCardCode = "62002"
  , cdName = "Jessica Jones"
  , cdCost = Just 3
  , cdTraits = fromList [T.Defender]
  , cdKeywords = fromList []
  , cdCardType = AllyType
  , cdUnique = True
  , cdAspect = Nothing
  , cdResources = [(PrintedResource, R.Wild)]
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdBoostIcons = []
  , cdDeckLimit = Just 1
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = NoCriteria
  }

jl62003 :: CardDef
jl62003 = Base.fw53002
  { cdCardCode = "62003"
  , cdName = "Harlem's Hero"
  , cdCost = Just 1
  , cdTraits = fromList [T.Skill]
  , cdKeywords = fromList []
  , cdCardType = EventType
  , cdUnique = False
  , cdAspect = Nothing
  , cdResources = [(PrintedResource, R.Physical)]
  , cdAbilityType = Just Ab.HeroAction
  , cdAbilitySubTypes = []
  , cdBoostIcons = []
  , cdDeckLimit = Nothing
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = NoCriteria
  }

jl62004 :: CardDef
jl62004 = Base.fw53002
  { cdCardCode = "62004"
  , cdName = "Knuckle Sandwich"
  , cdCost = Just 1
  , cdTraits = fromList [T.Attack]
  , cdKeywords = fromList []
  , cdCardType = EventType
  , cdUnique = False
  , cdAspect = Nothing
  , cdResources = [(PrintedResource, R.Physical)]
  , cdAbilityType = Just Ab.HeroAction
  , cdAbilitySubTypes = [Ab.Attack]
  , cdBoostIcons = []
  , cdDeckLimit = Nothing
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = EnemyExists AttackableEnemy
  }

jl62005 :: CardDef
jl62005 = Base.fw53002
  { cdCardCode = "62005"
  , cdName = "\"Stand with Me!\""
  , cdCost = Just 2
  , cdTraits = fromList [T.Thwart]
  , cdKeywords = fromList []
  , cdCardType = EventType
  , cdUnique = False
  , cdAspect = Nothing
  , cdResources = [(PrintedResource, R.Mental)]
  , cdAbilityType = Just Ab.HeroAction
  , cdAbilitySubTypes = [Ab.Thwart]
  , cdBoostIcons = []
  , cdDeckLimit = Nothing
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = NoCriteria
  }

jl62006 :: CardDef
jl62006 = Base.fw53002
  { cdCardCode = "62006"
  , cdName = "\"Sweet Christmas!\""
  , cdCost = Just 5
  , cdTraits = fromList [T.Attack]
  , cdKeywords = fromList []
  , cdCardType = EventType
  , cdUnique = False
  , cdAspect = Nothing
  , cdResources = [(PrintedResource, R.Energy)]
  , cdAbilityType = Just Ab.HeroAction
  , cdAbilitySubTypes = [Ab.Attack]
  , cdBoostIcons = []
  , cdDeckLimit = Nothing
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = EnemyExists AttackableEnemy
  }

jl62007 :: CardDef
jl62007 = Base.fw53002
  { cdCardCode = "62007"
  , cdName = "Fogwell's Gym"
  , cdCost = Just 1
  , cdTraits = fromList [T.Location]
  , cdKeywords = fromList []
  , cdCardType = SupportType
  , cdUnique = True
  , cdAspect = Nothing
  , cdResources = [(PrintedResource, R.Mental)]
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdBoostIcons = []
  , cdDeckLimit = Just 1
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = NoCriteria
  }

jl62008 :: CardDef
jl62008 = Base.fw53002
  { cdCardCode = "62008"
  , cdName = "Burstein Process"
  , cdCost = Just 2
  , cdTraits = fromList [T.Condition]
  , cdKeywords = fromList []
  , cdCardType = UpgradeType
  , cdUnique = True
  , cdAspect = Nothing
  , cdResources = [(PrintedResource, R.Mental)]
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdBoostIcons = []
  , cdDeckLimit = Just 1
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = NoCriteria
  }

jl62009 :: CardDef
jl62009 = Base.fw53002
  { cdCardCode = "62009"
  , cdName = "Cruisin' for a Bruisin'"
  , cdCost = Just 0
  , cdTraits = fromList [T.Condition]
  , cdKeywords = fromList []
  , cdCardType = UpgradeType
  , cdUnique = False
  , cdAspect = Nothing
  , cdResources = [(PrintedResource, R.Physical)]
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdBoostIcons = []
  , cdDeckLimit = Nothing
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = NoCriteria
  , cdLimit = Just (MaxPerEnemy 1)
  }

jl62010 :: CardDef
jl62010 = Base.fw53002
  { cdCardCode = "62010"
  , cdName = "Metal Bracer"
  , cdCost = Just 1
  , cdTraits = fromList [T.Armor, T.Item]
  , cdKeywords = fromList []
  , cdCardType = UpgradeType
  , cdUnique = False
  , cdAspect = Nothing
  , cdResources = [(PrintedResource, R.Mental)]
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdBoostIcons = []
  , cdDeckLimit = Nothing
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = NoCriteria
  }

jl62011 :: CardDef
jl62011 = Base.fw53002
  { cdCardCode = "62011"
  , cdName = "Power Man"
  , cdCost = Just 1
  , cdTraits = fromList [T.Title]
  , cdKeywords = fromList []
  , cdCardType = UpgradeType
  , cdUnique = True
  , cdAspect = Nothing
  , cdResources = [(PrintedResource, R.Physical)]
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdBoostIcons = []
  , cdDeckLimit = Just 1
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = NoCriteria
  }

jl62012 :: CardDef
jl62012 = Base.fw53002
  { cdCardCode = "62012"
  , cdName = ("Iron Fist" <:> "Danny Rand")
  , cdCost = Just 3
  , cdTraits = fromList [T.Defender, T.MartialArtist]
  , cdKeywords = fromList []
  , cdCardType = AllyType
  , cdUnique = True
  , cdAspect = Just A.Leadership
  , cdResources = [(PrintedResource, R.Mental)]
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdBoostIcons = []
  , cdDeckLimit = Just 1
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = NoCriteria
  }

jl62013 :: CardDef
jl62013 = Base.fw53002
  { cdCardCode = "62013"
  , cdName = "Misty Knight"
  , cdCost = Just 3
  , cdTraits = fromList [T.Defender, T.MartialArtist]
  , cdKeywords = fromList [K.Toughness]
  , cdCardType = AllyType
  , cdUnique = True
  , cdAspect = Just A.Leadership
  , cdResources = [(PrintedResource, R.Mental)]
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdBoostIcons = []
  , cdDeckLimit = Just 1
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = NoCriteria
  }

jl62014 :: CardDef
jl62014 = Base.fw53002
  { cdCardCode = "62014"
  , cdName = ("Valkyrie" <:> "Brunnhilde")
  , cdCost = Just 3
  , cdTraits = fromList [T.Asgard, T.Defender]
  , cdKeywords = fromList [K.Toughness]
  , cdCardType = AllyType
  , cdUnique = True
  , cdAspect = Just A.Leadership
  , cdResources = [(PrintedResource, R.Mental)]
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdBoostIcons = []
  , cdDeckLimit = Just 1
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = NoCriteria
  }

jl62015 :: CardDef
jl62015 = Base.fw53002
  { cdCardCode = "62015"
  , cdName = "Take a Stand"
  , cdCost = Just 1
  , cdTraits = fromList [T.Tactic]
  , cdKeywords = fromList []
  , cdCardType = EventType
  , cdUnique = False
  , cdAspect = Just A.Leadership
  , cdResources = [(PrintedResource, R.Energy)]
  , cdAbilityType = Just Ab.HeroResponse
  , cdAbilitySubTypes = []
  , cdBoostIcons = []
  , cdDeckLimit = Nothing
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = AllyExists (AllyControlledBy You)
  , cdResponseWindow = Just (W.HeroDefended W.After You AnyEnemy)
  }

jl62018 :: CardDef
jl62018 = Base.fw53002
  { cdCardCode = "62018"
  , cdName = "Defensive Formation"
  , cdCost = Just 2
  , cdTraits = fromList [T.Tactic]
  , cdKeywords = fromList []
  , cdCardType = SupportType
  , cdUnique = False
  , cdAspect = Just A.Leadership
  , cdResources = [(PrintedResource, R.Energy)]
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdBoostIcons = []
  , cdDeckLimit = Nothing
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = NoCriteria
  }

jl62019 :: CardDef
jl62019 = Base.fw53002
  { cdCardCode = "62019"
  , cdName = "Righteous Purpose"
  , cdCost = Just 2
  , cdTraits = fromList [T.Maxim]
  , cdKeywords = fromList []
  , cdCardType = SupportType
  , cdUnique = False
  , cdAspect = Just A.Leadership
  , cdResources = [(PrintedResource, R.Energy)]
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdBoostIcons = []
  , cdDeckLimit = Nothing
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = NoCriteria
  , cdLimit = Just (MaxPerPlayerUnderAnyControl 1)
  }

jl62020 :: CardDef
jl62020 = Base.fw53002
  { cdCardCode = "62020"
  , cdName = ("Hulk" <:> "Bruce Banner")
  , cdCost = Just 4
  , cdTraits = fromList [T.Defender, T.Gamma]
  , cdKeywords = fromList []
  , cdCardType = AllyType
  , cdUnique = True
  , cdAspect = Just A.Neutral
  , cdResources = [(PrintedResource, R.Physical)]
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdBoostIcons = []
  , cdDeckLimit = Just 1
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = NoCriteria
  }

jl62021 :: CardDef
jl62021 = Base.fw53002
  { cdCardCode = "62021"
  , cdName = ("She-Hulk" <:> "Jennifer Walters")
  , cdCost = Just 4
  , cdTraits = fromList [T.Defender, T.Gamma]
  , cdKeywords = fromList [K.Toughness]
  , cdCardType = AllyType
  , cdUnique = True
  , cdAspect = Just A.Neutral
  , cdResources = [(PrintedResource, R.Mental)]
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdBoostIcons = []
  , cdDeckLimit = Just 1
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = NoCriteria
  }

jl62022 :: CardDef
jl62022 = Base.fw53002
  { cdCardCode = "62022"
  , cdName = "Dynamic Duo"
  , cdCost = Just 2
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = EventType
  , cdUnique = False
  , cdAspect = Just A.Neutral
  , cdResources = [(PrintedResource, R.Physical)]
  , cdAbilityType = Just Ab.Action
  , cdAbilitySubTypes = []
  , cdBoostIcons = []
  , cdDeckLimit = Nothing
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = NoCriteria
  }

jl62023 :: CardDef
jl62023 = Base.fw53002
  { cdCardCode = "62023"
  , cdName = "Power Man and Iron Fist"
  , cdCost = Just 0
  , cdTraits = fromList [T.Attack]
  , cdKeywords = fromList []
  , cdCardType = EventType
  , cdUnique = False
  , cdAspect = Just A.Neutral
  , cdResources = [(PrintedResource, R.Physical)]
  , cdAbilityType = Just Ab.HeroAction
  , cdAbilitySubTypes = [Ab.Attack]
  , cdBoostIcons = []
  , cdDeckLimit = Just 1
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = namedTeam "Iron Fist" "Luke Cage"
  , cdTeamUp = Just ("Iron Fist", "Luke Cage")
  }

jl62028 :: CardDef
jl62028 = Base.fw53002
  { cdCardCode = "62028"
  , cdName = "Internal Injury"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = ObligationType
  , cdUnique = False
  , cdAspect = Nothing
  , cdResources = []
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdBoostIcons = [B.Boost, B.Boost]
  , cdDeckLimit = Nothing
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = NoCriteria
  }

jl62029 :: CardDef
jl62029 = Base.fw53002
  { cdCardCode = "62029"
  , cdName = "Cottonmouth"
  , cdCost = Nothing
  , cdTraits = fromList [T.SerpentSociety]
  , cdKeywords = fromList []
  , cdCardType = MinionType
  , cdUnique = True
  , cdAspect = Nothing
  , cdResources = []
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdBoostIcons = [B.Star]
  , cdDeckLimit = Just 1
  , cdEncounterSet = Just Set.LukeCageNemesis
  , cdEncounterSetQuantity = Just 1
  , cdCriteria = NoCriteria
  }

jl62030 :: CardDef
jl62030 = Base.fw53002
  { cdCardCode = "62030"
  , cdName = "Venomous Whispers"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = SideSchemeType
  , cdUnique = False
  , cdAspect = Nothing
  , cdResources = []
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdBoostIcons = [B.Boost, B.Boost, B.Star]
  , cdDeckLimit = Nothing
  , cdEncounterSet = Just Set.LukeCageNemesis
  , cdEncounterSetQuantity = Just 1
  , cdCriteria = NoCriteria
  }

jl62031 :: CardDef
jl62031 = Base.fw53002
  { cdCardCode = "62031"
  , cdName = "Augmented Jaws"
  , cdCost = Nothing
  , cdTraits = fromList [T.Weapon]
  , cdKeywords = fromList []
  , cdCardType = AttachmentType
  , cdUnique = False
  , cdAspect = Nothing
  , cdResources = []
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdBoostIcons = [B.Boost, B.Boost]
  , cdDeckLimit = Nothing
  , cdEncounterSet = Just Set.LukeCageNemesis
  , cdEncounterSetQuantity = Just 1
  , cdCriteria = NoCriteria
  }

jl62032 :: CardDef
jl62032 = Base.fw53002
  { cdCardCode = "62032"
  , cdName = "Asp"
  , cdCost = Nothing
  , cdTraits = fromList [T.SerpentSociety]
  , cdKeywords = fromList []
  , cdCardType = MinionType
  , cdUnique = True
  , cdAspect = Nothing
  , cdResources = []
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdBoostIcons = [B.Boost, B.Boost, B.Star]
  , cdDeckLimit = Just 1
  , cdEncounterSet = Just Set.LukeCageNemesis
  , cdEncounterSetQuantity = Just 1
  , cdCriteria = NoCriteria
  }

jl62033 :: CardDef
jl62033 = Base.fw53002
  { cdCardCode = "62033"
  , cdName = "Sidewinder"
  , cdCost = Nothing
  , cdTraits = fromList [T.SerpentSociety]
  , cdKeywords = fromList [K.Quickstrike]
  , cdCardType = MinionType
  , cdUnique = True
  , cdAspect = Nothing
  , cdResources = []
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdBoostIcons = [B.Boost, B.Boost]
  , cdDeckLimit = Just 1
  , cdEncounterSet = Just Set.LukeCageNemesis
  , cdEncounterSetQuantity = Just 1
  , cdCriteria = NoCriteria
  }

jl62034 :: CardDef
jl62034 = Base.fw53002
  { cdCardCode = "62034"
  , cdName = "Size Advantage"
  , cdCost = Just 2
  , cdTraits = fromList [T.Attack]
  , cdKeywords = fromList []
  , cdCardType = EventType
  , cdUnique = False
  , cdAspect = Just A.Aggression
  , cdResources = [(PrintedResource, R.Energy)]
  , cdAbilityType = Just Ab.HeroAction
  , cdAbilitySubTypes = [Ab.Attack]
  , cdBoostIcons = []
  , cdDeckLimit = Nothing
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = EnemyExists AttackableEnemy
  }

jl62035 :: CardDef
jl62035 = Base.fw53002
  { cdCardCode = "62035"
  , cdName = "Observe"
  , cdCost = Just 1
  , cdTraits = fromList [T.Tactic]
  , cdKeywords = fromList []
  , cdCardType = EventType
  , cdUnique = False
  , cdAspect = Just A.Justice
  , cdResources = [(PrintedResource, R.Energy)]
  , cdAbilityType = Just Ab.HeroAction
  , cdAbilitySubTypes = []
  , cdBoostIcons = []
  , cdDeckLimit = Nothing
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = NoCriteria
  }

jl62036 :: CardDef
jl62036 = Base.fw53002
  { cdCardCode = "62036"
  , cdName = "Toughen Up"
  , cdCost = Just 2
  , cdTraits = fromList [T.Condition]
  , cdKeywords = fromList []
  , cdCardType = UpgradeType
  , cdUnique = False
  , cdAspect = Just A.Protection
  , cdResources = [(PrintedResource, R.Physical)]
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdBoostIcons = []
  , cdDeckLimit = Nothing
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = NoCriteria
  }

jl62037 :: CardDef
jl62037 = Base.fw53002
  { cdCardCode = "62037"
  , cdName = "Black Belt"
  , cdCost = Just 0
  , cdTraits = fromList [T.Title]
  , cdKeywords = fromList []
  , cdCardType = UpgradeType
  , cdUnique = False
  , cdAspect = Just A.Neutral
  , cdResources = [(PrintedResource, R.Physical)]
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdBoostIcons = []
  , cdDeckLimit = Nothing
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = AnyCriteria [CharacterExists (CharacterControlledBy AnyIdentity <> CharacterWithTrait T.MartialArtist), UpgradeExists (UpgradeWithTrait T.MartialArtist), SupportExists (SupportWithTrait T.MartialArtist)]
  , cdLimit = Just (MaxPerCharacter 1)
  }

namedTeam :: Text -> Text -> Criteria
namedTeam first second = inPlay first <> inPlay second
 where
  inPlay name = AnyCriteria [CharacterExists (IdentityCharacters $ IdentityWithTitle name <> HeroIdentity), AllyExists (AllyNamedAs name)]
