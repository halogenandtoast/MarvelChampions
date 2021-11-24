module Marvel.Event.Cards where

import Marvel.Prelude

import Marvel.Ability.Type (AbilityType(Action, HeroAction, AlterEgoAction, Response))
import Marvel.Aspect
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Name
import Marvel.Resource
import Marvel.Trait
import Marvel.Window

allEvents :: HashMap CardCode CardDef
allEvents = fromList $ map
  (toCardCode &&& id)
  [ backflip
  , enhancedSpiderSense
  , swingingWebKick
  , gammaSlam
  , groundStomp
  , legalPractice
  , oneTwoPunch
  , splitPersonality
  , getReady
  , leadFromTheFront
  , makeTheCall
  , firstAid
  , haymaker
  ]

event :: CardCode -> Name -> Int -> [Trait] -> [Resource] -> Aspect -> CardDef
event code name cost traits resources aspect =
  baseEvent code name cost traits resources (Just aspect)

identityEvent :: CardCode -> Name -> Int -> [Trait] -> [Resource] -> CardDef
identityEvent code name cost traits resources =
  baseEvent code name cost traits resources Nothing

basicEvent :: CardCode -> Name -> Int -> [Trait] -> [Resource] -> CardDef
basicEvent code name cost traits resources =
  baseEvent code name cost traits resources Nothing

baseEvent
  :: CardCode -> Name -> Int -> [Trait] -> [Resource] -> Maybe Aspect -> CardDef
baseEvent code name cost traits resources mAspect = CardDef
  { cdCardCode = code
  , cdName = name
  , cdCost = Just cost
  , cdTraits = fromList traits
  , cdKeywords = mempty
  , cdCardType = EventType
  , cdAbilityType = Just Action
  , cdUnique = False
  , cdAspect = mAspect
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = NoCriteria
  , cdResources = map (PrintedResource, ) resources
  , cdResponseWindow = Nothing
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  }

backflip :: CardDef
backflip = (identityEvent "01003" "Backflip" 0 [Defense, Skill] [Physical])
  { cdResponseWindow = Just (WouldTakeDamage You FromAttack AnyValue)
  }

enhancedSpiderSense :: CardDef
enhancedSpiderSense =
  (identityEvent "01004" "Enhanced Spider-Sense" 1 [Superpower] [Mental])
    { cdCriteria = InHeroForm
    , cdResponseWindow = Just
      (TreacheryRevealed When AnyTreachery FromEncounterDeck)
    }

swingingWebKick :: CardDef
swingingWebKick = (identityEvent
                    "01005"
                    "Swinging Web Kick"
                    3
                    [Aerial, Attack, Superpower]
                    [Mental]
                  )
  { cdCriteria = InHeroForm
  , cdAbilityType = Just HeroAction
  }

gammaSlam :: CardDef
gammaSlam = (identityEvent
                    "01021"
                    "Gamma Slam"
                    4
                    [Attack, Superpower]
                    [Mental]
                  )
  { cdCriteria = InHeroForm
  , cdAbilityType = Just HeroAction
  }

groundStomp :: CardDef
groundStomp = (identityEvent
                    "01022"
                    "Ground Stomp"
                    2
                    [Superpower]
                    [Mental]
                  )
  { cdCriteria = InHeroForm
  , cdAbilityType = Just HeroAction
  }

legalPractice :: CardDef
legalPractice = (identityEvent
                    "01023"
                    "Legal Practice"
                    0
                    [Skill, Thwart]
                    [Physical]
                  )
  { cdCriteria = InAlterEgoForm
  , cdAbilityType = Just AlterEgoAction
  }

oneTwoPunch :: CardDef
oneTwoPunch = (identityEvent
                    "01024"
                    "One-Two Punch"
                    1
                    [Skill]
                    [Physical]
                  )
  { cdAbilityType = Just Response
  , cdResponseWindow = Just (MakesBasicAttack After You)
  }

splitPersonality :: CardDef
splitPersonality = (identityEvent
                    "01025"
                    "Split Personality"
                    3
                    []
                    [Energy]
                  )
  { cdAbilityType = Just Action
  }

getReady :: CardDef
getReady = (event "01069" "Get Ready" 0 [] [Physical] Leadership)
  { cdCriteria = AllyExists ExhaustedAlly
  }

leadFromTheFront :: CardDef
leadFromTheFront =
  (event "01070" "Lead from the Front" 2 [Tactic] [Energy] Leadership)
    { cdCriteria = InHeroForm
    , cdAbilityType = Just HeroAction
    }

makeTheCall :: CardDef
makeTheCall = (event "01071" "Make the Call" 0 [] [Mental] Leadership)
  { cdCriteria =
    ExtendedCardExists
    $ AffordableCardBy You
    $ InDiscardOf AnyIdentity
    $ BasicCardMatches (CardWithType AllyType)
  }

firstAid :: CardDef
firstAid = (basicEvent "01086" "First Aid" 1 [] [Mental])
  { cdCriteria = CharacterExists CharacterWithAnyDamage
  }


haymaker :: CardDef
haymaker = (basicEvent "01087" "Haymaker" 2 [Attack] [Energy])
  { cdCriteria = InHeroForm
  , cdAbilityType = Just HeroAction
  }
