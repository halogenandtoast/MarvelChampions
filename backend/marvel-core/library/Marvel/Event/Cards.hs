module Marvel.Event.Cards where

import Marvel.Prelude

import Marvel.Ability.Type (AbilityType(HeroAction))
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
  , cdAbilityType = Nothing
  , cdUnique = False
  , cdAspect = mAspect
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = NoCriteria
  , cdResources = map (PrintedResource, ) resources
  , cdResponseWindow = Nothing
  , cdBoostIcons = []
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

getReady :: CardDef
getReady = event "01069" "Get Ready" 0 [] [Physical] Leadership

leadFromTheFront :: CardDef
leadFromTheFront =
  event "01070" "Lead from the Front" 2 [Tactic] [Energy] Leadership

makeTheCall :: CardDef
makeTheCall = event "01071" "Make the Call" 0 [] [Mental] Leadership

firstAid :: CardDef
firstAid = basicEvent "01086" "First Aid" 1 [] [Mental]

haymaker :: CardDef
haymaker = (basicEvent "01087" "Haymaker" 2 [Attack] [Energy])
  { cdCriteria = InHeroForm
  , cdAbilityType = Just HeroAction
  }
