module Marvel.AlterEgo.Cards where

import Marvel.Prelude

import Marvel.Cost.Types (Cost (NoCost))

import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Criteria
import Marvel.Name
import Marvel.Trait

allAlterEgoCards :: [CardDef]
allAlterEgoCards =
  [peterParker, carolDanvers, jenniferWalters, tonyStark, tChalla, steveRogers, kamalaKhan, natashaRomanoff, odinson, stephenStrange, bruceBanner]

allAlterEgosMap :: HashMap CardCode CardDef
allAlterEgosMap = fromList $ map (toFst toCardCode) allAlterEgoCards

alterEgo :: CardCode -> Name -> [Trait] -> CardDef
alterEgo cardCode name traits = CardDef
  { cdCardCode = cardCode
  , cdName = name
  , cdCost = Nothing
  , cdTraits = fromList traits
  , cdKeywords = mempty
  , cdCardType = AlterEgoType
  , cdAbilityType = Nothing
  , cdAbilitySubType = Nothing
  , cdUnique = True
  , cdAspect = Nothing
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdResponseWindow = Nothing
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdLimit = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  }

peterParker :: CardDef
peterParker = alterEgo "01001b" "Peter Parker" [Genius]

carolDanvers :: CardDef
carolDanvers = alterEgo "01010b" "Carol Danvers" [Shield, Soldier]

jenniferWalters :: CardDef
jenniferWalters = alterEgo "01019b" "Jennifer Walters" [Attorney, Gamma]

tonyStark :: CardDef
tonyStark = alterEgo "01029b" "Tony Stark" [Genius]

tChalla :: CardDef
tChalla = alterEgo "01040b" "T'Challa" [King, Wakanda]

steveRogers :: CardDef
steveRogers = alterEgo "03001b" "Steve Rogers" [Shield, Soldier]

kamalaKhan :: CardDef
kamalaKhan = alterEgo "05001b" "Kamala Khan" [Inhuman]
bruceBanner :: CardDef
bruceBanner = alterEgo "10001b" "Bruce Banner" [Genius, Scientist]

natashaRomanoff :: CardDef
natashaRomanoff = alterEgo "08001b" "Natasha Romanoff" [Shield, Spy]

odinson :: CardDef
odinson = alterEgo "06001b" "Odinson" [Asgard]

stephenStrange :: CardDef
stephenStrange = alterEgo "09001b" "Stephen Strange" [Mystic]
