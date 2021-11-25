module Marvel.Ally.Cards where

import Marvel.Prelude

import Marvel.Aspect
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Criteria
import Marvel.Name
import Marvel.Resource
import Marvel.Trait

allAllies :: HashMap CardCode CardDef
allAllies = fromList $ map
  (toCardCode &&& id)
  [ blackCatFeliciaHardy
  , hellcatPatsyWalker
  , daredevilMattMurdock
  , hawkeyeClintBarton
  , mariaHill
  , vision
  , mockingbirdBobbiMorse
  , nickFury
  ]

ally :: CardCode -> Name -> Int -> [Trait] -> [Resource] -> Aspect -> CardDef
ally code name cost traits resources aspect =
  baseAlly code name cost traits resources (Just aspect)

identityAlly :: CardCode -> Name -> Int -> [Trait] -> [Resource] -> CardDef
identityAlly code name cost traits resources =
  baseAlly code name cost traits resources Nothing

basicAlly :: CardCode -> Name -> Int -> [Trait] -> [Resource] -> CardDef
basicAlly code name cost traits resources =
  baseAlly code name cost traits resources Nothing

baseAlly
  :: CardCode -> Name -> Int -> [Trait] -> [Resource] -> Maybe Aspect -> CardDef
baseAlly code name cost traits resources mAspect = CardDef
  { cdCardCode = code
  , cdName = name
  , cdCost = Just cost
  , cdTraits = fromList traits
  , cdKeywords = mempty
  , cdCardType = AllyType
  , cdAbilityType = Nothing
  , cdUnique = True
  , cdAspect = mAspect
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = NoCriteria
  , cdResources = map (PrintedResource, ) resources
  , cdBoostIcons = []
  , cdResponseWindow = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  }

blackCatFeliciaHardy :: CardDef
blackCatFeliciaHardy = identityAlly
  "01002"
  ("Black Cat" <:> "Felicia Hardy")
  2
  [HeroForHire]
  [Energy]

hellcatPatsyWalker :: CardDef
hellcatPatsyWalker =
  identityAlly "01020" ("Hellcat" <:> "Patsy Walker") 3 [Avenger] [Wild]

daredevilMattMurdock :: CardDef
daredevilMattMurdock =
  ally "01058" ("Daredevil" <:> "Matt Murdock") 4 [Defender] [Physical] Justice

jessicaJones :: CardDef
jessicaJones = ally "01059" "Jessica Jones" 3 [Defender] [Energy] Justice

hawkeyeClintBarton :: CardDef
hawkeyeClintBarton =
  ally "01066" ("Hawkeye" <:> "Clint Barton") 3 [Avenger] [Energy] Leadership

mariaHill :: CardDef
mariaHill = ally "01067" "Maria Hill" 2 [Shield] [Mental] Leadership

vision :: CardDef
vision = ally "01068" "Vision" 4 [Android, Avenger] [Physical] Leadership

mockingbirdBobbiMorse :: CardDef
mockingbirdBobbiMorse =
  basicAlly "01083" ("Mockingbird" <:> "Bobbi Morse") 3 [Shield, Spy] [Physical]

nickFury :: CardDef
nickFury = basicAlly "01084" "Nick Fury" 4 [Shield, Spy] [Mental]
