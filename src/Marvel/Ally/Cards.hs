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
allAllies = fromList $ concatMap
  toCardCodePairs
  [ blackCatFeliciaHardy
  , spiderWomanJessicaDrew
  , hellcatPatsyWalker
  , warMachineJamesRhodes
  , shuri
  , hulkBruceBanner
  , tigraGreerGrantNelson
  , daredevilMattMurdock
  , jessicaJones
  , hawkeyeClintBarton
  , mariaHill
  , vision
  , blackWidowNatashaRomanoff
  , lukeCage
  , mockingbirdBobbiMorse
  , nickFury
  , agent13SharonCarter
  , falconSamWilson
  , squirrelGirlDoreenGreen
  , wonderManSimonWilliams
  , redDagger
  , novaSamAlexander
  , lockjaw
  , winterSoldierBuckyBarnes
  , agentCoulson
  , quakeDaisyJohnson
  , wong
  , brotherVoodooJerichoDrumm
  , clea
  , ironFistDannyRand
  , ironManTonyStark
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
  , cdAbilitySubType = Nothing
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
  , cdLimit = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  }

blackCatFeliciaHardy :: CardDef
blackCatFeliciaHardy = identityAlly
  "01002"
  ("Black Cat" <:> "Felicia Hardy")
  2
  [HeroForHire]
  [Energy]

spiderWomanJessicaDrew :: CardDef
spiderWomanJessicaDrew = identityAlly
  "01011"
  ("Spider-Woman" <:> "Jessica Drew")
  3
  [Avenger, Spy]
  [Wild]

hellcatPatsyWalker :: CardDef
hellcatPatsyWalker =
  identityAlly "01020" ("Hellcat" <:> "Patsy Walker") 3 [Avenger] [Wild]

warMachineJamesRhodes :: CardDef
warMachineJamesRhodes = identityAlly
  "01030"
  ("War Machine" <:> "James Rhodes")
  4
  [Shield, Soldier]
  [Wild]

shuri :: CardDef
shuri = identityAlly "01041" "Shuri" 2 [Genius, Wakanda] [Physical]

hulkBruceBanner :: CardDef
hulkBruceBanner = ally
  "01050"
  ("Hulk" <:> "Bruce Banner")
  2
  [Avenger, Gamma]
  [Energy]
  Aggression

tigraGreerGrantNelson :: CardDef
tigraGreerGrantNelson = ally
  "01051"
  ("Tigra" <:> "Greer Grant Nelson")
  3
  [Avenger]
  [Mental]
  Aggression

daredevilMattMurdock :: CardDef
daredevilMattMurdock =
  ally "01058" ("Daredevil" <:> "Matt Murdock") 4 [Defender] [Physical] Justice

jessicaJones :: CardDef
jessicaJones = ally "01059" "Jessica Jones" 3 [Defender] [Energy] Justice

hawkeyeClintBarton :: CardDef
hawkeyeClintBarton = reprintedAs ["03012"] $
  ally "01066" ("Hawkeye" <:> "Clint Barton") 3 [Avenger] [Energy] Leadership

mariaHill :: CardDef
mariaHill = ally "01067" "Maria Hill" 2 [Shield] [Mental] Leadership

vision :: CardDef
vision = ally "01068" "Vision" 4 [Android, Avenger] [Physical] Leadership

blackWidowNatashaRomanoff :: CardDef
blackWidowNatashaRomanoff = ally
  "01075"
  ("Black Widow" <:> "Natasha Romanoff")
  3
  [Shield, Spy]
  [Physical]
  Protection

lukeCage :: CardDef
lukeCage = ally "01076" "Luke Cage" 4 [Defender] [Energy] Protection

mockingbirdBobbiMorse :: CardDef
mockingbirdBobbiMorse = reprintedAs ["03020"] $
  basicAlly "01083" ("Mockingbird" <:> "Bobbi Morse") 3 [Shield, Spy] [Physical]

nickFury :: CardDef
nickFury = reprintedAs ["08019"] $ basicAlly "01084" "Nick Fury" 4 [Shield, Spy] [Mental]

agent13SharonCarter :: CardDef
agent13SharonCarter =
  identityAlly "03002" ("Agent 13" <:> "Sharon Carter") 3 [Shield] [Mental]

falconSamWilson :: CardDef
falconSamWilson =
  ally "03011" ("Falcon" <:> "Sam Wilson") 4 [Aerial, Avenger] [Physical] Leadership

squirrelGirlDoreenGreen :: CardDef
squirrelGirlDoreenGreen =
  ally "03013" ("Squirrel Girl" <:> "Doreen Green") 2 [Avenger] [Mental] Leadership

wonderManSimonWilliams :: CardDef
wonderManSimonWilliams =
  ally "03014" ("Wonder Man" <:> "Simon Williams") 2 [Avenger] [Physical] Leadership

redDagger :: CardDef
redDagger = identityAlly "05002" "Red Dagger" 3 [Champion] [Energy]

novaSamAlexander :: CardDef
novaSamAlexander =
  ally "05012" ("Nova" <:> "Sam Alexander") 4 [Champion] [Energy] Protection

lockjaw :: CardDef
lockjaw =
  (basicAlly "05018" "Lockjaw" 4 [Inhuman] [Physical]) {cdPlayableFromDiscard = True}

winterSoldierBuckyBarnes :: CardDef
winterSoldierBuckyBarnes =
  identityAlly "08002" ("Winter Soldier" <:> "Bucky Barnes") 4 [Avenger, Spy] [Wild]

agentCoulson :: CardDef
agentCoulson = ally "08011" "Agent Coulson" 3 [Shield, Spy] [Mental] Justice

quakeDaisyJohnson :: CardDef
quakeDaisyJohnson =
  ally "08012" ("Quake" <:> "Daisy Johnson") 2 [Avenger, Shield] [Energy] Justice

wong :: CardDef
wong = identityAlly "09002" "Wong" 3 [Mystic] [Wild]

brotherVoodooJerichoDrumm :: CardDef
brotherVoodooJerichoDrumm =
  ally "09012" ("Brother Voodoo" <:> "Jericho Drumm") 3 [Avenger, Mystic] [Mental] Protection

clea :: CardDef
clea = ally "09013" "Clea" 2 [Mystic] [Physical] Protection

ironFistDannyRand :: CardDef
ironFistDannyRand =
  ally "09014" ("Iron Fist" <:> "Danny Rand") 4 [Defender] [Physical] Protection

ironManTonyStark :: CardDef
ironManTonyStark =
  ally "09039" ("Iron Man" <:> "Tony Stark") 4 [Avenger] [Wild] Leadership
