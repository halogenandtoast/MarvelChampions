module Marvel.Ally.Cards where

import Marvel.Prelude

import Marvel.Cost.Types (Cost (MayOverpay, NoCost))

import Marvel.Aspect
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Criteria
import Marvel.Matchers.Types (IdentityMatcher (IdentityWithTrait))
import Marvel.Keyword

import Marvel.EncounterSet qualified as EncounterSet
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
  , ladySif
  , hercules
  , valkyrie
  , heimdall
  , wong
  , mockingbird
  , captainMarvelCarolDanvers
  , brotherVoodooJerichoDrumm
  , clea
  , ironFistDannyRand
  , ironManTonyStark
  , brawn
  , sentryRobertReynolds
  , sheHulkJenniferWalters
  -- The Green Goblin

  -- The Wrecking Crew

  -- The Rise of Red Skull
  , mockingbird
  , hawkeyeKateBishop
  , blackKnightDaneWhitman
  , goliathBillFoster
  , usAgentJohnWalker
  , warMachine
  , captainMarvelCarolDanvers
  , spiderGirlAnyaCorazon
  , spiderManPeterParker

  -- Taskmaster
  , moonKnightMarcSpector
  , shangChi
  , whiteTigerAngelaDelToro
  , elektra

  -- The Once and Future Kang


  -- Ant-Man
  , waspNadiaVanDyne
  , antManHankPym
  , giantMan
  , ronin
  , stingerCassieLang

  -- Wasp
  , antManScottLang
  , thorJaneFoster
  , waspJanetVanDyne
  , ironheart
  , spiderManMilesMorales

  -- Quicksilver
  , scarletWitchWandaMaximoff
  , multipleMan
  , warlock

  -- Scarlet Witch
  , quicksilverPietroMaximoff
  , speedThomasShepherd
  , wiccanWilliamKaplan

  -- Star-Lord
  , novaPrimeRichardRider
  , adamWarlock
  , betaRayBill
  , yondu
  , cosmo
  ]

-- | The allies of a hero's own set, "hero-specific" (Hydra Prison takes them): neither
-- aspect nor basic. ponytail: kept by hand; HeroSpecificCard now tells them apart, by a
-- basic card's Neutral aspect, and could replace this list.
heroSpecificAllies :: [CardDef]
heroSpecificAllies =
  [ blackCatFeliciaHardy
  , spiderWomanJessicaDrew
  , hellcatPatsyWalker
  , warMachineJamesRhodes
  , shuri
  , agent13SharonCarter
  , redDagger
  , winterSoldierBuckyBarnes
  , ladySif
  , wong
  , mockingbird
  , captainMarvelCarolDanvers
  , waspNadiaVanDyne
  , antManScottLang
  , scarletWitchWandaMaximoff
  , quicksilverPietroMaximoff
  , novaPrimeRichardRider
  ]

ally :: CardCode -> Name -> Int -> [Trait] -> [Resource] -> Aspect -> CardDef
ally code name cost traits resources aspect =
  baseAlly code name cost traits resources (Just aspect)

identityAlly :: CardCode -> Name -> Int -> [Trait] -> [Resource] -> CardDef
identityAlly code name cost traits resources =
  baseAlly code name cost traits resources Nothing

basicAlly :: CardCode -> Name -> Int -> [Trait] -> [Resource] -> CardDef
basicAlly code name cost traits resources =
  baseAlly code name cost traits resources (Just Neutral)

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
  , cdAbilityCost = NoCost
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
brawn :: CardDef
brawn = ally "10011" "Brawn" 3 [Champion, Gamma] [Mental] Aggression

sentryRobertReynolds :: CardDef
sentryRobertReynolds =
  ally "10012" ("Sentry" <:> "Robert Reynolds") 4 [Avenger] [Energy] Aggression

sheHulkJenniferWalters :: CardDef
sheHulkJenniferWalters =
  ally "10013" ("She-Hulk" <:> "Jennifer Walters") 4 [Avenger, Gamma] [Physical] Aggression

winterSoldierBuckyBarnes :: CardDef
winterSoldierBuckyBarnes =
  identityAlly "08002" ("Winter Soldier" <:> "Bucky Barnes") 4 [Avenger, Spy] [Wild]

agentCoulson :: CardDef
agentCoulson = ally "08011" "Agent Coulson" 3 [Shield, Spy] [Mental] Justice

quakeDaisyJohnson :: CardDef
quakeDaisyJohnson =
  ally "08012" ("Quake" <:> "Daisy Johnson") 2 [Avenger, Shield] [Energy] Justice

ladySif :: CardDef
ladySif = identityAlly "06002" "Lady Sif" 4 [Asgard] [Wild]

hercules :: CardDef
hercules = ally "06011" "Hercules" 6 [Avenger, Olympus] [Physical] Aggression

valkyrie :: CardDef
valkyrie = ally "06012" "Valkyrie" 3 [Asgard, Avenger] [Energy] Aggression

heimdall :: CardDef
heimdall = basicAlly "06020" "Heimdall" 5 [Asgard] [Mental]

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

mockingbird :: CardDef
mockingbird = identityAlly "04004" ("Mockingbird" <:> "Bobbi Morse") 3 [Avenger, Shield] [Wild]

hawkeyeKateBishop :: CardDef
hawkeyeKateBishop = ally "04011" ("Hawkeye" <:> "Kate Bishop") 2 [Avenger] [Physical] Leadership

-- | "[star] Black Knight's basic attack gains piercing": the keyword on him
blackKnightDaneWhitman :: CardDef
blackKnightDaneWhitman =
  (ally "04012" ("Black Knight" <:> "Dane Whitman") 3 [Avenger] [Physical] Leadership)
    { cdKeywords = fromList [Piercing]
    }

goliathBillFoster :: CardDef
goliathBillFoster = ally "04013" ("Goliath" <:> "Bill Foster") 4 [Avenger] [Mental] Leadership

usAgentJohnWalker :: CardDef
usAgentJohnWalker =
  (ally "04014" ("U.S. Agent" <:> "John Walker") 3 [Avenger] [Mental] Leadership)
    { cdKeywords = fromList [Retaliate 1]
    }

-- | "[star] War Machine's basic attack gains ranged": the keyword on him
warMachine :: CardDef
warMachine =
  (basicAlly "04020" ("War Machine" <:> "James Rhodes") 4 [Avenger, Shield] [Mental])
    { cdKeywords = fromList [Toughness, Ranged]
    }

captainMarvelCarolDanvers :: CardDef
captainMarvelCarolDanvers = identityAlly "04032" ("Captain Marvel" <:> "Carol Danvers") 4 [Avenger] [Wild]

spiderGirlAnyaCorazon :: CardDef
spiderGirlAnyaCorazon = ally "04040" ("Spider-Girl" <:> "Anya Corazon") 2 [Avenger] [Energy] Aggression

spiderManPeterParker :: CardDef
spiderManPeterParker = ally "04045" ("Spider-Man" <:> "Peter Parker") 5 [Avenger] [Mental] Justice

-- The Rise of Red Skull

-- Taskmaster: the Captive allies, set aside out of play in the encounter set until a
-- player rescues one

-- | A Captive ally of the Taskmaster encounter set: a player card, cost 0, kept out of
-- the encounter deck (it is not an encounter card) and owned by whoever rescues it.
captiveAlly :: CardCode -> Name -> CardDef
captiveAlly code name =
  (basicAlly code name 0 [Captive, HeroForHire] [Wild]) {cdEncounterSet = Just EncounterSet.Taskmaster, cdEncounterSetQuantity = Just 1}

moonKnightMarcSpector :: CardDef
moonKnightMarcSpector = captiveAlly "04097" ("Moon Knight" <:> "Marc Spector")

shangChi :: CardDef
shangChi = reprintedAs ["10098"] $ captiveAlly "04098" "Shang-Chi"

whiteTigerAngelaDelToro :: CardDef
whiteTigerAngelaDelToro = captiveAlly "04099" ("White Tiger" <:> "Angela Del Toro")

elektra :: CardDef
elektra = captiveAlly "04100" "Elektra"

-- Ant-Man

waspNadiaVanDyne :: CardDef
waspNadiaVanDyne = identityAlly "12002" ("Wasp" <:> "Nadia Van Dyne") 3 [Aerial, Avenger] [Wild]

-- | Its cost is 0, though more may be paid for its pym counters.
antManHankPym :: CardDef
antManHankPym =
  (ally "12011" ("Ant-Man" <:> "Hank Pym") 0 [Avenger] [Mental] Leadership)
    { cdAbilityCost = MayOverpay
    }

giantMan :: CardDef
giantMan = ally "12012" "Giant-Man" 5 [Avenger, Giant] [Physical] Leadership

ronin :: CardDef
ronin = ally "12013" "Ronin" 3 [Avenger] [Energy] Leadership

stingerCassieLang :: CardDef
stingerCassieLang =
  (ally "12014" ("Stinger" <:> "Cassie Lang") 1 [Avenger] [Energy] Leadership)
    { cdCriteria = SelfMatches (IdentityWithTrait Avenger)
    }

-- Wasp

antManScottLang :: CardDef
antManScottLang = identityAlly "13002" ("Ant-Man" <:> "Scott Lang") 4 [Avenger] [Wild]

thorJaneFoster :: CardDef
thorJaneFoster = ally "13011" ("Thor" <:> "Jane Foster") 4 [Asgard, Avenger] [Energy] Aggression

-- | Its cost is 0, though more may be paid for its pym counters.
waspJanetVanDyne :: CardDef
waspJanetVanDyne =
  (ally "13012" ("Wasp" <:> "Janet Van Dyne") 0 [Aerial, Avenger] [Mental] Aggression)
    { cdAbilityCost = MayOverpay
    }

ironheart :: CardDef
ironheart = basicAlly "13018" "Ironheart" 2 [Champion] [Mental]

spiderManMilesMorales :: CardDef
spiderManMilesMorales = basicAlly "13019" ("Spider-Man" <:> "Miles Morales") 3 [Champion] [Physical]

-- Quicksilver

scarletWitchWandaMaximoff :: CardDef
scarletWitchWandaMaximoff = identityAlly "14002" ("Scarlet Witch" <:> "Wanda Maximoff") 3 [Avenger] [Wild]

multipleMan :: CardDef
multipleMan = (ally "14012" "Multiple Man" 4 [XFactor] [Mental] Protection) {cdUnique = False}

warlock :: CardDef
warlock = ally "14013" "Warlock" 3 [XFactor] [Physical] Protection

-- Scarlet Witch

quicksilverPietroMaximoff :: CardDef
quicksilverPietroMaximoff = identityAlly "15002" ("Quicksilver" <:> "Pietro Maximoff") 4 [Avenger] [Wild]

speedThomasShepherd :: CardDef
speedThomasShepherd = ally "15010" ("Speed" <:> "Thomas Shepherd") 4 [Avenger] [Physical] Justice

wiccanWilliamKaplan :: CardDef
wiccanWilliamKaplan = ally "15011" ("Wiccan" <:> "William Kaplan") 2 [Avenger] [Mental] Justice

-- Star-Lord

novaPrimeRichardRider :: CardDef
novaPrimeRichardRider = identityAlly "17002" ("Nova Prime" <:> "Richard Rider") 5 [Aerial, NovaCorps] [Wild]

adamWarlock :: CardDef
adamWarlock = ally "17011" "Adam Warlock" 3 [Aerial, Mystic] [Physical] Leadership

betaRayBill :: CardDef
betaRayBill = ally "17012" "Beta Ray Bill" 5 [Asgard] [Physical] Leadership

-- | "Yondu's attacks gain ranged": his keyword.
yondu :: CardDef
yondu =
  (ally "17013" "Yondu" 4 [Guardian] [Energy] Leadership)
    { cdKeywords = fromList [Ranged]
    }

cosmo :: CardDef
cosmo = basicAlly "17020" "Cosmo" 2 [Guardian] [Mental]
