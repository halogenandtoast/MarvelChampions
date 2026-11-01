module Marvel.Ally.Cards where

import Marvel.Ability.Types qualified as AnAbility
import Marvel.Aspect qualified as AnAspect
import Marvel.Boost qualified as AnBoost
import Marvel.Card.Def qualified as AnCard
import Marvel.Cost.Types qualified as AnCost
import Marvel.Criteria qualified as AnCriteria
import Marvel.EncounterSet qualified as AnSet
import Marvel.Keyword qualified as AnKeyword
import Marvel.Matchers qualified as AnMatch
import Marvel.Name qualified as AnName
import Marvel.Resource.Types qualified as AnResource
import Marvel.Trait qualified as AnTrait
import Marvel.Window.Types qualified as AnWindow

import Marvel.Ability.Types qualified as PsyAbility
import Marvel.Aspect qualified as PsyAspect
import Marvel.Boost qualified as PsyBoost
import Marvel.Card.Def qualified as PsyCard
import Marvel.Cost.Types qualified as PsyCost
import Marvel.Criteria qualified as PsyCriteria
import Marvel.EncounterSet qualified as PsySet
import Marvel.Keyword qualified as PsyKeyword
import Marvel.Matchers qualified as PsyMatch
import Marvel.Name qualified as PsyName
import Marvel.Resource.Types qualified as PsyResource
import Marvel.Trait qualified as PsyTrait
import Marvel.Window.Types qualified as PsyWindow

import Marvel.Card.Def (ResourceRestriction (PrintedResource))

import Marvel.Criteria (Criteria (NoCriteria))

import Marvel.Cost.Types (Cost (NoCost))

import Marvel.Resource.Types qualified as NEResource

import Marvel.Boost qualified as NEBoost

import Marvel.GameValue.Types qualified as NEValue

import Marvel.EncounterSet qualified as NESet

import Marvel.Keyword qualified as NEKeyword

import Marvel.Trait qualified as NETrait

import Marvel.Aspect qualified as NEAspect

import Marvel.Boost qualified as NEB
import Marvel.EncounterSet qualified as NES
import Marvel.Aspect qualified as NEA
import Marvel.Resource.Types qualified as NER
import Marvel.Keyword qualified as NEK
import Marvel.Trait qualified as NET
import Marvel.Trait qualified as NE
import Marvel.Resource qualified as NE
import Marvel.Keyword qualified as NE
import Marvel.Boost qualified as NE
import Marvel.Aspect qualified as NE
import Marvel.Card.Def qualified as NE
import Marvel.Ability.Types qualified as NEAbility

import Marvel.Trait qualified as MojoTrait
import Marvel.EncounterSet qualified as MojoSet
import Marvel.Keyword qualified as MojoKeyword
import Marvel.GameValue.Types qualified as MojoValue
import Marvel.Boost qualified as MojoBoost

import Marvel.EncounterSet qualified as MGSet
import Marvel.Keyword qualified as MGKeyword
import Marvel.Trait qualified as MGTrait
import Marvel.Resource qualified as MGResource
import Marvel.Boost qualified as MGBoost
import Marvel.EncounterSet
import Marvel.Ability.Types qualified as Ability
import Marvel.Cost.Types
import Marvel.Window
import Marvel.Trait qualified as MGTrait
import Marvel.EncounterSet qualified as MGSet
import Marvel.Keyword qualified as MGKeyword
import Marvel.GameValue.Types qualified as MGValue
import Marvel.Boost qualified as MGBoost
import Marvel.Prelude

import Marvel.Cost.Types (Cost (MayOverpay, NoCost))

import Marvel.Aspect
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Keyword

import Marvel.EncounterSet qualified as EncounterSet
import Marvel.Name
import Marvel.Resource
import Marvel.Trait

allAllies :: HashMap CardCode CardDef
allAllies = fromList $ concatMap
  toCardCodePairs
  [ psylockeAngel, elixir, siryn, warpath, cannonball
  , angelPsylocke, captainBritain, cypher, peteWisdom, dominoPsylocke
  , ne40079, ne40130
  , whiteFox
  , atlasBear
  , wolfsbane
  , feral
  , outlaw
  , diamondback
  , cable40014, cable40015, cable40016, cable40024, cable40025, cable40204
  , mojo39071
  , gambitRogue, iceman, karma, armor
  , rogueGambit, bishop, dazzler
  , havok, mirage, gentle, pixie, forge
  , phoenixJeanGrey, beast, dust, rockslide, blindfold, angel
  , jubilee, psylocke, sunfire, longshot
  , mgRobertKelly, mgJubilee, mgRictor, mgBoomBoom, mgCannonball, mgWolfsbane, mgCampaignMagneto
  , blackCatFeliciaHardy
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

  -- Gamora
  , nebulaAlly
  , angelaAldrifOdinsdottir
  , drax

  -- Groot
  , starhawkStakarOgord
  , rocketRaccoonAlly

  -- Rocket Raccoon
  , bug
  , grootAlly
  -- Drax
  , mantis
  , martyrPhylaVell
  , moondragonHeatherDouglas
  , gamoraAlly

  -- Venom
  , jackFlagJackHarrison
  , starLordPeterQuill

  -- Spectrum
  , blueMarvelAdamBrashear
  , captainAmericaSteveRogers
  , powerManVictorAlvarez
  , whiteTigerAvaAyala
  , kaluu
  , bladeEricBrooks

  -- Adam Warlock
  , pipTheTroll
  , marvelBoyNohVarr
  , quasarWendellVaughn
  , majorVictoryVanceAstro
  , charlie27
  , martinexTNaga

  -- The Mad Titan's Shadow: Hela
  , odinCaptive
  , odinKing

  -- The Mad Titan's Shadow: the campaign set
  , cosmoCampaign
  , ladySifCaptive
  , fandral
  , hogun
  , volstagg
  , gamoraNebula
  , eros
  , wraithZakDel
  , venomFlashThompson
  , ironManWarMachine
  , blackPantherTChalla
  , captainMarvelWarMachine
  , annabelleRiggs
  , thorOdinson
  , throgPuddlegulp
  , vivian
  , jocasta
  , protectorAlexis
  , victorMancha
  , machineManAaronStack
  , silkCindyMoon
  , spiderManMilesMoralesSinisterMotives
  , spiderUKBillyBraddock
  , spiderManHobieBrown
  , monicaChang
  , spiderWomanJessicaDrewSinisterMotives
  , agent13SharonCarterSinisterMotives
  , dumDumDugan
  , ghostSpiderGwenStacy
  , spiderManPeterParkerSinisterMotives
  , venomEddieBrock
  , msMarvelKamalaKhanNova
  , theLocustFernandaRodriguez
  , moonGirlLunellaLafayette
  , brawnAmadeusChoIronheart
  , cloud9
  , falconJoaquinTorres
  , patriotRayshaunLucas
  , snowguardAmkaAliyak
  , vivianIronheart
  , bombshellLanaBaumgartner
  , waspNadiaVanDyneIronheart
  , pinpointQureshiGupta
  , captainAmericat
  , ladySpider
  , spiderManPavitrPrabhakar
  , scarletSpider
  , spDr
  , venmAddyBrock
  , daredevilSpdr
  , spiderManNoir
  , spiderHamPeterPorker
  , spiderManOttoOctavius
  , shadowcatKittyPryde
  , nightcrawler
  , polaris
  , professorX
  , lockheed
  , wolverine
  , magik
  , colossusPiotrRasputin
  , cyclopsScottSummers
  , banshee
  , marvelGirl
  , stormOroroMunroe
  , controlledMinion
  ]

-- | The allies of a hero's own set, "hero-specific" (Hydra Prison takes them): neither
-- aspect nor basic. ponytail: kept by hand; HeroSpecificCard now tells them apart, by a
-- basic card's Neutral aspect, and could replace this list.
heroSpecificAllies :: [CardDef]
heroSpecificAllies =
  [ rogueGambit
  , blackCatFeliciaHardy
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
  , nebulaAlly
  , mantis
  , blueMarvelAdamBrashear
  , pipTheTroll
  , msMarvelKamalaKhanNova
  , brawnAmadeusChoIronheart
  , venmAddyBrock
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
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = mAspect
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = NoCriteria
  , cdResources = map (PrintedResource, ) resources
  , cdBoostIcons = []
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
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
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
mockingbirdBobbiMorse = deckLimited 1 $ reprintedAs ["03020", "23022"] $
  basicAlly "01083" ("Mockingbird" <:> "Bobbi Morse") 3 [Shield, Spy] [Physical]

nickFury :: CardDef
nickFury = reprintedAs ["08019"] $ basicAlly "01084" "Nick Fury" 4 [Shield, Spy] [Mental]

agent13SharonCarter :: CardDef
agent13SharonCarter =
  identityAlly "03002" ("Agent 13" <:> "Sharon Carter") 3 [Shield] [Mental]

falconSamWilson :: CardDef
falconSamWilson =
  deckLimited 1 $ reprintedAs ["23014"] $ ally "03011" ("Falcon" <:> "Sam Wilson") 4 [Aerial, Avenger] [Physical] Leadership

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
goliathBillFoster = deckLimited 1 $ reprintedAs ["23015"] $ ally "04013" ("Goliath" <:> "Bill Foster") 4 [Avenger] [Mental] Leadership

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
cosmo = deckLimited 1 $ reprintedAs ["22020"] $ basicAlly "17020" "Cosmo" 2 [Guardian] [Mental]

-- Gamora

nebulaAlly :: CardDef
nebulaAlly = identityAlly "18002" "Nebula" 2 [Guardian] [Wild]

angelaAldrifOdinsdottir :: CardDef
angelaAldrifOdinsdottir = deckLimited 1 $ reprintedAs ["25015"] $ ally "18011" ("Angela" <:> "Aldrif Odinsdottir") 0 [Asgard, Guardian] [Mental] Aggression

drax :: CardDef
drax =
  (basicAlly "18019" "Drax" 3 [Guardian] [Physical])
    { cdCriteria = SelfMatches (IdentityWithTrait Guardian)
    }

-- Groot

starhawkStakarOgord :: CardDef
starhawkStakarOgord = ally "16012" ("Starhawk" <:> "Stakar Ogord") 2 [Aerial, Guardian] [Physical] Protection

rocketRaccoonAlly :: CardDef
rocketRaccoonAlly =
  (basicAlly "16019" "Rocket Raccoon" 3 [Guardian] [Wild])
    { cdCriteria = SelfMatches (IdentityWithTrait Guardian)
    }

-- Rocket Raccoon

bug :: CardDef
bug = ally "16040" "Bug" 2 [Guardian] [Energy] Aggression

-- | "Play only if your identity has the Guardian trait."
grootAlly :: CardDef
grootAlly =
  (basicAlly "16047" "Groot" 3 [Guardian] [Wild])
    { cdCriteria = SelfMatches (IdentityWithTrait Guardian)
    }

-- Drax

mantis :: CardDef
mantis = identityAlly "19002" "Mantis" 2 [Guardian] [Wild]

martyrPhylaVell :: CardDef
martyrPhylaVell = ally "19012" ("Martyr" <:> "Phyla-Vell") 4 [Guardian] [Energy] Protection

moondragonHeatherDouglas :: CardDef
moondragonHeatherDouglas = ally "19013" ("Moondragon" <:> "Heather Douglas") 3 [Guardian] [Mental] Protection

gamoraAlly :: CardDef
gamoraAlly =
  (basicAlly "19020" "Gamora" 3 [Guardian] [Energy])
    { cdCriteria = SelfMatches (IdentityWithTrait Guardian)
    }

-- Venom

jackFlagJackHarrison :: CardDef
jackFlagJackHarrison = ally "20011" ("Jack Flag" <:> "Jack Harrison") 4 [Guardian] [Energy] Justice

starLordPeterQuill :: CardDef
starLordPeterQuill =
  (basicAlly "20016" ("Star-Lord" <:> "Peter Quill") 2 [Guardian] [Mental])
    { cdKeywords = fromList [Ranged]
    }

-- Spectrum

blueMarvelAdamBrashear :: CardDef
blueMarvelAdamBrashear = identityAlly "21005" ("Blue Marvel" <:> "Adam Brashear") 3 [Aerial, Avenger] [Wild]

captainAmericaSteveRogers :: CardDef
captainAmericaSteveRogers =
  (ally "21011" ("Captain America" <:> "Steve Rogers") 6 [Avenger] [Physical] Leadership)
    { cdKeywords = fromList [Toughness]
    }

powerManVictorAlvarez :: CardDef
powerManVictorAlvarez = ally "21012" ("Power Man" <:> "Victor Alvarez") 3 [Avenger] [Mental] Leadership

whiteTigerAvaAyala :: CardDef
whiteTigerAvaAyala = ally "21013" ("White Tiger" <:> "Ava Ayala") 3 [Avenger] [Energy] Leadership

kaluu :: CardDef
kaluu = ally "21014" "Kaluu" 2 [Avenger, Mystic] [Energy] Leadership

bladeEricBrooks :: CardDef
bladeEricBrooks = basicAlly "21019" ("Blade" <:> "Eric Brooks") 1 [Avenger, Vampire] [Physical]

-- Adam Warlock

pipTheTroll :: CardDef
pipTheTroll =
  (identityAlly "21032" "Pip the Troll" 2 [Mystic] [Wild])
    { cdKeywords = fromList [Toughness]
    }

marvelBoyNohVarr :: CardDef
marvelBoyNohVarr = ally "21041" ("Marvel Boy" <:> "Noh-Varr") 2 [Guardian, Kree] [Physical] Aggression

quasarWendellVaughn :: CardDef
quasarWendellVaughn = ally "21047" ("Quasar" <:> "Wendell Vaughn") 3 [Aerial, Guardian] [Energy] Justice

majorVictoryVanceAstro :: CardDef
majorVictoryVanceAstro = ally "21053" ("Major Victory" <:> "Vance Astro") 2 [Guardian] [Mental] Leadership

charlie27 :: CardDef
charlie27 =
  (ally "21059" "Charlie-27" 4 [Guardian] [Physical] Protection)
    { cdKeywords = fromList [Retaliate 1, Toughness]
    }

martinexTNaga :: CardDef
martinexTNaga = basicAlly "21065" ("Martinex" <:> "T'Naga") 3 [Guardian] [Energy]

-- The Mad Titan's Shadow: Hela

-- | Odin's two sides, the scenario's player card: attached to Odin's Torment, Captive
-- side faceup, until Hall of Nastrond is defeated.
odinCaptive :: CardDef
odinCaptive =
  (basicAlly "21139a" ("Odin" <:> "All-Father") 0 [Asgard, Captive] []) {cdEncounterSet = Just EncounterSet.Hela, cdEncounterSetQuantity = Just 1}

odinKing :: CardDef
odinKing =
  (basicAlly "21139b" ("Odin" <:> "All-Father") 0 [Asgard, King] []) {cdEncounterSet = Just EncounterSet.Hela, cdEncounterSetQuantity = Just 1}

-- The Mad Titan's Shadow: the campaign set, cards no deck may hold

-- | Secure the Landing Pad's other side: the first player gains control of him.
cosmoCampaign :: CardDef
cosmoCampaign = (basicAlly "21180b" "Cosmo" 0 [Guardian] [Mental]) {cdEncounterSet = Just EncounterSet.MadTitansShadowCampaign, cdEncounterSetQuantity = Just 1}

-- | A Captive ally of the campaign set, set aside for Open the Dungeons.
asgardCaptive :: CardCode -> Name -> CardDef
asgardCaptive code name =
  (basicAlly code name 0 [Asgard, Captive] [Wild]) {cdEncounterSet = Just EncounterSet.MadTitansShadowCampaign, cdEncounterSetQuantity = Just 1}

ladySifCaptive :: CardDef
ladySifCaptive = asgardCaptive "21190" "Lady Sif"

fandral :: CardDef
fandral = asgardCaptive "21191" "Fandral"

hogun :: CardDef
hogun = (asgardCaptive "21192" "Hogun") {cdKeywords = fromList [Piercing]}

volstagg :: CardDef
volstagg = (asgardCaptive "21193" "Volstagg") {cdKeywords = fromList [Retaliate 1, Toughness]}

-- Nebula

gamoraNebula :: CardDef
gamoraNebula = deckLimited 1 $ (identityAlly "22002" "Gamora" 3 [Guardian] [Wild])
    { cdUnique = True
    }

eros :: CardDef
eros = deckLimited 1 $ (ally "22011" "Eros" 2 [Guardian] [Mental] Justice)
    { cdUnique = True
    }

wraithZakDel :: CardDef
wraithZakDel = deckLimited 1 $ (ally "22012" ("Wraith" <:> "Zak-Del") 3 [Guardian] [Physical] Justice)
    { cdUnique = True
    }

venomFlashThompson :: CardDef
venomFlashThompson = deckLimited 1 $ (ally "22013" ("Venom" <:> "Flash Thompson") 4 [Guardian] [Energy] Justice)
    { cdUnique = True
    }

-- War Machine

ironManWarMachine :: CardDef
ironManWarMachine = deckLimited 1 $ (identityAlly "23002" ("Iron Man" <:> "Tony Stark") 3 [Avenger] [Physical])
    { cdUnique = True
    }

blackPantherTChalla :: CardDef
blackPantherTChalla = deckLimited 1 $ (ally "23012" ("Black Panther" <:> "T'Challa") 4 [Avenger, Wakanda] [Mental] Leadership)
    { cdUnique = True
    }

captainMarvelWarMachine :: CardDef
captainMarvelWarMachine = deckLimited 1 $ (ally "23013" ("Captain Marvel" <:> "Carol Danvers") 5 [Aerial, Avenger] [Energy] Leadership)
    { cdUnique = True
    }

-- Valkyrie

annabelleRiggs :: CardDef
annabelleRiggs = deckLimited 1 $ (identityAlly "25003" "Annabelle Riggs" 2 [Civilian] [Wild])
    { cdUnique = True
    }

thorOdinson :: CardDef
thorOdinson = deckLimited 1 $ (ally "25013" ("Thor" <:> "Odinson") 3 [Asgard, Avenger] [Energy] Aggression)
    { cdUnique = True
    , cdKeywords = fromList [Toughness]
    }

throgPuddlegulp :: CardDef
throgPuddlegulp = deckLimited 1 $ (ally "25014" ("Throg" <:> "Puddlegulp") 2 [Asgard, Guardian] [Energy] Aggression)
    { cdUnique = True
    }

-- Vision

vivian :: CardDef
vivian = deckLimited 1 $ (identityAlly "26003" "Vivian" 2 [Android, Champion] [Wild])
    { cdUnique = True
    }

jocasta :: CardDef
jocasta = deckLimited 1 $ (ally "26013" "Jocasta" 3 [Android, Avenger] [Mental] Protection)
    { cdUnique = True
    }

protectorAlexis :: CardDef
protectorAlexis = deckLimited 1 $ (ally "26014" ("Protector" <:> "Alexis") 4 [Android, Avenger] [Physical] Protection)
    { cdUnique = True
    }

victorMancha :: CardDef
victorMancha = deckLimited 1 $ (ally "26015" "Victor Mancha" 2 [Android, Avenger] [Energy] Protection)
    { cdUnique = True
    }

machineManAaronStack :: CardDef
machineManAaronStack = deckLimited 1 $ (basicAlly "26022" ("Machine Man" <:> "Aaron Stack") 2 [Android] [Physical])
    { cdUnique = True
    }


-- Sinister Motives

silkCindyMoon :: CardDef
silkCindyMoon = (ally "27010" ("Silk" <:> "Cindy Moon") 2 [WebWarrior] [Energy] Protection)
    { cdDeckLimit = Just 1
    }

spiderManMilesMoralesSinisterMotives :: CardDef
spiderManMilesMoralesSinisterMotives = (ally "27011" ("Spider-Man" <:> "Miles Morales") 4 [WebWarrior] [Mental] Protection)
    { cdDeckLimit = Just 1
    }

spiderUKBillyBraddock :: CardDef
spiderUKBillyBraddock = (ally "27012" ("Spider-UK" <:> "Billy Braddock") 3 [WebWarrior] [Physical] Protection)
    { cdDeckLimit = Just 1
    }

spiderManHobieBrown :: CardDef
spiderManHobieBrown = (basicAlly "27017" ("Spider-Man" <:> "Hobie Brown") 3 [WebWarrior] [Physical])
    { cdDeckLimit = Just 1
    , cdCriteria = AnyCriteria [SelfMatches (IdentityWithTrait WebWarrior), AllyExists (AllyControlledBy You <> AllyWithTrait WebWarrior), UpgradeExists (UpgradeControlledBy You <> UpgradeWithTrait WebWarrior), SupportExists (SupportMatches [SupportControlledBy You, SupportWithTrait WebWarrior])]
    }

monicaChang :: CardDef
monicaChang = (ally "27040" "Monica Chang" 3 [Shield] [Mental] Justice)
    { cdDeckLimit = Just 1
    }

spiderWomanJessicaDrewSinisterMotives :: CardDef
spiderWomanJessicaDrewSinisterMotives = (ally "27041" ("Spider-Woman" <:> "Jessica Drew") 3 [Shield, WebWarrior] [Mental] Justice)
    { cdDeckLimit = Just 1
    }

agent13SharonCarterSinisterMotives :: CardDef
agent13SharonCarterSinisterMotives = reprintedAs ["29022"] $ (basicAlly "27046" ("Agent 13" <:> "Sharon Carter") 4 [Shield, Spy] [Physical])
    { cdDeckLimit = Just 1
    }

dumDumDugan :: CardDef
dumDumDugan = (basicAlly "27047" "Dum Dum Dugan" 5 [Shield] [Physical])
    { cdDeckLimit = Just 1
    }

ghostSpiderGwenStacy :: CardDef
ghostSpiderGwenStacy = (basicAlly "27048" ("Ghost-Spider" <:> "Gwen Stacy") 3 [WebWarrior] [Wild])
    { cdDeckLimit = Just 1
    , cdCriteria = AnyCriteria [SelfMatches (IdentityWithTrait WebWarrior), AllyExists (AllyControlledBy You <> AllyWithTrait WebWarrior), UpgradeExists (UpgradeControlledBy You <> UpgradeWithTrait WebWarrior), SupportExists (SupportMatches [SupportControlledBy You, SupportWithTrait WebWarrior])]
    }

spiderManPeterParkerSinisterMotives :: CardDef
spiderManPeterParkerSinisterMotives = (basicAlly "27049" ("Spider-Man" <:> "Peter Parker") 3 [WebWarrior] [Mental])
    { cdDeckLimit = Just 1
    , cdKeywords = fromList [Requirement [Energy, Mental, Physical]]
    }

venomEddieBrock :: CardDef
venomEddieBrock = (basicAlly "27190" ("Venom" <:> "Eddie Brock") 4 [Symbiote] [Wild])
    { cdDeckLimit = Just 1
    , cdHazards = 1
    }

-- Nova

msMarvelKamalaKhanNova :: CardDef
msMarvelKamalaKhanNova = (identityAlly "28002" ("Ms. Marvel" <:> "Kamala Khan") 3 [Champion, Inhuman] [Physical])
  { cdDeckLimit = Just 1
  , cdUnique = True
  }

theLocustFernandaRodriguez :: CardDef
theLocustFernandaRodriguez = (ally "28010" ("The Locust" <:> "Fernanda Rodriguez") 2 [Champion] [Energy] Aggression)
  { cdDeckLimit = Just 1
  , cdUnique = True
  , cdCriteria = SelfMatches (IdentityWithTrait Champion)
  }

moonGirlLunellaLafayette :: CardDef
moonGirlLunellaLafayette = (basicAlly "28018" ("Moon Girl" <:> "Lunella Lafayette") 3 [Champion, Genius] [Mental])
  { cdDeckLimit = Just 1
  , cdUnique = True
  , cdCriteria = AnyCriteria [SelfMatches (IdentityWithTrait Champion), SelfMatches (IdentityWithTrait Genius)]
  }


-- Ironheart

brawnAmadeusChoIronheart :: CardDef
brawnAmadeusChoIronheart = (identityAlly "29004" ("Brawn" <:> "Amadeus Cho") 4 [Champion, Gamma] [Physical])
  { cdDeckLimit = Just 1
  }

cloud9 :: CardDef
cloud9 = (ally "29014" ("Cloud 9" <:> "Abby Boylen") 3 [Aerial, Champion] [Mental] Leadership)
  { cdDeckLimit = Just 1
  }

falconJoaquinTorres :: CardDef
falconJoaquinTorres = (ally "29015" ("Falcon" <:> "Joaquin Torres") 4 [Aerial, Champion] [Energy] Leadership)
  { cdDeckLimit = Just 1
  }

patriotRayshaunLucas :: CardDef
patriotRayshaunLucas = (ally "29016" ("Patriot" <:> "Rayshaun Lucas") 3 [Champion] [Physical] Leadership)
  { cdDeckLimit = Just 1
  }

snowguardAmkaAliyak :: CardDef
snowguardAmkaAliyak = (basicAlly "29023" ("Snowguard" <:> "Amka Aliyak") 4 [Champion] [Energy])
  { cdDeckLimit = Just 1
  }

vivianIronheart :: CardDef
vivianIronheart = (basicAlly "29024" "Vivian" 2 [Aerial, Android, Champion] [Mental])
  { cdDeckLimit = Just 1
  }

bombshellLanaBaumgartner :: CardDef
bombshellLanaBaumgartner = (ally "29033" ("Bombshell" <:> "Lana Baumgartner") 4 [Champion] [Energy] Aggression)
  { cdDeckLimit = Just 1
  , cdCriteria = SelfMatches (IdentityWithTrait Champion)
  }

waspNadiaVanDyneIronheart :: CardDef
waspNadiaVanDyneIronheart = (ally "29034" ("Wasp" <:> "Nadia Van Dyne") 2 [Champion] [Physical] Justice)
  { cdDeckLimit = Just 1
  , cdCriteria = SelfMatches (IdentityWithTrait Champion)
  }

pinpointQureshiGupta :: CardDef
pinpointQureshiGupta = (ally "29035" ("Pinpoint" <:> "Qureshi Gupta") 2 [Champion] [Mental] Protection)
  { cdDeckLimit = Just 1
  , cdCriteria = SelfMatches (IdentityWithTrait Champion)
  }


-- Spider-Ham

captainAmericat :: CardDef
captainAmericat = (identityAlly "30002" ("Captain Americat" <:> "Steve Mouser") 3 [Cartoon] [Wild])
  { cdDeckLimit = Just 1
  , cdUnique = True
  }

ladySpider :: CardDef
ladySpider = (ally "30012" ("Lady Spider" <:> "May Reilly") 4 [WebWarrior] [Mental] Justice)
  { cdDeckLimit = Just 1
  , cdUnique = True
  }

spiderManPavitrPrabhakar :: CardDef
spiderManPavitrPrabhakar = (ally "30013" ("Spider-Man" <:> "Pavitr Prabhakar") 3 [WebWarrior] [Energy] Justice)
  { cdDeckLimit = Just 1
  , cdUnique = True
  }

scarletSpider :: CardDef
scarletSpider = (basicAlly "30020" ("Scarlet Spider" <:> "Kaine") 4 [WebWarrior] [Mental])
  { cdDeckLimit = Just 1
  , cdUnique = True
  , cdCriteria = ControlsCardsWithTrait WebWarrior 1
  }

spDr :: CardDef
spDr = (basicAlly "30021" ("SP//dr" <:> "Peni Parker") 2 [WebWarrior] [Energy])
  { cdDeckLimit = Just 1
  , cdUnique = True
  , cdCriteria = ControlsCardsWithTrait WebWarrior 1
  }


-- SP//dr

venmAddyBrock :: CardDef
venmAddyBrock = (identityAlly "31003" ("VEN#m" <:> "Addy Brock") 4 [] [Energy])
  { cdUnique = True
  , cdDeckLimit = Just 1
  }

daredevilSpdr :: CardDef
daredevilSpdr = (ally "31014" "Daredevil" 2 [] [Physical] Protection)
  { cdUnique = True
  , cdDeckLimit = Just 1
  }

spiderManNoir :: CardDef
spiderManNoir = (ally "31015" "Spider-Man Noir" 3 [WebWarrior] [Energy] Protection)
  { cdUnique = True
  , cdDeckLimit = Just 1
  }

spiderHamPeterPorker :: CardDef
spiderHamPeterPorker = (basicAlly "31021" ("Spider-Ham" <:> "Peter Porker") 3 [Cartoon, WebWarrior] [Physical])
  { cdUnique = True
  , cdDeckLimit = Just 1
  , cdCriteria = ControlsCardsWithTrait WebWarrior 1
  }

spiderManOttoOctavius :: CardDef
spiderManOttoOctavius = (basicAlly "31022" ("Spider-Man" <:> "Otto Octavius") 2 [WebWarrior] [Mental])
  { cdUnique = True
  , cdDeckLimit = Just 1
  , cdCriteria = ControlsCardsWithTrait WebWarrior 1
  }

-- Mutant Genesis

shadowcatKittyPryde :: CardDef
shadowcatKittyPryde = (identityAlly "32002" ("Shadowcat" <:> "Kitty Pryde") 3 [XMen] [Wild])
  { cdUnique = True
  , cdDeckLimit = Just 1
  }

-- Mutant Genesis

nightcrawler :: CardDef
nightcrawler = (ally "32011" ("Nightcrawler" <:> "Kurt Wagner") 3 [XMen] [Physical] Protection)
  { cdUnique = True
  , cdDeckLimit = Just 1
  }

-- Mutant Genesis

polaris :: CardDef
polaris = (ally "32012" ("Polaris" <:> "Lorna Dane") 3 [XMen] [Energy] Protection)
  { cdUnique = True
  , cdDeckLimit = Just 1
  }

-- Mutant Genesis

professorX :: CardDef
professorX = reprintedAs ["37017"] $ (basicAlly "32019" ("Professor X" <:> "Charles Xavier") 3 [Psionic, XMen] [Mental])
  { cdUnique = True
  , cdDeckLimit = Just 1
  }

-- Mutant Genesis

lockheed :: CardDef
lockheed = (identityAlly "32032" "Lockheed" 2 [Dragon, XMen] [Wild])
  { cdUnique = True
  , cdDeckLimit = Just 1
  }

-- Mutant Genesis

wolverine :: CardDef
wolverine = (ally "32041" ("Wolverine" <:> "Logan") 4 [XMen] [Physical] Aggression)
  { cdUnique = True
  , cdDeckLimit = Just 1
  }

-- Mutant Genesis

magik :: CardDef
magik = (ally "32042" ("Magik" <:> "Illyana Rasputin") 3 [Mystic, XMen] [Energy] Aggression)
  { cdUnique = True
  , cdDeckLimit = Just 1
  }

-- Mutant Genesis

colossusPiotrRasputin :: CardDef
colossusPiotrRasputin = reprintedAs ["35021"] $ (basicAlly "32048" ("Colossus" <:> "Piotr Rasputin") 4 [XMen] [Physical])
  { cdUnique = True
  , cdDeckLimit = Just 1
  , cdKeywords = fromList [Toughness]
  }

mgCampaignMagneto :: CardDef
mgCampaignMagneto = (basicAlly "32172b" ("Magneto" <:> "Erik Lehnsherr") 0 [MGTrait.BrotherhoodOfMutants] [MGResource.Wild]) {cdEncounterSet = Just MGSet.MutantGenesisCampaign, cdEncounterSetQuantity = Just 1, cdKeywords = fromList [MGKeyword.Victory 1]}

-- Mutant Genesis scenario definitions
mgRobertKelly :: CardDef
mgRobertKelly = (baseAlly "32066" ("Robert Kelly") 0 [] [] Nothing)
  { cdTraits = fromList [MGTrait.Senator]
  , cdUnique = True
  , cdEncounterSet = Just MGSet.Sabretooth
  , cdEncounterSetQuantity = Just 0
  , cdBoostIcons = []
  , cdKeywords = fromList []
  , cdResources = []
  }

mgJubilee :: CardDef
mgJubilee = (baseAlly "32088b" ("Jubilee" <:> "Jubilation Lee") 0 [] [] Nothing)
  { cdTraits = fromList [MGTrait.XMen]
  , cdUnique = True
  , cdEncounterSet = Just MGSet.ProjectWideawake
  , cdEncounterSetQuantity = Just 0
  , cdBoostIcons = []
  , cdKeywords = fromList [MGKeyword.Victory (-1)]
  , cdResources = [(PrintedResource, Wild)]
  }

mgRictor :: CardDef
mgRictor = (baseAlly "32089" ("Rictor" <:> "Julio Richter") 2 [] [] Nothing)
  { cdTraits = fromList [MGTrait.Captive, MGTrait.XMen]
  , cdUnique = True
  , cdEncounterSet = Just MGSet.ProjectWideawake
  , cdEncounterSetQuantity = Just 0
  , cdBoostIcons = []
  , cdKeywords = fromList []
  , cdResources = [(PrintedResource, Wild)]
  }

mgBoomBoom :: CardDef
mgBoomBoom = (baseAlly "32090" ("Boom Boom" <:> "Tabitha Smith") 2 [] [] Nothing)
  { cdTraits = fromList [MGTrait.Captive, MGTrait.XMen]
  , cdUnique = True
  , cdEncounterSet = Just MGSet.ProjectWideawake
  , cdEncounterSetQuantity = Just 0
  , cdBoostIcons = []
  , cdKeywords = fromList []
  , cdResources = [(PrintedResource, Wild)]
  }

mgCannonball :: CardDef
mgCannonball = (baseAlly "32091" ("Cannonball" <:> "Sam Guthrie") 2 [] [] Nothing)
  { cdTraits = fromList [MGTrait.Captive, MGTrait.XMen]
  , cdUnique = True
  , cdEncounterSet = Just MGSet.ProjectWideawake
  , cdEncounterSetQuantity = Just 0
  , cdBoostIcons = []
  , cdKeywords = fromList []
  , cdResources = [(PrintedResource, Wild)]
  }

mgWolfsbane :: CardDef
mgWolfsbane = (baseAlly "32092" ("Wolfsbane" <:> "Rahne Sinclair") 2 [] [] Nothing)
  { cdTraits = fromList [MGTrait.Captive, MGTrait.XMen]
  , cdUnique = True
  , cdEncounterSet = Just MGSet.ProjectWideawake
  , cdEncounterSetQuantity = Just 0
  , cdBoostIcons = []
  , cdKeywords = fromList []
  , cdResources = [(PrintedResource, Wild)]
  }

-- Wolverine

jubilee :: CardDef
jubilee = (identityAlly "35003" ("Jubilee" <:> "Jubilation Lee") 2 [XMen] [Wild]) {cdUnique = True, cdDeckLimit = Just 1}

psylocke :: CardDef
psylocke = (ally "35013" ("Psylocke" <:> "Betsy Braddock") 4 [Psionic, XMen] [Mental] Aggression) {cdUnique = True, cdDeckLimit = Just 1}

sunfire :: CardDef
sunfire = (ally "35014" ("Sunfire" <:> "Shiro Yoshida") 2 [XMen] [Energy] Aggression) {cdUnique = True, cdDeckLimit = Just 1}

longshot :: CardDef
longshot = (basicAlly "35033" "Longshot" 4 [XMen] [Mental]) {cdUnique = True, cdDeckLimit = Just 1, cdCriteria = SelfMatches (IdentityWithTrait XMen)}


-- Phoenix

cyclopsScottSummers :: CardDef
cyclopsScottSummers = (identityAlly "34003" ("Cyclops" <:> "Scott Summers") 3 [XMen] [Wild]) {cdDeckLimit = Just 1}

banshee :: CardDef
banshee = (ally "34014" ("Banshee" <:> "Sean Cassidy") 4 [Aerial, XMen] [Energy] Justice) {cdDeckLimit = Just 1}

marvelGirl :: CardDef
marvelGirl = (ally "34015" ("Marvel Girl" <:> "Rachel Summers") 3 [Psionic, XMen] [Mental] Justice) {cdDeckLimit = Just 1}

stormOroroMunroe :: CardDef
stormOroroMunroe = (basicAlly "34021" ("Storm" <:> "Ororo Munroe") 5 [Aerial, XMen] [Energy]) {cdDeckLimit = Just 1}


-- Phoenix

controlledMinion :: CardDef
controlledMinion = basicAlly "xcontrolledminion" "Controlled minion" 0 [] []


-- Cyclops

phoenixJeanGrey :: CardDef
phoenixJeanGrey = (identityAlly "33002" ("Phoenix" <:> "Jean Grey") 3 [Psionic, XMen] [Wild]) { cdDeckLimit = Just 1, cdUnique = True }

beast :: CardDef
beast = (ally "33011" ("Beast" <:> "Hank McCoy") 4 [Genius, XMen] [Mental] Leadership) { cdDeckLimit = Just 1, cdUnique = True }

dust :: CardDef
dust = (ally "33012" ("Dust" <:> "Sooraya Qadir") 3 [XMen] [Energy] Aggression) { cdDeckLimit = Just 1, cdUnique = True }

rockslide :: CardDef
rockslide = (ally "33013" ("Rockslide" <:> "Santo Vaccarro") 4 [XMen] [Physical] Protection) { cdDeckLimit = Just 1, cdUnique = True, cdKeywords = fromList [Retaliate 1] }

blindfold :: CardDef
blindfold = (ally "33014" ("Blindfold" <:> "Ruth Aldine") 3 [Psionic, XMen] [Physical] Justice) { cdDeckLimit = Just 1, cdUnique = True }

angel :: CardDef
angel = (basicAlly "33019" ("Angel" <:> "Warren Worthington III") 3 [Aerial, XMen] [Mental]) { cdDeckLimit = Just 1, cdUnique = True }


-- Storm

havok :: CardDef
havok = (ally "36014" ("Havok" <:> "Alex Summers") 4 [XMen] [Energy] Leadership) { cdUnique = True, cdDeckLimit = Just 1 }

mirage :: CardDef
mirage = (ally "36015" ("Mirage" <:> "Dani Moonstar") 3 [Psionic, XMen] [Mental] Leadership) { cdUnique = True, cdDeckLimit = Just 1 }

gentle :: CardDef
gentle = (ally "36016" ("Gentle" <:> "Nezhno Abidemi") 3 [XMen] [Physical] Leadership) { cdUnique = True, cdDeckLimit = Just 1 }

pixie :: CardDef
pixie = (ally "36017" ("Pixie" <:> "Megan Gwynn") 2 [Aerial, XMen] [Energy] Leadership) { cdUnique = True, cdDeckLimit = Just 1 }

forge :: CardDef
forge = (basicAlly "36022" "Forge" 2 [XMen] [Mental]) { cdUnique = True, cdDeckLimit = Just 1 }

-- Gambit

rogueGambit :: CardDef
rogueGambit = (identityAlly "37002" ("Rogue" <:> "Anna Marie") 4 [Aerial, XMen] [Wild]) { cdDeckLimit = Just 1, cdUnique = True, cdKeywords = fromList [Toughness] }

bishop :: CardDef
bishop = (ally "37011" ("Bishop" <:> "Lucas Bishop") 3 [XMen] [Energy] Justice) { cdDeckLimit = Just 1, cdUnique = True }

dazzler :: CardDef
dazzler = (ally "37012" ("Dazzler" <:> "Alison Blaire") 4 [XMen] [Energy] Justice) { cdDeckLimit = Just 1, cdUnique = True }


-- Rogue

gambitRogue :: CardDef
gambitRogue = (identityAlly "38003" ("Gambit" <:> "Remy LeBeau") 3 [XMen] [Wild]) { cdUnique = True, cdDeckLimit = Just 1 }

iceman :: CardDef
iceman = (ally "38010" ("Iceman" <:> "Bobby Drake") 3 [XMen] [Mental] Protection) { cdUnique = True, cdDeckLimit = Just 1 }

karma :: CardDef
karma = (ally "38011" ("Karma" <:> "Xi'an Coy Manh") 4 [Psionic, XMen] [Energy] Protection) { cdUnique = True, cdDeckLimit = Just 1 }

armor :: CardDef
armor = (ally "38012" ("Armor" <:> "Hisako Ichiki") 2 [XMen] [Physical] Protection) { cdUnique = True, cdDeckLimit = Just 1, cdKeywords = fromList [Toughness], cdCriteria = SelfMatches (IdentityWithTrait XMen) }


-- Mojo Mania

mojoCardDef :: CardCode -> Name -> MojoSet.EncounterSet -> Natural -> CardDef
mojoCardDef code name set quantity = CardDef
  { cdCardCode = code
  , cdName = name
  , cdCost = Nothing
  , cdTraits = mempty
  , cdKeywords = mempty
  , cdCardType = AllyType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdEncounterSet = Just set
  , cdEncounterSetQuantity = Just quantity
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
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
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = NoCost
  }

mojo39071 :: CardDef
mojo39071 = (mojoCardDef "39071" "Longshot" MojoSet.LongshotSet 1)
  { cdTraits = fromList [MojoTrait.XMen]
  , cdUnique = True
  , cdKeywords = fromList [MojoKeyword.Surge]
  , cdBoostIcons = []
  }

-- NeXt Evolution: Cable

cable40014 :: CardDef
cable40014 = (ally "40014" "Caliban" 3 [NE.Psionic, NE.XForce] [NE.Mental] NE.Leadership) { cdUnique = True, cdDeckLimit = Just 1 }

cable40015 :: CardDef
cable40015 = (ally "40015" ("Fantomex" <:> "Charlie Cluster-7") 4 [NE.Psionic, NE.XForce] [NE.Energy] NE.Leadership) { cdUnique = True, cdDeckLimit = Just 1 }

cable40016 :: CardDef
cable40016 = (ally "40016" ("Sunspot" <:> "Bobby Da Costa") 3 [NE.Aerial, NE.XForce] [NE.Energy] NE.Leadership) { cdUnique = True, cdDeckLimit = Just 1 }

cable40024 :: CardDef
cable40024 = (basicAlly "40024" ("Deadpool" <:> "Wade Wilson") 3 [NE.Mercenary, NE.XForce] [NE.Wild]) { cdUnique = True, cdDeckLimit = Just 1 }

cable40025 :: CardDef
cable40025 = (basicAlly "40025" ("Deathlok" <:> "Unit L17") 4 [NE.Cyborg, NE.XForce] [NE.Physical]) { cdUnique = True, cdDeckLimit = Just 1 }

cable40204 :: CardDef
cable40204 = (basicAlly "40204" "Hope Summers" 4 [NE.XForce] [NE.Mental]) { cdUnique = True, cdDeckLimit = Just 1 }

-- NeXt Evolution: Domino
diamondback :: CardDef
diamondback = CardDef
  { cdCardCode = "40038", cdName = "Diamondback" <:> "Rachel Leighton", cdCost = Just 2
  , cdTraits = fromList [NET.Posse], cdKeywords = fromList []
  , cdCardType = AllyType, cdAbilityType = Nothing, cdAbilitySubTypes = []
  , cdUnique = True, cdAspect = Nothing, cdCriteria = NoCriteria
  , cdResources = [(PrintedResource, NER.Wild)], cdBoostIcons = []
  , cdEncounterSet = Nothing, cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing, cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 1, cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False, cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False, cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = NoCost
  }

-- NeXt Evolution: Domino
outlaw :: CardDef
outlaw = CardDef
  { cdCardCode = "40039", cdName = "Outlaw" <:> "Inez Temple", cdCost = Just 3
  , cdTraits = fromList [NET.Outlaw, NET.Posse], cdKeywords = fromList [NEK.Toughness]
  , cdCardType = AllyType, cdAbilityType = Nothing, cdAbilitySubTypes = []
  , cdUnique = True, cdAspect = Nothing, cdCriteria = NoCriteria
  , cdResources = [(PrintedResource, NER.Wild)], cdBoostIcons = []
  , cdEncounterSet = Nothing, cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing, cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 1, cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False, cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False, cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = NoCost
  }

-- NeXt Evolution: Domino
feral :: CardDef
feral = CardDef
  { cdCardCode = "40050", cdName = "Feral" <:> "Maria Callasantos", cdCost = Just 4
  , cdTraits = fromList [NET.XForce], cdKeywords = fromList []
  , cdCardType = AllyType, cdAbilityType = Nothing, cdAbilitySubTypes = []
  , cdUnique = True, cdAspect = Just NEA.Justice, cdCriteria = NoCriteria
  , cdResources = [(PrintedResource, NER.Energy)], cdBoostIcons = []
  , cdEncounterSet = Nothing, cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing, cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 1, cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False, cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False, cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = NoCost
  }

-- NeXt Evolution: Domino
wolfsbane :: CardDef
wolfsbane = CardDef
  { cdCardCode = "40051", cdName = "Wolfsbane" <:> "Rahne Sinclair", cdCost = Just 3
  , cdTraits = fromList [NET.XForce], cdKeywords = fromList []
  , cdCardType = AllyType, cdAbilityType = Nothing, cdAbilitySubTypes = []
  , cdUnique = True, cdAspect = Just NEA.Justice, cdCriteria = NoCriteria
  , cdResources = [(PrintedResource, NER.Physical)], cdBoostIcons = []
  , cdEncounterSet = Nothing, cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing, cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 1, cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False, cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False, cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = NoCost
  }

-- NeXt Evolution: Domino
atlasBear :: CardDef
atlasBear = CardDef
  { cdCardCode = "40056", cdName = "Atlas Bear" <:> "Shoon'kwa", cdCost = Just 3
  , cdTraits = fromList [NET.Posse, NET.Wakanda], cdKeywords = fromList []
  , cdCardType = AllyType, cdAbilityType = Nothing, cdAbilitySubTypes = []
  , cdUnique = True, cdAspect = Just NEA.Neutral, cdCriteria = NoCriteria
  , cdResources = [(PrintedResource, NER.Mental)], cdBoostIcons = []
  , cdEncounterSet = Nothing, cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing, cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 1, cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False, cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False, cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = NoCost
  }

-- NeXt Evolution: Domino
whiteFox :: CardDef
whiteFox = CardDef
  { cdCardCode = "40057", cdName = "White Fox" <:> "Ami Han", cdCost = Just 3
  , cdTraits = fromList [NET.Posse], cdKeywords = fromList []
  , cdCardType = AllyType, cdAbilityType = Nothing, cdAbilitySubTypes = []
  , cdUnique = True, cdAspect = Just NEA.Neutral, cdCriteria = NoCriteria
  , cdResources = [(PrintedResource, NER.Energy)], cdBoostIcons = []
  , cdEncounterSet = Nothing, cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing, cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 1, cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False, cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False, cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = NoCost
  }

-- NeXt Evolution campaign and scenarios

ne40079 :: CardDef
ne40079 = CardDef
  { cdCardCode = "40079"
  , cdName = "Morlock"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Morlock, NETrait.Mutant]
  , cdKeywords = fromList [NEKeyword.Victory (-1)]
  , cdCardType = AllyType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = [(PrintedResource, NEResource.Mental)]
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.MorlockSiege
  , cdEncounterSetQuantity = Just 4
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
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

ne40130 :: CardDef
ne40130 = CardDef
  { cdCardCode = "40130"
  , cdName = "Hope Summers"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Psionic, NETrait.XForce, NETrait.XMen]
  , cdKeywords = fromList [NEKeyword.Setup]
  , cdCardType = AllyType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = [(PrintedResource, NEResource.Mental)]
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.HopeSummersSet
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
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

-- Psylocke

angelPsylocke :: CardDef
angelPsylocke = CardDef
  { cdCardCode = "41003", cdName = ("Angel" PsyName.<:> "Warren Worthington III"), cdCost = Just 3
  , cdTraits = fromList [PsyTrait.Aerial, PsyTrait.XForce], cdKeywords = fromList []
  , cdCardType = PsyCard.AllyType, cdAbilityType = Nothing, cdAbilitySubTypes = []
  , cdUnique = True, cdAspect = Nothing, cdCriteria = PsyCriteria.NoCriteria
  , cdResources = [(PsyCard.PrintedResource, PsyResource.Wild)], cdBoostIcons = []
  , cdEncounterSet = Nothing, cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 1, cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False, cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False, cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = PsyCost.NoCost
  }

captainBritain :: CardDef
captainBritain = CardDef
  { cdCardCode = "41012", cdName = ("Captain Britain" PsyName.<:> "Brian Braddock"), cdCost = Just 4
  , cdTraits = fromList [PsyTrait.Aerial, PsyTrait.Excalibur], cdKeywords = fromList []
  , cdCardType = PsyCard.AllyType, cdAbilityType = Nothing, cdAbilitySubTypes = []
  , cdUnique = True, cdAspect = Just PsyAspect.Justice, cdCriteria = PsyCriteria.NoCriteria
  , cdResources = [(PsyCard.PrintedResource, PsyResource.Mental)], cdBoostIcons = []
  , cdEncounterSet = Nothing, cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 1, cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False, cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False, cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = PsyCost.NoCost
  }

cypher :: CardDef
cypher = CardDef
  { cdCardCode = "41013", cdName = ("Cypher" PsyName.<:> "Doug Ramsey"), cdCost = Just 2
  , cdTraits = fromList [PsyTrait.XForce], cdKeywords = fromList []
  , cdCardType = PsyCard.AllyType, cdAbilityType = Nothing, cdAbilitySubTypes = []
  , cdUnique = True, cdAspect = Just PsyAspect.Justice, cdCriteria = PsyCriteria.NoCriteria
  , cdResources = [(PsyCard.PrintedResource, PsyResource.Energy)], cdBoostIcons = []
  , cdEncounterSet = Nothing, cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 1, cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False, cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False, cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = PsyCost.NoCost
  }

peteWisdom :: CardDef
peteWisdom = CardDef
  { cdCardCode = "41018", cdName = "Pete Wisdom", cdCost = Just 4
  , cdTraits = fromList [PsyTrait.XForce], cdKeywords = fromList []
  , cdCardType = PsyCard.AllyType, cdAbilityType = Nothing, cdAbilitySubTypes = []
  , cdUnique = True, cdAspect = Just PsyAspect.Neutral, cdCriteria = PsyCriteria.SelfMatches (PsyMatch.IdentityWithTrait PsyTrait.XForce)
  , cdResources = [(PsyCard.PrintedResource, PsyResource.Mental)], cdBoostIcons = []
  , cdEncounterSet = Nothing, cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 1, cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False, cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False, cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = PsyCost.NoCost
  }

dominoPsylocke :: CardDef
dominoPsylocke = CardDef
  { cdCardCode = "41031", cdName = ("Domino" PsyName.<:> "Neena Thurman"), cdCost = Just 3
  , cdTraits = fromList [PsyTrait.Posse, PsyTrait.XForce], cdKeywords = fromList []
  , cdCardType = PsyCard.AllyType, cdAbilityType = Nothing, cdAbilitySubTypes = []
  , cdUnique = True, cdAspect = Just PsyAspect.Leadership, cdCriteria = PsyCriteria.NoCriteria
  , cdResources = [(PsyCard.PrintedResource, PsyResource.Wild)], cdBoostIcons = []
  , cdEncounterSet = Nothing, cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 1, cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False, cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False, cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = PsyCost.NoCost
  }

-- Angel

psylockeAngel :: CardDef
psylockeAngel = CardDef
  { cdCardCode = "42002"
  , cdName = AnName.Name "Psylocke" (Just "Betsy Braddock")
  , cdCost = Just 3
  , cdTraits = fromList [AnTrait.Psionic, AnTrait.XForce]
  , cdKeywords = fromList []
  , cdCardType = AnCard.AllyType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = AnCriteria.NoCriteria
  , cdResources = [(AnCard.PrintedResource, AnResource.Wild)]
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
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = AnCost.NoCost
  }

elixir :: CardDef
elixir = CardDef
  { cdCardCode = "42011"
  , cdName = AnName.Name "Elixir" (Just "Josh Foley")
  , cdCost = Just 4
  , cdTraits = fromList [AnTrait.XForce]
  , cdKeywords = fromList []
  , cdCardType = AnCard.AllyType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Just AnAspect.Protection
  , cdCriteria = AnCriteria.AnyCriteria [AnCriteria.SelfMatches (AnMatch.IdentityWithTrait AnTrait.XForce), AnCriteria.SelfMatches (AnMatch.IdentityWithTrait AnTrait.XMen)]
  , cdResources = [(AnCard.PrintedResource, AnResource.Mental)]
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
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = AnCost.NoCost
  }

siryn :: CardDef
siryn = CardDef
  { cdCardCode = "42012"
  , cdName = AnName.Name "Siryn" (Just "Theresa Cassidy")
  , cdCost = Just 4
  , cdTraits = fromList [AnTrait.Aerial, AnTrait.XForce]
  , cdKeywords = fromList []
  , cdCardType = AnCard.AllyType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Just AnAspect.Protection
  , cdCriteria = AnCriteria.NoCriteria
  , cdResources = [(AnCard.PrintedResource, AnResource.Energy)]
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
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = AnCost.NoCost
  }

warpath :: CardDef
warpath = CardDef
  { cdCardCode = "42013"
  , cdName = AnName.Name "Warpath" (Just "James Proudstar")
  , cdCost = Just 4
  , cdTraits = fromList [AnTrait.Aerial, AnTrait.XForce]
  , cdKeywords = fromList [AnKeyword.Toughness]
  , cdCardType = AnCard.AllyType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Just AnAspect.Protection
  , cdCriteria = AnCriteria.NoCriteria
  , cdResources = [(AnCard.PrintedResource, AnResource.Energy)]
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
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = AnCost.NoCost
  }

cannonball :: CardDef
cannonball = CardDef
  { cdCardCode = "42020"
  , cdName = AnName.Name "Cannonball" (Just "Sam Guthrie")
  , cdCost = Just 3
  , cdTraits = fromList [AnTrait.Aerial, AnTrait.XForce]
  , cdKeywords = fromList []
  , cdCardType = AnCard.AllyType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Just AnAspect.Neutral
  , cdCriteria = AnCriteria.NoCriteria
  , cdResources = [(AnCard.PrintedResource, AnResource.Physical)]
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
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = AnCost.NoCost
  }
