{-# OPTIONS_GHC -Wno-orphans #-}
module Marvel.Ally



  ( module Marvel.Ally
  , module X
  ) where

import Marvel.Ally.Allies.AgeOfApocalypseBasicCampaign
import Marvel.Ally.Allies.NextEvolutionMorlockSiege
import Marvel.Ally.Allies.NextEvolutionHopeSummersSet

import Marvel.Ally.Allies.MutantGenesisCampaign
import Marvel.Ally.Allies.MissionAlly (MissionAlly(..))
import Marvel.Prelude
import Data.Aeson.Types (Parser)

import Marvel.Ally.Allies
import Marvel.Ally.Types
import Marvel.Ally.Types as X (Ally)
import Marvel.Id

instance FromJSON Ally where
  parseJSON = withObject "Ally" $ \o -> do
    cardDef <- o .: "allyCardDef"
    originalMinion <- o .:? "allyOriginalMinion" :: Parser (Maybe Value)
    mission <- o .:? "missionAlly" .!= False
    if mission then Ally . MissionAlly <$> parseJSON (Object o) else withAllyCardCode (if isJust originalMinion then "xcontrolledminion" else cdCardCode cardDef)
      $ \(_ :: AllyCard a) -> Ally <$> parseJSON @a (Object o)

withAllyCardCode :: CardCode -> (forall a . IsAlly a => AllyCard a -> r) -> r
withAllyCardCode cCode f = case lookup cCode allAllies of
  Nothing -> error "invalid ally"
  Just (SomeAllyCard a) -> f a

lookupAlly :: CardCode -> IdentityId -> AllyId -> Ally
lookupAlly cardCode identityId allyId = case lookup cardCode allAllies of
  Just (SomeAllyCard a) -> Ally $ buildCard a (identityId, allyId)
  Nothing -> error $ "Invalid card code for ally " <> show cardCode

allAllies :: HashMap CardCode SomeAllyCard
allAllies = fromList $ map
  (toFst someAllyCardCode)
  $ concatMap (\(SomeAllyCard a) -> SomeAllyCard <$> printings a)
  [ SomeAllyCard destiny, SomeAllyCard blink, SomeAllyCard morph, SomeAllyCard xMan
  , SomeAllyCard triage
  , SomeAllyCard tempus
  , SomeAllyCard goldballs
  , SomeAllyCard colossusMagik
  , SomeAllyCard malcolm, SomeAllyCard randall, SomeAllyCard cableBishop, SomeAllyCard x23Bishop, SomeAllyCard legionBishop, SomeAllyCard marrow
  , SomeAllyCard cableDeadpool, SomeAllyCard dogpool, SomeAllyCard headpool, SomeAllyCard kidpool, SomeAllyCard ladyDeadpool, SomeAllyCard bobAgentOfHydra, SomeAllyCard negasonicTeenageWarhead, SomeAllyCard pandapool
  , SomeAllyCard honeyBadger, SomeAllyCard boomBoom, SomeAllyCard rictor, SomeAllyCard shatterstar
  , SomeAllyCard psylockeAngel, SomeAllyCard elixir, SomeAllyCard siryn, SomeAllyCard warpath, SomeAllyCard cannonball
  , SomeAllyCard angelPsylocke, SomeAllyCard captainBritain, SomeAllyCard cypher, SomeAllyCard peteWisdom, SomeAllyCard dominoPsylocke
  , SomeAllyCard ne40079, SomeAllyCard ne40130
  , SomeAllyCard whiteFox
  , SomeAllyCard atlasBear
  , SomeAllyCard wolfsbane
  , SomeAllyCard feral
  , SomeAllyCard outlaw
  , SomeAllyCard diamondback
  , SomeAllyCard cable40014, SomeAllyCard cable40015, SomeAllyCard cable40016, SomeAllyCard cable40024, SomeAllyCard cable40025, SomeAllyCard cable40204
  , SomeAllyCard mojo39071, SomeAllyCard gambitRogue, SomeAllyCard iceman, SomeAllyCard karma, SomeAllyCard armor, SomeAllyCard rogueGambit, SomeAllyCard bishop, SomeAllyCard dazzler
  , SomeAllyCard havok
  , SomeAllyCard mirage
  , SomeAllyCard gentle
  , SomeAllyCard pixie
  , SomeAllyCard forge
  , SomeAllyCard phoenixJeanGrey, SomeAllyCard beast, SomeAllyCard dust, SomeAllyCard rockslide, SomeAllyCard blindfold, SomeAllyCard angel
  , SomeAllyCard jubilee, SomeAllyCard psylocke, SomeAllyCard sunfire, SomeAllyCard longshot
  , SomeAllyCard mgRobertKelly, SomeAllyCard mgJubilee, SomeAllyCard mgRictor, SomeAllyCard mgBoomBoom, SomeAllyCard mgCannonball, SomeAllyCard mgWolfsbane, SomeAllyCard mgCampaignMagneto
  , SomeAllyCard blackCatFeliciaHardy
  , SomeAllyCard spiderWomanJessicaDrew
  , SomeAllyCard hellcatPatsyWalker
  , SomeAllyCard warMachineJamesRhodes
  , SomeAllyCard shuri
  , SomeAllyCard hulkBruceBanner
  , SomeAllyCard tigraGreerGrantNelson
  , SomeAllyCard daredevilMattMurdock
  , SomeAllyCard jessicaJones
  , SomeAllyCard hawkeyeClintBarton
  , SomeAllyCard mariaHill
  , SomeAllyCard vision
  , SomeAllyCard blackWidowNatashaRomanoff
  , SomeAllyCard lukeCage
  , SomeAllyCard mockingbirdBobbiMorse
  , SomeAllyCard nickFury
  , SomeAllyCard agent13SharonCarter
  , SomeAllyCard falconSamWilson
  , SomeAllyCard squirrelGirlDoreenGreen
  , SomeAllyCard wonderManSimonWilliams
  , SomeAllyCard redDagger
  , SomeAllyCard novaSamAlexander
  , SomeAllyCard lockjaw
  , SomeAllyCard winterSoldierBuckyBarnes
  , SomeAllyCard agentCoulson
  , SomeAllyCard quakeDaisyJohnson
  , SomeAllyCard ladySif
  , SomeAllyCard hercules
  , SomeAllyCard valkyrie
  , SomeAllyCard heimdall
  , SomeAllyCard wong
  , SomeAllyCard brotherVoodooJerichoDrumm
  , SomeAllyCard clea
  , SomeAllyCard ironFistDannyRand
  , SomeAllyCard ironManTonyStark
  , SomeAllyCard brawn
  , SomeAllyCard sentryRobertReynolds
  , SomeAllyCard sheHulkJenniferWalters
  -- The Green Goblin

  -- The Wrecking Crew

  -- The Rise of Red Skull
  , SomeAllyCard mockingbird
  , SomeAllyCard hawkeyeKateBishop
  , SomeAllyCard blackKnightDaneWhitman
  , SomeAllyCard goliathBillFoster
  , SomeAllyCard usAgentJohnWalker
  , SomeAllyCard warMachine
  , SomeAllyCard captainMarvelCarolDanvers
  , SomeAllyCard spiderGirlAnyaCorazon
  , SomeAllyCard spiderManPeterParker

  -- Taskmaster
  , SomeAllyCard moonKnightMarcSpector
  , SomeAllyCard shangChi
  , SomeAllyCard whiteTigerAngelaDelToro
  , SomeAllyCard elektra

  -- The Once and Future Kang


  -- Ant-Man
  , SomeAllyCard waspNadiaVanDyne
  , SomeAllyCard antManHankPym
  , SomeAllyCard giantMan
  , SomeAllyCard ronin
  , SomeAllyCard stingerCassieLang

  -- Wasp
  , SomeAllyCard antManScottLang
  , SomeAllyCard thorJaneFoster
  , SomeAllyCard waspJanetVanDyne
  , SomeAllyCard ironheart
  , SomeAllyCard spiderManMilesMorales

  -- Quicksilver
  , SomeAllyCard scarletWitchWandaMaximoff
  , SomeAllyCard multipleMan
  , SomeAllyCard warlock

  -- Scarlet Witch
  , SomeAllyCard quicksilverPietroMaximoff
  , SomeAllyCard speedThomasShepherd
  , SomeAllyCard wiccanWilliamKaplan

  -- Star-Lord
  , SomeAllyCard novaPrimeRichardRider
  , SomeAllyCard adamWarlock
  , SomeAllyCard betaRayBill
  , SomeAllyCard yondu
  , SomeAllyCard cosmo

  -- Gamora
  , SomeAllyCard nebulaAlly
  , SomeAllyCard angelaAldrifOdinsdottir
  , SomeAllyCard drax

  -- Groot
  , SomeAllyCard starhawkStakarOgord
  , SomeAllyCard rocketRaccoonAlly

  -- Rocket Raccoon
  , SomeAllyCard bug
  , SomeAllyCard grootAlly
  -- Drax
  , SomeAllyCard mantis
  , SomeAllyCard martyrPhylaVell
  , SomeAllyCard moondragonHeatherDouglas
  , SomeAllyCard gamoraAlly
  -- Venom
  , SomeAllyCard jackFlagJackHarrison
  , SomeAllyCard starLordPeterQuill
  -- Spectrum
  , SomeAllyCard blueMarvelAdamBrashear
  , SomeAllyCard captainAmericaSteveRogers
  , SomeAllyCard powerManVictorAlvarez
  , SomeAllyCard whiteTigerAvaAyala
  , SomeAllyCard kaluu
  , SomeAllyCard bladeEricBrooks
  -- Adam Warlock
  , SomeAllyCard pipTheTroll
  , SomeAllyCard marvelBoyNohVarr
  , SomeAllyCard quasarWendellVaughn
  , SomeAllyCard majorVictoryVanceAstro
  , SomeAllyCard charlie27
  , SomeAllyCard martinexTNaga
  -- The Mad Titan's Shadow: Hela
  , SomeAllyCard odinCaptive
  , SomeAllyCard odinKing
  -- The Mad Titan's Shadow: the campaign set
  , SomeAllyCard cosmoCampaign
  , SomeAllyCard ladySifCaptive
  , SomeAllyCard fandral
  , SomeAllyCard hogun
  , SomeAllyCard volstagg
  , SomeAllyCard gamoraNebula
  , SomeAllyCard eros
  , SomeAllyCard wraithZakDel
  , SomeAllyCard venomFlashThompson
  , SomeAllyCard ironManWarMachine
  , SomeAllyCard blackPantherTChalla
  , SomeAllyCard captainMarvelWarMachine
  , SomeAllyCard annabelleRiggs
  , SomeAllyCard thorOdinson
  , SomeAllyCard throgPuddlegulp
  , SomeAllyCard vivian
  , SomeAllyCard jocasta
  , SomeAllyCard protectorAlexis
  , SomeAllyCard victorMancha
  , SomeAllyCard machineManAaronStack
  , SomeAllyCard silkCindyMoon
  , SomeAllyCard spiderManMilesMoralesSinisterMotives
  , SomeAllyCard spiderUKBillyBraddock
  , SomeAllyCard spiderManHobieBrown
  , SomeAllyCard monicaChang
  , SomeAllyCard spiderWomanJessicaDrewSinisterMotives
  , SomeAllyCard agent13SharonCarterSinisterMotives
  , SomeAllyCard dumDumDugan
  , SomeAllyCard ghostSpiderGwenStacy
  , SomeAllyCard spiderManPeterParkerSinisterMotives
  , SomeAllyCard venomEddieBrock
  , SomeAllyCard msMarvelKamalaKhanNova
  , SomeAllyCard theLocustFernandaRodriguez
  , SomeAllyCard moonGirlLunellaLafayette
  , SomeAllyCard brawnAmadeusChoIronheart
  , SomeAllyCard cloud9
  , SomeAllyCard falconJoaquinTorres
  , SomeAllyCard patriotRayshaunLucas
  , SomeAllyCard snowguardAmkaAliyak
  , SomeAllyCard vivianIronheart
  , SomeAllyCard bombshellLanaBaumgartner
  , SomeAllyCard waspNadiaVanDyneIronheart
  , SomeAllyCard pinpointQureshiGupta
  , SomeAllyCard captainAmericat
  , SomeAllyCard ladySpider
  , SomeAllyCard spiderManPavitrPrabhakar
  , SomeAllyCard scarletSpider
  , SomeAllyCard spDr
  , SomeAllyCard venmAddyBrock
  , SomeAllyCard daredevilSpdr
  , SomeAllyCard spiderManNoir
  , SomeAllyCard spiderHamPeterPorker
  , SomeAllyCard spiderManOttoOctavius
  , SomeAllyCard shadowcatKittyPryde
  , SomeAllyCard nightcrawler
  , SomeAllyCard polaris
  , SomeAllyCard professorX
  , SomeAllyCard lockheed
  , SomeAllyCard wolverine
  , SomeAllyCard magik
  , SomeAllyCard colossusPiotrRasputin
  , SomeAllyCard cyclopsScottSummers
  , SomeAllyCard banshee
  , SomeAllyCard marvelGirl
  , SomeAllyCard stormOroroMunroe
  , SomeAllyCard controlledMinion
  ]
