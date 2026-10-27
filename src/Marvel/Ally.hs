{-# OPTIONS_GHC -Wno-orphans #-}
module Marvel.Ally
  ( module Marvel.Ally
  , module X
  ) where

import Marvel.Prelude

import Marvel.Ally.Allies
import Marvel.Ally.Types
import Marvel.Ally.Types as X (Ally)
import Marvel.Id

instance FromJSON Ally where
  parseJSON = withObject "Ally" $ \o -> do
    cardDef <- o .: "allyCardDef"
    withAllyCardCode (cdCardCode cardDef)
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
  [ SomeAllyCard blackCatFeliciaHardy
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

  -- Drax
  , SomeAllyCard mantis
  , SomeAllyCard martyrPhylaVell
  , SomeAllyCard moondragonHeatherDouglas
  , SomeAllyCard gamoraAlly
  ]
