{-# OPTIONS_GHC -Wno-orphans #-}
module Marvel.Support where

import Marvel.FearNoEvil.Presses qualified as Presses

import Marvel.Support.Supports.JessicaLuke qualified as JL
import Marvel.Support.Supports.AgeOfApocalypseBasicCampaign

import Marvel.Support.Supports.NextEvolutionNextEvolutionCampaign

import Marvel.Support.Supports.MutantGenesisCampaign
import Marvel.Prelude

import Marvel.Card.Builder
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Id
import Marvel.Support.Supports
import Marvel.Support.Types

instance FromJSON Support where
  parseJSON = withObject "Support" $ \o -> do
    cardDef <- o .: "supportCardDef"
    withSupportCardCode (cdCardCode cardDef)
      $ \(_ :: SupportCard a) -> Support <$> parseJSON @a (Object o)

withSupportCardCode
  :: CardCode -> (forall a . IsSupport a => SupportCard a -> r) -> r
withSupportCardCode cCode f = case lookup cCode allSupports of
  Nothing -> error "invalid support"
  Just (SomeSupportCard a) -> f a

allSupports :: HashMap CardCode SomeSupportCard
allSupports = fromList $ [(toCardCode card,SomeSupportCard card) | card <- Presses.bugleStaff] <>  map
  (toFst someSupportCardCode)
  $ concatMap (\(SomeSupportCard a) -> SomeSupportCard <$> printings a)
  [ SomeSupportCard cw56003, SomeSupportCard cw56012, SomeSupportCard cw56031, SomeSupportCard cw56043, SomeSupportCard fne60013, SomeSupportCard fne60014, SomeSupportCard fne60015, SomeSupportCard fne60016, SomeSupportCard fne60029, SomeSupportCard fne60042
  , SomeSupportCard wm58007, SomeSupportCard wm58024, SomeSupportCard wm58031, SomeSupportCard wm58034, SomeSupportCard hc59012, SomeSupportCard hc59023
  , SomeSupportCard JL.jl61002, SomeSupportCard JL.jl61008, SomeSupportCard JL.jl62007, SomeSupportCard JL.jl62018, SomeSupportCard JL.jl62019
  , SomeSupportCard fw53006, SomeSupportCard fw53007, SomeSupportCard fw53020, SomeSupportCard fw53021, SomeSupportCard fw53037, SomeSupportCard fw54007
  , SomeSupportCard aosP50008, SomeSupportCard aosP50009, SomeSupportCard aosP50015, SomeSupportCard aosP50017, SomeSupportCard aosP50018, SomeSupportCard aosP50019, SomeSupportCard aosP50020, SomeSupportCard aosP50028, SomeSupportCard aosP50040, SomeSupportCard aosP50041, SomeSupportCard aosP50055, SomeSupportCard aosP50056
  , SomeSupportCard theElephantsTrunkShuri, SomeSupportCard queenRamondaShuri, SomeSupportCard theRaftShuri, SomeSupportCard doraMilajeShuri, SomeSupportCard targetSpotterShuri, SomeSupportCard albertMoonSilk, SomeSupportCard jJonahJamesonSilk, SomeSupportCard drSinclairSilk
  , SomeSupportCard nc48003
  , SomeSupportCard iceWall
  , SomeSupportCard asteroidM, SomeSupportCard wontStayDown, SomeSupportCard childrenOfTheAtom
  , SomeSupportCard missionTeamFront, SomeSupportCard missionTeamFinished
  , SomeSupportCard stepfordCuckoos
  , SomeSupportCard limbo
  , SomeSupportCard chimichangaTruck, SomeSupportCard deadpoolCorpsShip, SomeSupportCard plotConvenience
  , SomeSupportCard sisterhood
  , SomeSupportCard worthingtonIndustries, SomeSupportCard angelsAerie
  , SomeSupportCard trainingRegimen, SomeSupportCard ipac, SomeSupportCard xBunker
  , SomeSupportCard ne40197
  , SomeSupportCard thePaintedLady
  , SomeSupportCard pipthePug
  , SomeSupportCard cable40007, SomeSupportCard cable40008, SomeSupportCard cable40021, SomeSupportCard cable40022
  , SomeSupportCard moiraMacTaggert, SomeSupportCard medLab, SomeSupportCard theThievesGuild, SomeSupportCard warRoom
  , SomeSupportCard clearSkies
  , SomeSupportCard hurricane
  , SomeSupportCard thunderstorm
  , SomeSupportCard blizzard
  , SomeSupportCard ororosGarden
  , SomeSupportCard uncannyXMen
  , SomeSupportCard hangarBay
  , SomeSupportCard utopia, SomeSupportCard dangerRoom
  , SomeSupportCard logansCabin, SomeSupportCard weaponX, SomeSupportCard commandCenter
  , SomeSupportCard mgMetroPD
  , SomeSupportCard auntMay
  , SomeSupportCard alphaFlightStation
  , SomeSupportCard superhumanLawDivision
  , SomeSupportCard pepperPotts
  , SomeSupportCard starkTower
  , SomeSupportCard theGoldenCity
  , SomeSupportCard tacTeam
  , SomeSupportCard interrogationRoom
  , SomeSupportCard surveillanceTeam
  , SomeSupportCard theTriskellion
  , SomeSupportCard medTeam
  , SomeSupportCard avengersMansion
  , SomeSupportCard helicarrier
  , SomeSupportCard stevesApartment
  , SomeSupportCard quinjet
  , SomeSupportCard avengersTower
  , SomeSupportCard aamirKhan
  , SomeSupportCard brunoCarrelli
  , SomeSupportCard nakiaBahadir
  , SomeSupportCard safeHouse29
  , SomeSupportCard quincarrier
  , SomeSupportCard asgard
  , SomeSupportCard hallOfHeroes
  , SomeSupportCard sanctumSanctorum
  , SomeSupportCard theNightNurse
  , SomeSupportCard bannersLaboratory
  , SomeSupportCard beatCop
  -- The Green Goblin

  -- The Wrecking Crew

  -- The Rise of Red Skull
  , SomeSupportCard teamTraining
  , SomeSupportCard jessicaDrewsApartment

  -- The Once and Future Kang


  -- Ant-Man
  , SomeSupportCard armyOfAnts
  , SomeSupportCard teamBuildingExercise

  -- Wasp
  , SomeSupportCard bootCamp

  -- Quicksilver
  , SomeSupportCard servalIndustries

  -- Scarlet Witch
  , SomeSupportCard agathaHarkness

  -- Star-Lord
  , SomeSupportCard badBoy
  , SomeSupportCard targetPractice
  , SomeSupportCard citt
  , SomeSupportCard knowhere

  -- Gamora
  , SomeSupportCard conditioningRoom

  -- Groot
  , SomeSupportCard fertileGround

  -- The Galaxy's Most Wanted
  , SomeSupportCard milano
  -- Drax
  , SomeSupportCard regroup

  -- Venom
  , SomeSupportCard projectRebirth20
  , SomeSupportCard crewQuarters

  -- Spectrum
  , SomeSupportCard mightyAvengers

  -- Adam Warlock
  , SomeSupportCard soulWorld
  , SomeSupportCard nebulasShipSupport
  , SomeSupportCard guardiansOfTheGalaxy
  , SomeSupportCard defensiveTraining
  , SomeSupportCard munitionsBunker
  , SomeSupportCard commandTeam
  , SomeSupportCard vigilanteTraining
  , SomeSupportCard valhalla
  , SomeSupportCard theBifrost
  , SomeSupportCard leadershipTraining
  , SomeSupportCard hickoryBranchLane616
  , SomeSupportCard assaultTraining
  , SomeSupportCard georgeStacy
  , SomeSupportCard webOfLifeAndDestiny
  , SomeSupportCard gankeLee
  , SomeSupportCard jeffersonDavis
  , SomeSupportCard fieldAgent
  , SomeSupportCard governmentLiaison
  , SomeSupportCard skyDestroyer
  , SomeSupportCard jesseAlexander
  , SomeSupportCard championsMobileBunker
  , SomeSupportCard ronnieWilliams
  , SomeSupportCard tonyStarkAI
  , SomeSupportCard rAndDFacility
  , SomeSupportCard theDailyBeagle
  , SomeSupportCard spdrSuitInactive
  , SomeSupportCard auntMayAndUncleBen
  , SomeSupportCard ejectionProtocol
  , SomeSupportCard spdrCommand
  , SomeSupportCard piotrsStudio
  , SomeSupportCard theXjet
  , SomeSupportCard kittysRoom
  , SomeSupportCard xmansion
  , SomeSupportCard whiteHotRoom
  , SomeSupportCard cerebro
  ]

lookupSupport :: CardCode -> IdentityId -> SupportId -> Support
lookupSupport cardCode identityId supportId =
  case lookup cardCode allSupports of
    Just (SomeSupportCard a) ->
      Support $ buildCard a (identityId, supportId)
    Nothing -> error $ "Invalid card code for support " <> show cardCode
