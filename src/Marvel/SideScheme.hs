{-# OPTIONS_GHC -Wno-orphans #-}
module Marvel.SideScheme where

import Marvel.SideScheme.SideSchemes.TheHood qualified as Hood
import Marvel.Prelude

import Marvel.Card
import Marvel.Id
import Marvel.SideScheme.SideSchemes
import Marvel.SideScheme.Types

instance FromJSON SideScheme where
  parseJSON = withObject "SideScheme" $ \o -> do
    cardDef <- o .: "sideSchemeCardDef"
    withSideSchemeCardCode (cdCardCode cardDef)
      $ \(_ :: SideSchemeCard a) -> SideScheme <$> parseJSON @a (Object o)

withSideSchemeCardCode
  :: CardCode -> (forall a . IsSideScheme a => SideSchemeCard a -> r) -> r
withSideSchemeCardCode cCode f = case lookup cCode allSideSchemes of
  Nothing -> error "invalid side scheme"
  Just (SomeSideSchemeCard a) -> f a

allSideSchemes :: HashMap CardCode SomeSideSchemeCard
allSideSchemes = fromList $ map
  (toFst someSideSchemeCardCode)
  [ SomeSideSchemeCard Hood.unbridledAmbition
  , SomeSideSchemeCard breakinAndTakin
  , SomeSideSchemeCard crowdControl
  , SomeSideSchemeCard bombScare
  , SomeSideSchemeCard defenseNetwork
  , SomeSideSchemeCard illegalArmsFactory
  , SomeSideSchemeCard theImmortalKlaw
  , SomeSideSchemeCard theMastersOfEvil
  , SomeSideSchemeCard usurpTheThrone
  , SomeSideSchemeCard personalChallenge
  , SomeSideSchemeCard highwayRobbery
  , SomeSideSchemeCard imminentOverload
  , SomeSideSchemeCard thePsycheMagnitron
  , SomeSideSchemeCard droneFactory
  , SomeSideSchemeCard invasiveAI
  , SomeSideSchemeCard ultronsImperative
  , SomeSideSchemeCard underAttack
  , SomeSideSchemeCard legionsOfHydra
  , SomeSideSchemeCard theDoomsdayChair
  , SomeSideSchemeCard hitSquad
  , SomeSideSchemeCard generationWhy
  , SomeSideSchemeCard killerForHire
  , SomeSideSchemeCard familyFeud
  , SomeSideSchemeCard openTheDarkDimension
  , SomeSideSchemeCard totalDestruction
  -- The Green Goblin
  , SomeSideSchemeCard collapsingBridge
  , SomeSideSchemeCard goblinNation
  , SomeSideSchemeCard goblinReinforcements
  , SomeSideSchemeCard oscorpManufacturing
  , SomeSideSchemeCard overrun
  , SomeSideSchemeCard payoff
  , SomeSideSchemeCard aMessOfThings
  , SomeSideSchemeCard powerDrain
  , SomeSideSchemeCard runningInterference

  -- The Wrecking Crew
  , SomeSideSchemeCard dayOfReckoning
  , SomeSideSchemeCard thunderstruck
  , SomeSideSchemeCard pileItOn
  , SomeSideSchemeCard clearTheRoad

  -- The Rise of Red Skull
  , SomeSideSchemeCard markedForDeath
  , SomeSideSchemeCard theVipersAmbition
  , SomeSideSchemeCard crossbonesAssault
  , SomeSideSchemeCard corneredStaff
  , SomeSideSchemeCard avalanche
  , SomeSideSchemeCard superAbsorbingPower
  , SomeSideSchemeCard hydraPatrol

  -- Zola
  , SomeSideSchemeCard hydraPrison
  , SomeSideSchemeCard testSubjects
  , SomeSideSchemeCard zolasExperiments

  -- Red Skull
  , SomeSideSchemeCard theRedHouse
  , SomeSideSchemeCard theSleeperAwakened
  , SomeSideSchemeCard prisonCamps
  , SomeSideSchemeCard censorThePast
  , SomeSideSchemeCard hydraReinforcements
  , SomeSideSchemeCard massChaos

  -- Taskmaster
  , SomeSideSchemeCard capturedByHydra
  , SomeSideSchemeCard taskmastersTrainingCamp

  -- The Once and Future Kang
  , SomeSideSchemeCard timePortal
  , SomeSideSchemeCard theAnachronauts
  , SomeSideSchemeCard lightOfCenturiesSphere
  , SomeSideSchemeCard corruptedTimestream
  , SomeSideSchemeCard kangsDominion
  , SomeSideSchemeCard pinnedDown
  , SomeSideSchemeCard rampage


  -- Ant-Man
  , SomeSideSchemeCard techTheft

  -- Wasp
  , SomeSideSchemeCard mothersOrders

  -- Quicksilver
  , SomeSideSchemeCard extortionOfSeismicProportion

  -- Scarlet Witch
  , SomeSideSchemeCard theNextEvolution

  -- Star-Lord
  , SomeSideSchemeCard buddingCrimeSyndicate

  -- Gamora
  , SomeSideSchemeCard siblingRivalry

  -- Groot
  , SomeSideSchemeCard blazingInferno

  -- Rocket Raccoon
  , SomeSideSchemeCard vendetta

  -- The Galaxy's Most Wanted
  , SomeSideSchemeCard cannonade
  , SomeSideSchemeCard fugitiveRecovery
  -- Brotherhood of Badoon
  , SomeSideSchemeCard blockade
  , SomeSideSchemeCard bombardment
  , SomeSideSchemeCard oppressiveArmada
  , SomeSideSchemeCard spatialPositioning
  -- Ronan the Accuser
  , SomeSideSchemeCard cutThePower
  , SomeSideSchemeCard pincerManeuver
  , SomeSideSchemeCard superiorTactics
  -- The Galaxy's Most Wanted: Nebula, Space Pirates
  , SomeSideSchemeCard lethalIntent
  , SomeSideSchemeCard soundTheAlarms
  -- The Mad Titan's Shadow: Ebony Maw, Black Order, Armies of Titan
  , SomeSideSchemeCard reactorOverload
  , SomeSideSchemeCard theBlackOrder
  , SomeSideSchemeCard landingCraft

  -- The Galaxy's Most Wanted: Challenge
  , SomeSideSchemeCard badoonBlitz
  , SomeSideSchemeCard badoonBlitzExpert
  , SomeSideSchemeCard galleryOfSplendor
  , SomeSideSchemeCard galleryOfSplendorExpert
  , SomeSideSchemeCard thereIsNoEscape
  , SomeSideSchemeCard thereIsNoEscapeExpert
  , SomeSideSchemeCard guerrillaTactics
  , SomeSideSchemeCard guerrillaTacticsExpert
  , SomeSideSchemeCard kreeSupremacy
  , SomeSideSchemeCard kreeSupremacyExpert

  -- The Galaxy's Most Wanted: Galactic Artifacts
  , SomeSideSchemeCard hujahdarianMonarchEgg
  , SomeSideSchemeCard magicalTeapot
  , SomeSideSchemeCard philosophersStone
  , SomeSideSchemeCard crystalBall
  -- Drax
  , SomeSideSchemeCard cullTheWeak

  -- Venom
  , SomeSideSchemeCard klyntarFrenzy

  -- Spectrum
  , SomeSideSchemeCard reactorMeltdown

  -- Adam Warlock
  , SomeSideSchemeCard universalChurchOfTruth

  -- The Mad Titan's Shadow: Tower Defense
  , SomeSideSchemeCard cityUnderAttack

  -- The Mad Titan's Shadow: Thanos, Children of Thanos
  , SomeSideSchemeCard sanctuary
  , SomeSideSchemeCard theTitansThrone
  , SomeSideSchemeCard tribute

  -- The Mad Titan's Shadow: Loki
  , SomeSideSchemeCard casketOfAncientWinters
  , SomeSideSchemeCard warInAsgard
  , SomeSideSchemeCard madnessOnMidgard
  , SomeSideSchemeCard openTheBifrost

  -- The Mad Titan's Shadow: Hela, Legions of Hel, Frost Giants
  , SomeSideSchemeCard gnipahellir
  , SomeSideSchemeCard hallOfNastrond
  , SomeSideSchemeCard gjallerbru
  , SomeSideSchemeCard legionsOfHelScheme
  , SomeSideSchemeCard unnaturalStorm

  -- The Mad Titan's Shadow: the campaign set
  , SomeSideSchemeCard secureTheLandingPad
  , SomeSideSchemeCard securityBreach
  , SomeSideSchemeCard saveTheShawarmaPlace
  , SomeSideSchemeCard hackSanctuarysComputer
  , SomeSideSchemeCard defensiveProtocols
  , SomeSideSchemeCard findTheNornStones
  , SomeSideSchemeCard retrieveOdinsArmor
  , SomeSideSchemeCard openTheDungeons
  , SomeSideSchemeCard selfPreservation
  , SomeSideSchemeCard deadlyLightShow
  , SomeSideSchemeCard powerfulEnchantments
  , SomeSideSchemeCard ultronUnleashed
  ]

lookupSideScheme :: CardCode -> SideSchemeId -> SideScheme
lookupSideScheme cardCode = case lookup cardCode allSideSchemes of
  Just (SomeSideSchemeCard a) -> SideScheme <$> buildCard a
  Nothing -> error $ "Invalid card code for side scheme " <> show cardCode
