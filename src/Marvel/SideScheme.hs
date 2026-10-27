{-# OPTIONS_GHC -Wno-orphans #-}
module Marvel.SideScheme where

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
  [ SomeSideSchemeCard breakinAndTakin
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
  ]

lookupSideScheme :: CardCode -> SideSchemeId -> SideScheme
lookupSideScheme cardCode = case lookup cardCode allSideSchemes of
  Just (SomeSideSchemeCard a) -> SideScheme <$> buildCard a
  Nothing -> error $ "Invalid card code for side scheme " <> show cardCode
