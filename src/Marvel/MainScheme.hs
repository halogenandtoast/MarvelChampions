{-# OPTIONS_GHC -Wno-orphans #-}
module Marvel.MainScheme where

import Marvel.FearNoEvil.Raft qualified as Raft

import Marvel.FearNoEvil.Getaway qualified as Getaway

import Marvel.FearNoEvil.ArtMuseum qualified as Art

import Marvel.Shadowland.MainSchemes qualified as ShadowSchemes

import Marvel.CivilWar.MainSchemes qualified as CW
import Marvel.MainScheme.MainSchemes.AgentsOfShield qualified as AosEncounter

import Marvel.MainScheme.MainSchemes.TricksterTakeover qualified as TT

import Marvel.MainScheme.MainSchemes.AgeOfApocalypseUnus

import Marvel.MainScheme.MainSchemes.AgeOfApocalypseFourHorsemen

import Marvel.MainScheme.MainSchemes.AgeOfApocalypseEnSabahNur

import Marvel.MainScheme.MainSchemes.AgeOfApocalypseDarkBeast

import Marvel.MainScheme.MainSchemes.AgeOfApocalypseApocalypse

import Marvel.MainScheme.MainSchemes.NextEvolutionNeStryfe

import Marvel.MainScheme.MainSchemes.NextEvolutionOnTheRun

import Marvel.MainScheme.MainSchemes.NextEvolutionMorlockSiege

import Marvel.MainScheme.MainSchemes.NextEvolutionMisterSinister

import Marvel.MainScheme.MainSchemes.NextEvolutionNeJuggernaut

import Marvel.MainScheme.MainSchemes.Mojo
import Marvel.MainScheme.MainSchemes.MojoOpening
import Marvel.MainScheme.MainSchemes.TheHood qualified as Hood
import Marvel.Prelude

import Marvel.Card
import Marvel.Id
import Marvel.MainScheme.MainSchemes
import Marvel.MainScheme.Types

instance FromJSON MainScheme where
  parseJSON = withObject "MainScheme" $ \o -> do
    cardDef <- o .: "mainSchemeCardDef"
    withMainSchemeCardCode (cdCardCode cardDef)
      $ \(_ :: MainSchemeCard a) -> MainScheme <$> parseJSON @a (Object o)

withMainSchemeCardCode
  :: CardCode -> (forall a . IsMainScheme a => MainSchemeCard a -> r) -> r
withMainSchemeCardCode cCode f = case lookup cCode allMainSchemes of
  Nothing -> error "invalid minion"
  Just (SomeMainSchemeCard a) -> f a

allMainSchemes :: HashMap CardCode SomeMainSchemeCard
allMainSchemes = fromList $ [(toCardCode Raft.raftBreakout,SomeMainSchemeCard Raft.raftBreakout)] <>  [(toCardCode Getaway.getaway,SomeMainSchemeCard Getaway.getaway)] <>  [(toCardCode Art.museumHeist,SomeMainSchemeCard Art.museumHeist)] <>  [(toCardCode card,SomeMainSchemeCard card) | card <- ShadowSchemes.shadowMainSchemes] <>  map (toFst someMainSchemeCardCode) (map SomeMainSchemeCard CW.civilWarMainSchemes) <> map (toFst someMainSchemeCardCode) [ SomeMainSchemeCard AosEncounter.aos50067b, SomeMainSchemeCard AosEncounter.aos50087b, SomeMainSchemeCard AosEncounter.aos50088b, SomeMainSchemeCard AosEncounter.aos50089b, SomeMainSchemeCard AosEncounter.aos50104b, SomeMainSchemeCard AosEncounter.aos50130b, SomeMainSchemeCard AosEncounter.aos50167b, SomeMainSchemeCard AosEncounter.aos50168b, SomeMainSchemeCard AosEncounter.aos50169b] <> [
  ("55004", SomeMainSchemeCard TT.tt55004b), ("55005", SomeMainSchemeCard TT.tt55005b), ("55028", SomeMainSchemeCard TT.tt55028b), ("55033", SomeMainSchemeCard TT.tt55033b),   ("39002", SomeMainSchemeCard mojo39002b), ("39015", SomeMainSchemeCard mojo39015b), ("39025", SomeMainSchemeCard mojo39025b)] <> map
  (toFst someMainSchemeCardCode)
  [ SomeMainSchemeCard TT.tt55004a, SomeMainSchemeCard TT.tt55004b, SomeMainSchemeCard TT.tt55005a, SomeMainSchemeCard TT.tt55005b, SomeMainSchemeCard TT.tt55028a, SomeMainSchemeCard TT.tt55028b, SomeMainSchemeCard TT.tt55033a, SomeMainSchemeCard TT.tt55033b
  , SomeMainSchemeCard huntingGeneTraitors, SomeMainSchemeCard theHorsemenOfApocalypse, SomeMainSchemeCard theAgeOfApocalypse, SomeMainSchemeCard darkBeastsBogusJourney, SomeMainSchemeCard enSabahNursPyramid, SomeMainSchemeCard theRiseOfApocalypse
  , SomeMainSchemeCard ne40077, SomeMainSchemeCard ne40078, SomeMainSchemeCard ne40103, SomeMainSchemeCard ne40104, SomeMainSchemeCard ne40121, SomeMainSchemeCard ne40139, SomeMainSchemeCard ne40140, SomeMainSchemeCard ne40141, SomeMainSchemeCard ne40142, SomeMainSchemeCard ne40143, SomeMainSchemeCard ne40166, SomeMainSchemeCard ne40167
  , SomeMainSchemeCard mojo39002b, SomeMainSchemeCard mojo39015b, SomeMainSchemeCard mojo39025b
  , SomeMainSchemeCard mgStalkedBySabretooth, SomeMainSchemeCard mgTheInjuredSenator, SomeMainSchemeCard mgNightOfTheSentinels, SomeMainSchemeCard mgTheSentinelFactory, SomeMainSchemeCard mgMasterMoldsAgenda, SomeMainSchemeCard mgTheBrotherhoodStrikes, SomeMainSchemeCard mgTheAtrium, SomeMainSchemeCard mgTheCafeteria, SomeMainSchemeCard mgTheBasketballCourt, SomeMainSchemeCard mgTheCourtyard, SomeMainSchemeCard mgAsteroidM, SomeMainSchemeCard mgFactoryOnline, SomeMainSchemeCard mgTheRuleOfMagnus, SomeMainSchemeCard smHaplessPedestrians, SomeMainSchemeCard smLeaveUsAlone, SomeMainSchemeCard smMazeOfMirrors, SomeMainSchemeCard smEdgeOfReality, SomeMainSchemeCard smSinisterSynchronization, SomeMainSchemeCard smSinisterBeatdown, SomeMainSchemeCard smSkiesOverNewYork, SomeMainSchemeCard smLowerManhattan, SomeMainSchemeCard smMidtownManhattan, SomeMainSchemeCard smUpperManhattan
  , SomeMainSchemeCard Hood.makingConnections
  , SomeMainSchemeCard Hood.promisedProsperity
  , SomeMainSchemeCard Hood.crimeState
  , SomeMainSchemeCard theBreakIn
  , SomeMainSchemeCard undergroundDistribution
  , SomeMainSchemeCard secretRendezvous
  , SomeMainSchemeCard theCrimsonCowl
  , SomeMainSchemeCard assaultOnNorad
  , SomeMainSchemeCard countdownToOblivion
  -- The Green Goblin
  , SomeMainSchemeCard hostileTakeover
  , SomeMainSchemeCard corporateAcquisition
  , SomeMainSchemeCard unleashingTheMutagen
  , SomeMainSchemeCard mutagenCloud

  -- The Wrecking Crew
  , SomeMainSchemeCard breakout

  -- The Rise of Red Skull
  , SomeMainSchemeCard attackOnMountAthena
  , SomeMainSchemeCard theInfinityStone
  , SomeMainSchemeCard theGetaway
  , SomeMainSchemeCard noneShallPass
  -- Zola
  , SomeMainSchemeCard theIslandOfDrZola
  , SomeMainSchemeCard theMadDoctor

  -- Red Skull
  , SomeMainSchemeCard theRiseOfRedSkull
  , SomeMainSchemeCard newWorldHydra

  -- Taskmaster
  , SomeMainSchemeCard huntingDownHeroes

  -- The Once and Future Kang
  , SomeMainSchemeCard kangsArrival
  , SomeMainSchemeCard theMasterOfTime
  , SomeMainSchemeCard theChronopolis
  , SomeMainSchemeCard inexorableFate
  , SomeMainSchemeCard theRealmOfRamaTut
  , SomeMainSchemeCard thePresentFutureWar
  , SomeMainSchemeCard kangsWrath

  -- The Galaxy's Most Wanted: Brotherhood of Badoon
  , SomeMainSchemeCard terrestrialInvasion
  , SomeMainSchemeCard protectThePlanet

  -- The Galaxy's Most Wanted: Escape the Museum
  , SomeMainSchemeCard theMissingMilano
  , SomeMainSchemeCard lostInTheMuseum
  , SomeMainSchemeCard theGreatEscape

  -- Ronan the Accuser
  , SomeMainSchemeCard interceptionImminent
  , SomeMainSchemeCard takeWhatIsMine
  -- The Galaxy's Most Wanted: Nebula
  , SomeMainSchemeCard theArtOfEvasion
  , SomeMainSchemeCard warpDriveInitiated

  -- The Galaxy's Most Wanted: Infiltrate the Museum
  , SomeMainSchemeCard theGrandCollection
  -- The Mad Titan's Shadow: Ebony Maw
  , SomeMainSchemeCard attackOnKnowhere
  , SomeMainSchemeCard thePowerStoneScheme
  -- The Mad Titan's Shadow: Tower Defense
  , SomeMainSchemeCard underSiege
  , SomeMainSchemeCard theArmiesOfThanos
  -- The Mad Titan's Shadow: Thanos
  , SomeMainSchemeCard theInfinityStones
  , SomeMainSchemeCard balanceTheScales
  -- The Mad Titan's Shadow: Loki
  , SomeMainSchemeCard allHailKingLoki
  -- The Mad Titan's Shadow: Hela
  , SomeMainSchemeCard odinsTorment
  ]

lookupMainScheme :: CardCode -> MainSchemeId -> Maybe MainScheme
lookupMainScheme cardCode mainSchemeId = case lookup cardCode allMainSchemes of
  Just (SomeMainSchemeCard a) ->
    Just . MainScheme $ buildCard a mainSchemeId
  Nothing -> Nothing
