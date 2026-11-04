{-# OPTIONS_GHC -Wno-orphans #-}
module Marvel.Villain where

import Marvel.FearNoEvil.Kingpin qualified as Kingpin

import Marvel.FearNoEvil.Mary qualified as Mary

import Marvel.FearNoEvil.PurpleMan qualified as Purple

import Marvel.FearNoEvil.Hammerhead qualified as Hammerhead

import Marvel.FearNoEvil.Electro qualified as Electro

import Marvel.FearNoEvil.BullseyeVillain qualified as Bullseye

import Marvel.Shadowland.Villains qualified as Shadows

import Marvel.Villain.Villains.AgentsOfShield qualified as AosEncounter

import Marvel.Villain.Villains.TricksterTakeover qualified as TT

import Marvel.Villain.Villains.AgeOfApocalypseUnus

import Marvel.Villain.Villains.AgeOfApocalypseFourHorsemen

import Marvel.Villain.Villains.AgeOfApocalypseEnSabahNur

import Marvel.Villain.Villains.AgeOfApocalypseDarkBeast

import Marvel.Villain.Villains.AgeOfApocalypseApocalypse

import Marvel.Villain.Villains.NextEvolutionNeStryfe

import Marvel.Villain.Villains.NextEvolutionMisterSinister

import Marvel.Villain.Villains.NextEvolutionNeMarauders

import Marvel.Villain.Villains.NextEvolutionNeJuggernaut

import Marvel.Villain.Villains.Mojo
import Marvel.Villain.Villains.MojoOpening
import Marvel.Villain.Villains.MutantGenesis
import Marvel.Villain.Villains.SmVenomGoblin

import Marvel.Villain.Villains.SmSinisterSix

import Marvel.Villain.Villains.SmMysterio

import Marvel.Villain.Villains.SmVenom

import Marvel.Villain.Villains.SmSandman

import Marvel.Villain.Villains.TheHood qualified as Hood
import Marvel.Prelude

import Marvel.Card.Builder
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Villain.Types
import Marvel.CivilWar.Leaders qualified as CW
import Marvel.Villain.Villains.Klaw
import Marvel.Villain.Villains.Rhino
import Marvel.Villain.Villains.Ultron

-- The Green Goblin
import Marvel.Villain.Villains.GreenGoblin qualified as Mutagen
import Marvel.Villain.Villains.NormanOsborn

-- The Wrecking Crew
import Marvel.Villain.Villains.Bulldozer
import Marvel.Villain.Villains.Piledriver
import Marvel.Villain.Villains.Thunderball
import Marvel.Villain.Villains.Wrecker

-- The Rise of Red Skull
import Marvel.Villain.Villains.Crossbones
import Marvel.Villain.Villains.AbsorbingMan
-- Zola
import Marvel.Villain.Villains.Zola

-- Red Skull
import Marvel.Villain.Villains.RedSkull

-- Taskmaster
import Marvel.Villain.Villains.Taskmaster

-- The Once and Future Kang
import Marvel.Villain.Villains.Kang
-- Infiltrate the Museum
import Marvel.Villain.Villains.Collector

-- The Galaxy's Most Wanted: Brotherhood of Badoon
import Marvel.Villain.Villains.Drang

-- The Galaxy's Most Wanted: Escape the Museum
import Marvel.Villain.Villains.CollectorEscape

-- Ronan the Accuser
import Marvel.Villain.Villains.Ronan

-- The Galaxy's Most Wanted: Nebula
import Marvel.Villain.Villains.Nebula qualified as Nebula

-- The Mad Titan's Shadow: Ebony Maw
import Marvel.Villain.Villains.EbonyMaw qualified as EbonyMaw

-- The Mad Titan's Shadow: Tower Defense
import Marvel.Villain.Villains.TowerDefense qualified as TowerDefense

-- The Mad Titan's Shadow: Thanos
import Marvel.Villain.Villains.Thanos qualified as Thanos

-- The Mad Titan's Shadow: Hela
import Marvel.Villain.Villains.Hela qualified as Hela

-- The Mad Titan's Shadow: Loki
import Marvel.Villain.Villains.Loki qualified as Loki

instance FromJSON Villain where
  parseJSON = withObject "Villain" $ \o -> do
    cardDef <- o .: "villainCardDef"
    withVillainCardCode (cdCardCode cardDef)
      $ \(_ :: VillainCard a) -> Villain <$> parseJSON @a (Object o)

withVillainCardCode
  :: CardCode -> (forall a . IsVillain a => VillainCard a -> r) -> r
withVillainCardCode cCode f = case lookup cCode allVillains of
  Nothing -> error "invalid villain"
  Just (SomeVillainCard a) -> f a

lookupVillain :: CardCode -> VillainId -> Maybe Villain
lookupVillain cardCode villainId = case lookup cardCode allVillains of
  Just (SomeVillainCard a) -> Just . Villain $ buildCard a villainId
  Nothing -> Nothing

allVillains :: HashMap CardCode SomeVillainCard
allVillains = fromList $ [(toCardCode card,SomeVillainCard card) | card <- Kingpin.kingpinFaces] <>  [(toCardCode card,SomeVillainCard card) | card <- Mary.maryFaces] <>  [(toCardCode card,SomeVillainCard card) | card <- Purple.purpleManStages] <>  [(toCardCode card,SomeVillainCard card) | card <- Hammerhead.hammerheadStages] <>  [(toCardCode card,SomeVillainCard card) | card <- Electro.electroStages] <>  [(toCardCode card,SomeVillainCard card) | card <- Bullseye.bullseyeStages] <>  [(toCardCode card,SomeVillainCard card) | card <- Shadows.shadowVillains] <> [(toCardCode card,SomeVillainCard card) | card <- Shadows.krahllak] <> [(toCardCode Shadows.shiftingCorridors,SomeVillainCard Shadows.shiftingCorridors)] <>  [(toCardCode card,SomeVillainCard card) | card <- CW.leaderCards] <> map
  (toFst someVillainCardCode)
  [ SomeVillainCard TT.tt55001, SomeVillainCard TT.tt55002, SomeVillainCard TT.tt55003, SomeVillainCard TT.tt55027a, SomeVillainCard TT.tt55027b, SomeVillainCard TT.tt55029a, SomeVillainCard TT.tt55029b, SomeVillainCard TT.tt55030a, SomeVillainCard TT.tt55030b, SomeVillainCard TT.tt55031a, SomeVillainCard TT.tt55031b, SomeVillainCard TT.tt55032a, SomeVillainCard TT.tt55032b
  , SomeVillainCard AosEncounter.aos50064, SomeVillainCard AosEncounter.aos50065, SomeVillainCard AosEncounter.aos50066, SomeVillainCard AosEncounter.aos50086a, SomeVillainCard AosEncounter.aos50086b, SomeVillainCard AosEncounter.aos50103a, SomeVillainCard AosEncounter.aos50103b, SomeVillainCard AosEncounter.aos50129a, SomeVillainCard AosEncounter.aos50129b, SomeVillainCard AosEncounter.aos50165a, SomeVillainCard AosEncounter.aos50165b, SomeVillainCard AosEncounter.aos50166a, SomeVillainCard AosEncounter.aos50166b
  , SomeVillainCard unus1, SomeVillainCard unus2, SomeVillainCard unus3, SomeVillainCard warA, SomeVillainCard warB, SomeVillainCard famineA, SomeVillainCard famineB, SomeVillainCard pestilenceA, SomeVillainCard pestilenceB, SomeVillainCard deathA, SomeVillainCard deathB, SomeVillainCard apocalypse1, SomeVillainCard apocalypse2, SomeVillainCard apocalypse3, SomeVillainCard apocalypse4, SomeVillainCard darkBeast1, SomeVillainCard darkBeast2, SomeVillainCard darkBeast3, SomeVillainCard enSabahNurBiomorph1, SomeVillainCard enSabahNurCyberpath1, SomeVillainCard enSabahNurGiant1, SomeVillainCard enSabahNurBiomorph2, SomeVillainCard enSabahNurCyberpath2, SomeVillainCard enSabahNurGiant2, SomeVillainCard enSabahNurBiomorph3, SomeVillainCard enSabahNurCyberpath3, SomeVillainCard enSabahNurGiant3
  , SomeVillainCard ne40070a, SomeVillainCard ne40070b, SomeVillainCard ne40071a, SomeVillainCard ne40071b, SomeVillainCard ne40072a, SomeVillainCard ne40072b, SomeVillainCard ne40073a, SomeVillainCard ne40073b, SomeVillainCard ne40074a, SomeVillainCard ne40074b, SomeVillainCard ne40075a, SomeVillainCard ne40075b, SomeVillainCard ne40076a, SomeVillainCard ne40076b, SomeVillainCard ne40118, SomeVillainCard ne40119, SomeVillainCard ne40120, SomeVillainCard ne40136, SomeVillainCard ne40137, SomeVillainCard ne40138, SomeVillainCard ne40163, SomeVillainCard ne40164, SomeVillainCard ne40165
  , SomeVillainCard mojo39001a, SomeVillainCard mojo39001b, SomeVillainCard mojo39012a, SomeVillainCard mojo39012b, SomeVillainCard mojo39013a, SomeVillainCard mojo39013b, SomeVillainCard mojo39014a, SomeVillainCard mojo39014b, SomeVillainCard mojo39022, SomeVillainCard mojo39023, SomeVillainCard mojo39024
  , SomeVillainCard mgSabretooth60, SomeVillainCard mgSabretooth61, SomeVillainCard mgSabretooth62, SomeVillainCard mgSentinel84, SomeVillainCard mgSentinel85, SomeVillainCard mgSentinel86, SomeVillainCard mgMasterMold09, SomeVillainCard mgMasterMold10, SomeVillainCard mgMasterMold11, SomeVillainCard mgAvalancheA, SomeVillainCard mgAvalancheB, SomeVillainCard mgBlobA, SomeVillainCard mgBlobB, SomeVillainCard mgPyroA, SomeVillainCard mgPyroB, SomeVillainCard mgToadA, SomeVillainCard mgToadB, SomeVillainCard mgMagneto38, SomeVillainCard mgMagneto39, SomeVillainCard mgMagneto40, SomeVillainCard smSandman1, SomeVillainCard smSandman2, SomeVillainCard smSandman3, SomeVillainCard smVenom1, SomeVillainCard smVenom2, SomeVillainCard smVenom3, SomeVillainCard smMysterio1, SomeVillainCard smMysterio2, SomeVillainCard smMysterio3, SomeVillainCard smDoctorOctopus1, SomeVillainCard smElectro1, SomeVillainCard smHobgoblin1, SomeVillainCard smKravenTheHunter1, SomeVillainCard smScorpion1, SomeVillainCard smVulture1, SomeVillainCard smVenomGoblin1, SomeVillainCard smVenomGoblin2, SomeVillainCard smVenomGoblin3
  , SomeVillainCard Hood.theHood1
  , SomeVillainCard Hood.theHood2
  , SomeVillainCard Hood.theHood3
  , SomeVillainCard rhino
  , SomeVillainCard rhino1
  , SomeVillainCard rhino2
  , SomeVillainCard rhino3
  , SomeVillainCard klaw
  , SomeVillainCard klaw1
  , SomeVillainCard klaw2
  , SomeVillainCard klaw3
  , SomeVillainCard ultron
  , SomeVillainCard ultron1
  , SomeVillainCard ultron2
  , SomeVillainCard ultron3
  -- The Green Goblin
  , SomeVillainCard normanOsborn1
  , SomeVillainCard greenGoblin1
  , SomeVillainCard normanOsborn2
  , SomeVillainCard greenGoblin2
  , SomeVillainCard normanOsborn3
  , SomeVillainCard greenGoblin3
  , SomeVillainCard Mutagen.greenGoblin1
  , SomeVillainCard Mutagen.greenGoblin2
  , SomeVillainCard Mutagen.greenGoblin3

  -- The Wrecking Crew
  , SomeVillainCard wreckerA
  , SomeVillainCard wreckerB
  , SomeVillainCard thunderballA
  , SomeVillainCard thunderballB
  , SomeVillainCard piledriverA
  , SomeVillainCard piledriverB
  , SomeVillainCard bulldozerA
  , SomeVillainCard bulldozerB

  -- The Rise of Red Skull
  , SomeVillainCard crossbones
  , SomeVillainCard crossbones1
  , SomeVillainCard crossbones2
  , SomeVillainCard crossbones3
  , SomeVillainCard absorbingMan
  , SomeVillainCard absorbingMan1
  , SomeVillainCard absorbingMan2
  , SomeVillainCard absorbingMan3
  -- Zola
  , SomeVillainCard zola
  , SomeVillainCard zola1
  , SomeVillainCard zola2
  , SomeVillainCard zola3

  -- Red Skull
  , SomeVillainCard redSkull
  , SomeVillainCard redSkull1
  , SomeVillainCard redSkull2
  , SomeVillainCard redSkull3

  -- Taskmaster
  , SomeVillainCard taskmasterVillain
  , SomeVillainCard taskmasterVillain1
  , SomeVillainCard taskmasterVillain2
  , SomeVillainCard taskmasterVillain3

  -- The Once and Future Kang
  , SomeVillainCard kang1
  , SomeVillainCard kangImmortus
  , SomeVillainCard kangIronLad
  , SomeVillainCard kangRamaTut
  , SomeVillainCard kangScarletCenturion
  , SomeVillainCard kang3
  , SomeVillainCard kang1Expert
  , SomeVillainCard kangImmortusExpert
  , SomeVillainCard kangIronLadExpert
  , SomeVillainCard kangRamaTutExpert
  , SomeVillainCard kangScarletCenturionExpert
  , SomeVillainCard kang3Expert

  -- The Galaxy's Most Wanted: Brotherhood of Badoon
  , SomeVillainCard drang
  , SomeVillainCard drang1
  , SomeVillainCard drang2
  , SomeVillainCard drang3

  -- The Galaxy's Most Wanted: Escape the Museum
  , SomeVillainCard collectorA1
  , SomeVillainCard collectorA2
  , SomeVillainCard collectorB1
  , SomeVillainCard collectorB2

  -- Ronan the Accuser
  , SomeVillainCard ronan1
  , SomeVillainCard ronan2
  , SomeVillainCard ronan3
  -- The Galaxy's Most Wanted: Nebula
  , SomeVillainCard Nebula.nebula1
  , SomeVillainCard Nebula.nebula2
  , SomeVillainCard Nebula.nebula3

  -- The Galaxy's Most Wanted: Infiltrate the Museum
  , SomeVillainCard collector
  , SomeVillainCard collector1
  , SomeVillainCard collector2
  , SomeVillainCard collector3

  -- The Mad Titan's Shadow: Ebony Maw
  , SomeVillainCard EbonyMaw.ebonyMaw1
  , SomeVillainCard EbonyMaw.ebonyMaw2
  , SomeVillainCard EbonyMaw.ebonyMaw3

  -- The Mad Titan's Shadow: Tower Defense
  , SomeVillainCard TowerDefense.proximaMidnight1
  , SomeVillainCard TowerDefense.proximaMidnight2
  , SomeVillainCard TowerDefense.proximaMidnight3
  , SomeVillainCard TowerDefense.corvusGlaive1
  , SomeVillainCard TowerDefense.corvusGlaive2
  , SomeVillainCard TowerDefense.corvusGlaive3

  -- The Mad Titan's Shadow: Thanos
  , SomeVillainCard Thanos.thanos1
  , SomeVillainCard Thanos.thanos2
  , SomeVillainCard Thanos.thanos3
  -- The Mad Titan's Shadow: Hela
  , SomeVillainCard Hela.helaA1
  , SomeVillainCard Hela.helaA2
  , SomeVillainCard Hela.helaB1
  , SomeVillainCard Hela.helaB2
  -- The Mad Titan's Shadow: Loki
  , SomeVillainCard Loki.loki1
  , SomeVillainCard Loki.loki2
  , SomeVillainCard Loki.loki3
  , SomeVillainCard Loki.loki4
  , SomeVillainCard Loki.loki5
  ]
