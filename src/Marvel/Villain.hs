{-# OPTIONS_GHC -Wno-orphans #-}
module Marvel.Villain where

import Marvel.Prelude

import Marvel.Card.Builder
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Villain.Types
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
allVillains = fromList $ map
  (toFst someVillainCardCode)
  [ SomeVillainCard rhino
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

  -- The Galaxy's Most Wanted: Infiltrate the Museum
  , SomeVillainCard collector
  , SomeVillainCard collector1
  , SomeVillainCard collector2
  , SomeVillainCard collector3
  ]
