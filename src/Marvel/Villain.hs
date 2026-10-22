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

-- The Wrecking Crew

-- The Rise of Red Skull
-- Red Skull
import Marvel.Villain.Villains.RedSkull

-- Taskmaster
import Marvel.Villain.Villains.Taskmaster

-- The Once and Future Kang

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

  -- The Wrecking Crew

  -- The Rise of Red Skull
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

  ]
