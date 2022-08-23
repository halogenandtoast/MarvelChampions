{-# OPTIONS_GHC -Wno-orphans #-}
module Marvel.Villain where

import Marvel.Prelude

import Marvel.Card.Builder
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Villain.Types
import Marvel.Villain.Villains.Klaw
import Marvel.Villain.Villains.Rhino

instance FromJSON Villain where
  parseJSON v = flip (withObject "Villain") v $ \o -> do
    cardDef <- o .: "villainCardDef"
    withVillainCardCode (cdCardCode cardDef)
      $ \(_ :: VillainCard a) -> Villain <$> parseJSON @a v

withVillainCardCode
  :: CardCode -> (forall a . IsVillain a => VillainCard a -> r) -> r
withVillainCardCode cCode f = case lookup cCode allVillains of
  Nothing -> error "invalid villain"
  Just (SomeVillainCard a) -> f a

lookupVillain :: CardCode -> VillainId -> Maybe Villain
lookupVillain cardCode villainId = case lookup cardCode allVillains of
  Just (SomeVillainCard a) -> Just . Villain $ cbCardBuilder a villainId
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
  ]
