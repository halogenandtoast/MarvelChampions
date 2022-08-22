{-# OPTIONS_GHC -Wno-orphans #-}
module Marvel.AlterEgo where

import Marvel.Prelude

import Marvel.AlterEgo.AlterEgos
import Marvel.AlterEgo.Types
import Marvel.Card.Builder
import Marvel.Card.Code
import Marvel.Card.Def

instance FromJSON AlterEgo where
  parseJSON = withObject "AlterEgo" $ \o -> do
    cardDef <- o .: "alterEgoCardDef"
    withAlterEgoCardCode (cdCardCode cardDef)
      $ \(_ :: AlterEgoCard a) -> AlterEgo <$> parseJSON @a (Object o)

withAlterEgoCardCode
  :: CardCode -> (forall a . IsAlterEgo a => AlterEgoCard a -> r) -> r
withAlterEgoCardCode cCode f = case lookup cCode allAlterEgos of
  Nothing -> error "invalid alter ego"
  Just (SomeAlterEgoCard a) -> f a

allAlterEgos :: HashMap CardCode SomeAlterEgoCard
allAlterEgos = fromList $ map
  (toFst someAlterEgoCardCode)
  [ SomeAlterEgoCard peterParker
  , SomeAlterEgoCard carolDanvers
  , SomeAlterEgoCard jenniferWalters
  , SomeAlterEgoCard tonyStark
  , SomeAlterEgoCard tChalla
  ]

lookupAlterEgoByCardCode :: CardCode -> IdentityId -> AlterEgo
lookupAlterEgoByCardCode cardCode = case lookup cardCode allAlterEgos of
  Nothing -> error $ "Unknown alter ego: " <> show cardCode
  Just (SomeAlterEgoCard a) -> AlterEgo <$> cbCardBuilder a
