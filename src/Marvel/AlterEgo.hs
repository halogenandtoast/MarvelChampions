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
  [ SomeAlterEgoCard warrenWorthingtonIII
  , SomeAlterEgoCard betsyBraddock
  , SomeAlterEgoCard cable40001b
  , SomeAlterEgoCard neenaThurman
  , SomeAlterEgoCard annaMarie, SomeAlterEgoCard remyLeBeau
  , SomeAlterEgoCard ororoMunroe
  , SomeAlterEgoCard scottSummers
  , SomeAlterEgoCard logan
  , SomeAlterEgoCard peterParker
  , SomeAlterEgoCard carolDanvers
  , SomeAlterEgoCard jenniferWalters
  , SomeAlterEgoCard tonyStark
  , SomeAlterEgoCard tChalla
  , SomeAlterEgoCard steveRogers
  , SomeAlterEgoCard kamalaKhan
  , SomeAlterEgoCard natashaRomanoff
  , SomeAlterEgoCard odinson
  , SomeAlterEgoCard stephenStrange
  , SomeAlterEgoCard bruceBanner
  -- The Green Goblin

  -- The Wrecking Crew

  -- The Rise of Red Skull
  , SomeAlterEgoCard clintBarton
  , SomeAlterEgoCard jessicaDrew

  -- The Once and Future Kang

  -- Ant-Man
  , SomeAlterEgoCard scottLang

  -- Wasp
  , SomeAlterEgoCard nadiaVanDyne

  -- Quicksilver
  , SomeAlterEgoCard pietroMaximoff

  -- Scarlet Witch
  , SomeAlterEgoCard wandaMaximoff

  -- Star-Lord
  , SomeAlterEgoCard peterQuill

  -- Gamora
  , SomeAlterEgoCard gamoraAlterEgo

  -- Groot
  , SomeAlterEgoCard grootAlterEgo

  -- Rocket Raccoon
  , SomeAlterEgoCard rocketRaccoonAlterEgo
  -- Drax
  , SomeAlterEgoCard draxAlterEgo
  -- Venom
  , SomeAlterEgoCard flashThompson
  -- Spectrum
  , SomeAlterEgoCard monicaRambeau
  -- Adam Warlock
  , SomeAlterEgoCard adamWarlockAlterEgo
  , SomeAlterEgoCard nebulaAlterEgo
  , SomeAlterEgoCard jamesRhodes
  , SomeAlterEgoCard brunnhilde
  , SomeAlterEgoCard visionAlterEgo
  , SomeAlterEgoCard gwenStacy
  , SomeAlterEgoCard milesMorales
  , SomeAlterEgoCard samAlexander
  , SomeAlterEgoCard ririWilliamsVersion1
  , SomeAlterEgoCard ririWilliamsVersion2
  , SomeAlterEgoCard ririWilliamsVersion3
  , SomeAlterEgoCard peterPorker
  , SomeAlterEgoCard peniParker
  , SomeAlterEgoCard piotrRasputin
  , SomeAlterEgoCard kittyPryde
  , SomeAlterEgoCard jeanGrey
  ]

lookupAlterEgoByCardCode :: CardCode -> IdentityId -> AlterEgo
lookupAlterEgoByCardCode cardCode = case lookup cardCode allAlterEgos of
  Nothing -> error $ "Unknown alter ego: " <> show cardCode
  Just (SomeAlterEgoCard a) -> AlterEgo <$> buildCard a
