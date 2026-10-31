{-# OPTIONS_GHC -Wno-orphans #-}
module Marvel.Hero where

import Marvel.Prelude

import Marvel.AlterEgo.Types
import Marvel.Card.Builder
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Hero.Heroes
import Marvel.Hero.Types
import Marvel.Modifier
import Marvel.Trait

instance HasTraits Hero where
  getTraits = pure . cdTraits . getCardDef


instance HasModifiersFor Hero where
  getModifiersFor source target (Hero a) = getModifiersFor source target a

instance FromJSON Hero where
  parseJSON = withObject "Hero" $ \o -> do
    cardDef <- o .: "heroCardDef"
    withHeroCardCode (cdCardCode cardDef)
      $ \(_ :: HeroCard a) -> Hero <$> parseJSON @a (Object o)

withHeroCardCode :: CardCode -> (forall a . IsHero a => HeroCard a -> r) -> r
withHeroCardCode cCode f = case lookup cCode allHeroes of
  Nothing -> error "invalid hero"
  Just (SomeHeroCard a) -> f a

allHeroes :: HashMap CardCode SomeHeroCard
allHeroes = fromList $ map
  (toFst someHeroCardCode)
  [ SomeHeroCard stormHero
  , SomeHeroCard cyclopsHero
  , SomeHeroCard spiderMan
  , SomeHeroCard captainMarvel
  , SomeHeroCard sheHulk
  , SomeHeroCard ironMan
  , SomeHeroCard blackPanther
  , SomeHeroCard captainAmerica
  , SomeHeroCard msMarvel
  , SomeHeroCard blackWidow
  , SomeHeroCard thor
  , SomeHeroCard doctorStrange
  , SomeHeroCard hulk
  -- The Green Goblin

  -- The Wrecking Crew

  -- The Rise of Red Skull
  , SomeHeroCard hawkeye
  , SomeHeroCard spiderWoman

  -- The Once and Future Kang

  -- Ant-Man
  , SomeHeroCard antManTiny
  , SomeHeroCard antManGiant

  -- Wasp
  , SomeHeroCard waspTiny
  , SomeHeroCard waspGiant

  -- Quicksilver
  , SomeHeroCard quicksilver

  -- Scarlet Witch
  , SomeHeroCard scarletWitch

  -- Star-Lord
  , SomeHeroCard starLord

  -- Gamora
  , SomeHeroCard gamora

  -- Groot
  , SomeHeroCard groot

  -- Rocket Raccoon
  , SomeHeroCard rocketRaccoon
  -- Drax
  , SomeHeroCard drax
  -- Venom
  , SomeHeroCard venom
  -- Spectrum
  , SomeHeroCard spectrum
  -- Adam Warlock
  , SomeHeroCard adamWarlockHero
  , SomeHeroCard nebulaHero
  , SomeHeroCard warMachineHero
  , SomeHeroCard valkyrieHero
  , SomeHeroCard visionHero
  , SomeHeroCard ghostSpider
  , SomeHeroCard spiderManMilesMoralesHero
  , SomeHeroCard novaHero
  , SomeHeroCard ironheartVersion1
  , SomeHeroCard ironheartVersion2
  , SomeHeroCard ironheartVersion3
  , SomeHeroCard spiderHam
  , SomeHeroCard spdrSuitHero
  , SomeHeroCard colossusHero
  , SomeHeroCard shadowcatHero
  , SomeHeroCard phoenixHero
  ]

lookupHeroByCardCode :: CardCode -> IdentityId -> Hero
lookupHeroByCardCode cardCode = case lookup cardCode allHeroes of
  Nothing -> error $ "Unknown hero: " <> show cardCode
  Just (SomeHeroCard a) -> Hero <$> buildCard a
