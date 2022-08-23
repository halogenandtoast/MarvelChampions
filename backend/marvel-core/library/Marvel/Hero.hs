{-# OPTIONS_GHC -Wno-orphans #-}
module Marvel.Hero where

import Marvel.Prelude

import Marvel.AlterEgo.Types
import Marvel.Card.Builder
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Hero.Heroes
import Marvel.Hero.Types

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
  [ SomeHeroCard spiderMan
  , SomeHeroCard captainMarvel
  , SomeHeroCard sheHulk
  , SomeHeroCard ironMan
  , SomeHeroCard blackPanther
  ]

lookupHeroByCardCode :: CardCode -> IdentityId -> Hero
lookupHeroByCardCode cardCode = case lookup cardCode allHeroes of
  Nothing -> error $ "Unknown hero: " <> show cardCode
  Just (SomeHeroCard a) -> Hero <$> cbCardBuilder a
