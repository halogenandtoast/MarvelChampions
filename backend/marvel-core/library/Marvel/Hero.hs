module Marvel.Hero where

import Marvel.Prelude

import Data.Typeable
import Marvel.Ability hiding (Attack, Thwart)
import Marvel.AlterEgo.Types
import Marvel.Card.Builder
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Hand
import Marvel.Hero.Attrs
import Marvel.Hero.Heroes
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Source
import Marvel.Trait (HasTraits(..))
import Text.Show qualified

instance FromJSON Hero where
  parseJSON = withObject "Hero" $ \o -> do
    cardDef <- o .: "heroCardDef"
    withHeroCardCode (cdCardCode cardDef) $ \(_ :: HeroCard a) -> Hero <$> parseJSON @a (Object o)

withHeroCardCode
  :: CardCode
  -> (forall a. IsHero a => HeroCard a -> r)
  -> r
withHeroCardCode cCode f =
  case lookup cCode allHeroes of
    Nothing -> error "invalid hero"
    Just (SomeHeroCard a) -> f a

allHeroes :: HashMap CardCode SomeHeroCard
allHeroes =
  fromList $ map (toFst someHeroCardCode)
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
