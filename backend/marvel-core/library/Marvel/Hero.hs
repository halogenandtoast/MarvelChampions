module Marvel.Hero where

import Marvel.Prelude

import Data.Typeable
import Marvel.Ability hiding (Attack, Thwart)
import Marvel.AlterEgo.Attrs
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

data Hero = forall a. IsHero a => Hero a

instance Show Hero where
  show (Hero a) = show a

instance ToJSON Hero where
  toJSON (Hero a) = toJSON a

instance FromJSON Hero where
  parseJSON v = flip (withObject "Hero") v $ \o -> do
    cCode :: CardCode <- o .: "cardCode"
    withHeroCardCode cCode $ \(_ :: HeroCard a) -> Hero <$> parseJSON @a v

withHeroCardCode
  :: CardCode
  -> (forall a. IsHero a => HeroCard a -> r)
  -> r
withHeroCardCode cCode f =
  case lookup cCode allHeroes of
    Nothing -> error "invalid hero"
    Just (SomeHeroCard a) -> f a

instance Eq Hero where
  (Hero (a :: a)) == (Hero (b :: b)) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

instance RunMessage Hero where
  runMessage msg (Hero a) = Hero <$> runMessage msg a

instance HasTraits Hero where
  getTraits = pure . cdTraits . getCardDef

instance HasAbilities Hero where
  getAbilities (Hero a) = getAbilities a <> basicAbilities
   where
    basicAbilities =
      [ ability a 300 Basic (SchemeExists ThwartableScheme) ExhaustCost Thwart
      , ability a 301 Basic (EnemyExists AttackableEnemy) ExhaustCost Attack
      ]

data SomeHeroCard = forall a. IsHero a => SomeHeroCard (HeroCard a)

liftHeroCard :: (forall a . HeroCard a -> b) -> SomeHeroCard -> b
liftHeroCard f (SomeHeroCard a) = f a

someHeroCardCode :: SomeHeroCard -> CardCode
someHeroCardCode = liftHeroCard cbCardCode

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

instance HasStartingHP Hero where
  startingHP = startingHP . toAttrs

instance HasHandSize Hero where
  handSize = handSize . toAttrs

instance HasCardCode Hero where
  toCardCode = toCardCode . toAttrs

instance HasCardDef Hero where
  getCardDef = getCardDef . toAttrs

instance IsSource Hero where
  toSource = toSource . toAttrs

instance Entity Hero where
  type EntityId Hero = IdentityId
  type EntityAttrs Hero = HeroAttrs
  toId = toId . toAttrs
  toAttrs (Hero a) = toAttrs a

instance HasModifiersFor Hero where
  getModifiersFor source target (Hero a) = getModifiersFor source target a
