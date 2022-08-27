module Marvel.Hero.Types
  ( module Marvel.Hero.Types
  , module X
  ) where

import Marvel.Prelude

import Data.Typeable
import Marvel.Ability
import Marvel.Card.Builder
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Card.Side
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hand
import Marvel.Hp as X
import Marvel.Id as X
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Source
import Marvel.Stats
import Marvel.Target
import Marvel.Trait
import Marvel.Window qualified as W
import Text.Show qualified

data Hero = forall a . IsHero a => Hero a

instance Show Hero where
  show (Hero a) = show a

instance ToJSON Hero where
  toJSON (Hero a) = toJSON a

instance Eq Hero where
  Hero (a :: a) == Hero (b :: b) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

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
  type Id Hero = IdentityId
  data Attrs Hero = HeroAttrs
    { heroIdentityId :: IdentityId
    , heroBaseHandSize :: HandSize
    , heroBaseThwart :: Thw
    , heroBaseAttack :: Atk
    , heroBaseDefense :: Def
    , heroAlterEgoForms :: [Side]
    , heroStartingHP :: HP GameValue
    , heroCardDef :: CardDef
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)
  data Field Hero :: Type -> Type where
    HeroIdentityId :: Field Hero IdentityId
    HeroBaseHandSize :: Field Hero HandSize
    HeroBaseThwart :: Field Hero Thw
    HeroBaseAttack :: Field Hero Atk
    HeroBaseDefense :: Field Hero Def
    HeroAlterEgoForms :: Field Hero [Side]
    HeroStartingHP :: Field Hero (HP GameValue)
    HeroCardDef :: Field Hero CardDef
  field fld h = let HeroAttrs {..} = toAttrs h in case fld of
    HeroIdentityId -> heroIdentityId
    HeroBaseHandSize -> heroBaseHandSize
    HeroBaseThwart -> heroBaseThwart
    HeroBaseAttack -> heroBaseAttack
    HeroBaseDefense -> heroBaseDefense
    HeroAlterEgoForms -> heroAlterEgoForms
    HeroStartingHP -> heroStartingHP
    HeroCardDef -> heroCardDef
  toId = heroIdentityId . toAttrs
  toAttrs (Hero a) = toHeroAttrs a

instance HasModifiersFor Hero where
  getModifiersFor source target (Hero a) = getModifiersFor source target a

instance RunMessage Hero where
  runMessage msg (Hero a) = Hero <$> runMessage msg a

instance HasTraits Hero where
  getTraits = pure . cdTraits . getCardDef

data SomeHeroCard = forall a . IsHero a => SomeHeroCard (HeroCard a)

liftHeroCard :: (forall a . HeroCard a -> b) -> SomeHeroCard -> b
liftHeroCard f (SomeHeroCard a) = f a

someHeroCardCode :: SomeHeroCard -> CardCode
someHeroCardCode = liftHeroCard cbCardCode


hero
  :: (Attrs Hero -> a)
  -> CardDef
  -> HP GameValue
  -> HandSize
  -> Thw
  -> Atk
  -> Def
  -> CardBuilder IdentityId a
hero f cardDef hp hSize thw atk def = CardBuilder
  { cbCardCode = cdCardCode cardDef
  , cbCardBuilder = \ident -> f $ HeroAttrs
    { heroIdentityId = ident
    , heroBaseHandSize = hSize
    , heroBaseThwart = thw
    , heroBaseAttack = atk
    , heroBaseDefense = def
    , heroAlterEgoForms = [A]
    , heroStartingHP = hp
    , heroCardDef = cardDef
    }
  }

class (ToJSON a, FromJSON a, Show a, Eq a, Typeable a, HasAbilities a, RunMessage a, IsSource a, HasModifiersFor a) => IsHero a where
  toHeroAttrs :: a -> Attrs Hero
  default toHeroAttrs :: Coercible a (Attrs Hero) => a -> Attrs Hero
  toHeroAttrs = coerce

type HeroCard a = CardBuilder IdentityId a

instance HasStartingHP (Attrs Hero) where
  startingHP = heroStartingHP

instance HasHandSize (Attrs Hero) where
  handSize = heroBaseHandSize

instance HasCardCode (Attrs Hero) where
  toCardCode = toCardCode . heroCardDef

instance HasCardDef (Attrs Hero) where
  getCardDef = heroCardDef

instance IsSource (Attrs Hero) where
  toSource = IdentitySource . heroIdentityId

instance IsTarget (Attrs Hero) where
  toTarget = IdentityTarget . heroIdentityId

getModifiedAttack :: MonadGame env m => Attrs Hero -> m Natural
getModifiedAttack attrs = do
  modifiers <- getModifiers attrs
  pure $ foldr applyModifier (unAtk $ heroBaseAttack attrs) modifiers
 where
  applyModifier (AttackModifier n) = max 0 . (+ fromIntegral n)
  applyModifier _ = id

getModifiedThwart :: MonadGame env m => Attrs Hero -> m Natural
getModifiedThwart attrs = do
  modifiers <- getModifiers attrs
  pure $ foldr applyModifier (unThw $ heroBaseThwart attrs) modifiers
 where
  applyModifier (ThwartModifier n) = max 0 . (+ fromIntegral n)
  applyModifier _ = id

getModifiedDefense :: MonadGame env m => Attrs Hero -> m Natural
getModifiedDefense attrs = do
  modifiers <- getModifiers attrs
  pure $ foldr applyModifier (unDef $ heroBaseDefense attrs) modifiers
 where
  applyModifier (DefenseModifier n) = max 0 . (+ fromIntegral n)
  applyModifier _ = id

damageChoice :: Attrs Hero -> Damage -> EnemyId -> Choice
damageChoice attrs dmg = \case
  EnemyVillainId vid -> TargetLabel
    (VillainTarget vid)
    [ DamageEnemy (VillainTarget vid) (toSource attrs) dmg
    , Run
      [ CheckWindows
          [ W.Window W.After
              $ W.IdentityAttack (heroIdentityId attrs) (EnemyVillainId vid)
          ]
      ]
    ]
  EnemyMinionId mid -> TargetLabel
    (MinionTarget mid)
    [ DamageEnemy (MinionTarget mid) (toSource attrs) dmg
    , Run
      [ CheckWindows
          [W.Window W.After $ W.IdentityAttack (heroIdentityId attrs) (EnemyMinionId mid)]
      ]
    ]

thwartChoice :: Attrs Hero -> Natural -> SchemeId -> Choice
thwartChoice attrs thw = \case
  SchemeMainSchemeId vid -> TargetLabel
    (MainSchemeTarget vid)
    [ThwartScheme (MainSchemeTarget vid) (toSource attrs) thw]
  SchemeSideSchemeId sid -> TargetLabel
    (SideSchemeTarget sid)
    [ThwartScheme (SideSchemeTarget sid) (toSource attrs) thw]
