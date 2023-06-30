{-# LANGUAGE QuantifiedConstraints #-}

module Marvel.Hero.Types (
  module Marvel.Hero.Types,
  module X,
) where

import Marvel.Prelude

import Data.Typeable
import Marvel.Ability.Types
import Marvel.Card.Builder
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Card.Side
import Marvel.Entity
import Marvel.GameValue.Types
import Marvel.Hand
import Marvel.Hp as X
import Marvel.Id as X
import {-# SOURCE #-} Marvel.Message
import {-# SOURCE #-} Marvel.Modifier
import Marvel.Ref
import Marvel.Stats

data Hero = forall a. (IsHero a) => Hero a

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

instance IsRef Hero where
  toRef = IdentityRef . toId

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
  field fld h =
    let HeroAttrs {..} = toAttrs h
     in case fld of
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

data SomeHeroCard = forall a. (IsHero a) => SomeHeroCard (HeroCard a)

liftHeroCard :: (forall a. HeroCard a -> b) -> SomeHeroCard -> b
liftHeroCard f (SomeHeroCard a) = f a

someHeroCardCode :: SomeHeroCard -> CardCode
someHeroCardCode = liftHeroCard cbCardCode

hero ::
  (Attrs Hero -> a) ->
  CardDef ->
  HP GameValue ->
  HandSize ->
  Thw ->
  Atk ->
  Def ->
  CardBuilder IdentityId a
hero f cardDef hp hSize thw atk def =
  CardBuilder
    { cbCardCode = cdCardCode cardDef
    , cbCardBuilder = \ident ->
        f $
          HeroAttrs
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

class (ToJSON a, FromJSON a, Show a, Eq a, Typeable a, HasAbilities a, RunMessage a, HasModifiersFor a, IsRef a) => IsHero a where
  toHeroAttrs :: a -> Attrs Hero
  default toHeroAttrs :: (Coercible a (Attrs Hero)) => a -> Attrs Hero
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

instance IsRef (Attrs Hero) where
  toRef = IdentityRef . heroIdentityId
