module Marvel.Hero.Attrs
  ( module Marvel.Hero.Attrs
  , module X
  ) where

import Marvel.Prelude

import Marvel.Card.Builder
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Card.Side
import Marvel.Hand
import Marvel.Hp as X
import Marvel.Id as X
import Marvel.Source
import Marvel.Target

hero
  :: (HeroAttrs -> a)
  -> CardDef
  -> HP
  -> HandSize
  -> Thw
  -> Atk
  -> Def
  -> CardBuilder IdentityId a
hero f cardDef hp handSize thw atk def = CardBuilder
  { cbCardCode = cdCardCode cardDef
  , cbCardBuilder = \ident -> f $ HeroAttrs
    { heroIdentityId = ident
    , heroBaseHandSize = handSize
    , heroBaseThwart = thw
    , heroBaseAttack = atk
    , heroBaseDefense = def
    , heroAlterEgoForms = [A]
    , heroStartingHP = hp
    , heroCardDef = cardDef
    }
  }

class IsHero a

type HeroCard a = CardBuilder IdentityId a

newtype Atk = Atk Int
  deriving newtype (Show, Eq, ToJSON, FromJSON)

newtype Thw = Thw Int
  deriving newtype (Show, Eq, ToJSON, FromJSON)

newtype Def = Def Int
  deriving newtype (Show, Eq, ToJSON, FromJSON)

data HeroAttrs = HeroAttrs
  { heroIdentityId :: IdentityId
  , heroBaseHandSize :: HandSize
  , heroBaseThwart :: Thw
  , heroBaseAttack :: Atk
  , heroBaseDefense :: Def
  , heroAlterEgoForms :: [Side]
  , heroStartingHP :: HP
  , heroCardDef :: CardDef
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance HasStartingHP HeroAttrs where
  startingHP = heroStartingHP

instance HasCardCode HeroAttrs where
  toCardCode = toCardCode . heroCardDef

instance IsSource HeroAttrs where
  toSource = IdentitySource . heroIdentityId

instance IsTarget HeroAttrs where
  toTarget = IdentityTarget . heroIdentityId
