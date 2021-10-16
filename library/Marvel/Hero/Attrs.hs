module Marvel.Hero.Attrs
  ( module Marvel.Hero.Attrs
  , module X
  ) where

import Marvel.Prelude

import Marvel.Card.Builder
import Marvel.Card.Def
import Marvel.Card.Side
import Marvel.Hp as X
import Marvel.Id as X
import Marvel.Identity.Attrs as X
import Marvel.Message

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
    { heroIdentityAttrs = defaultAttrs ident cardDef hp
    , heroBaseHandSize = handSize
    , heroBaseThwart = thw
    , heroBaseAttack = atk
    , heroBaseDefense = def
    , heroAlterEgoForms = [A]
    }
  }

class IsHero a

type HeroCard a = CardBuilder IdentityId a

newtype HandSize = HandSize Int
  deriving newtype (Show, Eq, ToJSON, FromJSON)

newtype Atk = Atk Int
  deriving newtype (Show, Eq, ToJSON, FromJSON)

newtype Thw = Thw Int
  deriving newtype (Show, Eq, ToJSON, FromJSON)

newtype Def = Def Int
  deriving newtype (Show, Eq, ToJSON, FromJSON)

data HeroAttrs = HeroAttrs
  { heroIdentityAttrs :: IdentityAttrs
  , heroBaseHandSize :: HandSize
  , heroBaseThwart :: Thw
  , heroBaseAttack :: Atk
  , heroBaseDefense :: Def
  , heroAlterEgoForms :: [Side]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance RunMessage HeroAttrs where
  runMessage msg x = do
    identityAttrs' <- runMessage msg (heroIdentityAttrs x)
    pure $ x { heroIdentityAttrs = identityAttrs' }

instance HasStartingHP HeroAttrs where
  startingHP = startingHP . view identityAttrsL

instance HasIdentityAttrs HeroAttrs where
  identityAttrsL = lens heroIdentityAttrs \m x -> m { heroIdentityAttrs = x }
