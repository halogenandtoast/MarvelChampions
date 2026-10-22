module Marvel.Ally.Allies.CaptainMarvelCarolDanvers (
  captainMarvelCarolDanvers,
  CaptainMarvelCarolDanvers (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Window hiding (AllyAttacked, AllyThwarted)
import Marvel.Window qualified as W

captainMarvelCarolDanvers :: AllyCard CaptainMarvelCarolDanvers
captainMarvelCarolDanvers = ally CaptainMarvelCarolDanvers Cards.captainMarvelCarolDanvers (Thw 2, 1) (Atk 2, 1) (HP 3)

newtype CaptainMarvelCarolDanvers = CaptainMarvelCarolDanvers (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

-- | Response: After Captain Marvel uses a basic power, draw 1 card.
instance HasAbilities CaptainMarvelCarolDanvers where
  getAbilities a@(CaptainMarvelCarolDanvers attrs) =
    [ limitedWindowAbility
        a
        1
        (WindowOneOf [W.AllyThwarted After self AnyScheme, W.AllyAttacked After self AnyEnemy])
        Response
        OwnsThis
        NoCost
        (runAbility a 1)
    ]
   where
    self = AllyWithId (allyId attrs)

instance RunMessage CaptainMarvelCarolDanvers where
  runMessage msg a@(CaptainMarvelCarolDanvers attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget a -> True) 1 _ _ -> do
      drawCards ident 1
      pure a
    _ -> CaptainMarvelCarolDanvers <$> runMessage msg attrs
