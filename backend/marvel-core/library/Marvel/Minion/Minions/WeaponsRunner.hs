module Marvel.Minion.Minions.WeaponsRunner
  ( weaponsRunner
  , WeaponsRunner(..)
  ) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Hp
import Marvel.Message
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Types

weaponsRunner :: MinionCard WeaponsRunner
weaponsRunner = minion WeaponsRunner Cards.weaponsRunner (Sch 1) (Atk 1) (HP 2)

newtype WeaponsRunner = WeaponsRunner MinionAttrs
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage WeaponsRunner where
  runMessage msg e@(WeaponsRunner attrs) = case msg of
    Boost msg' -> case msg' of
      RevealedAsBoost target _ | isTarget attrs target -> do
        ident <- getActivePlayerId
        e <$ push (PutBoostIntoPlay target ident)
      _ -> WeaponsRunner <$> runMessage msg attrs
    _ -> WeaponsRunner <$> runMessage msg attrs
