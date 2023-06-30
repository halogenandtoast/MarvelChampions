module Marvel.Minion.Minions.Melter (
  melter,
  Melter (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Hp
import Marvel.Matchers hiding (ExhaustedAlly)
import Marvel.Message
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

melter :: MinionCard Melter
melter =
  minionWith
    Melter
    Cards.melter
    (Sch 1)
    (Atk 3)
    (HP 5)
    (defensePriorityL .~ AllyIfAble)

newtype Melter = Melter (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage Melter where
  runMessage msg e@(Melter attrs) = case msg of
    Boost msg' -> case msg' of
      RevealedAsBoost target _ | isTarget attrs target -> do
        allies <- selectList (AllyControlledBy You <> UnexhaustedAlly)
        e <$ pushAll (map (\a -> AllyMessage a ExhaustedAlly) allies)
      _ -> Melter <$> runMessage msg attrs
    _ -> Melter <$> runMessage msg attrs
