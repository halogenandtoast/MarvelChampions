module Marvel.Minion.Minions.Terminatrix (
  terminatrix,
  Terminatrix (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Keyword
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

terminatrix :: MinionCard Terminatrix
terminatrix = minion Terminatrix Cards.terminatrix (Sch 2) (Atk 2) (HP 5)

newtype Terminatrix = Terminatrix (Attrs Minion)
  deriving anyclass (IsMinion, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Quickstrike. [star] Her attacks gain piercing.
instance HasModifiersFor Terminatrix where
  getModifiersFor _ (MinionRef mid) (Terminatrix a) | mid == minionId a = pure [KeywordModifier Piercing]
  getModifiersFor _ _ _ = pure []

-- | [star] Boost: this enemy is given 2 more boost cards.
instance RunMessage Terminatrix where
  runMessage msg e@(Terminatrix attrs) = runQueueT $ case msg of
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      dealBoost enemy
      dealBoost enemy
      pure e
    _ -> Terminatrix <$> runMessage msg attrs
