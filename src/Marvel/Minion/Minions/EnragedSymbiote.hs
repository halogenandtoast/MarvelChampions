module Marvel.Minion.Minions.EnragedSymbiote (
  enragedSymbiote,
  EnragedSymbiote (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Game.Source
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

-- | Guard and patrol are its keywords.
enragedSymbiote :: MinionCard EnragedSymbiote
enragedSymbiote = minion EnragedSymbiote Cards.enragedSymbiote (Sch 1) (Atk 2) (HP 2)

newtype EnragedSymbiote = EnragedSymbiote (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | [star] Boost: put Enraged Symbiote into play engaged with you.
instance RunMessage EnragedSymbiote where
  runMessage msg e@(EnragedSymbiote attrs) = runQueueT $ case msg of
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      -- "you": the player the attack is against (RR Defense)
      you <- getBoostedPlayer enemy
      e <$ putBoostIntoPlay target you
    _ -> EnragedSymbiote <$> runMessage msg attrs
