module Marvel.Minion.Minions.HydraJetTrooper (
  hydraJetTrooper,
  HydraJetTrooper (..),
) where

import Marvel.Prelude

import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Ability
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

hydraJetTrooper :: MinionCard HydraJetTrooper
hydraJetTrooper = minion HydraJetTrooper Cards.hydraJetTrooper (Sch 0) (Atk 2) (HP 3)

newtype HydraJetTrooper = HydraJetTrooper (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Quickstrike. [star] Boost: if you are in hero form, the villain attacks you after
-- this activation, no boost cards dealt for that attack (RR Activation: it follows the
-- current activation).
instance RunMessage HydraJetTrooper where
  runMessage msg e@(HydraJetTrooper attrs) = runQueueT $ case msg of
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      you <- getBoostedPlayer enemy
      hero <- identityMatches HeroIdentity you
      villain <- selectOne ActiveVillain
      for_ villain \vid -> when hero $ activationAfter enemy vid (villainAttacksWithoutBoost vid you)
      pure e
    _ -> HydraJetTrooper <$> runMessage msg attrs
