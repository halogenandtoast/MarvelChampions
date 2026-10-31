module Marvel.SideScheme.SideSchemes.TheCarbonadiumSynthesizer where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Status
import Marvel.Window qualified as W
import Marvel.GameValue
import Marvel.Minion.Cards qualified as Minions
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

theCarbonadiumSynthesizer :: SideSchemeCard TheCarbonadiumSynthesizer
theCarbonadiumSynthesizer = sideScheme TheCarbonadiumSynthesizer Cards.theCarbonadiumSynthesizer (Static 5)

newtype TheCarbonadiumSynthesizer = TheCarbonadiumSynthesizer (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor TheCarbonadiumSynthesizer where
  getModifiersFor _ (MinionRef mid) _ = do
    omega <- selectAny (MinionWithId mid <> MinionIs Minions.omegaRed)
    pure [CannotBeDefeated | omega]
  getModifiersFor _ _ _ = pure []

instance RunMessage TheCarbonadiumSynthesizer where
  runMessage msg (TheCarbonadiumSynthesizer attrs) = runQueueT do
    when (leaves msg) $ do
      omegas <- selectList (MinionIs Minions.omegaRed)
      for_ omegas (`minionStep` MinionCheckHitPoints)
    TheCarbonadiumSynthesizer <$> runMessage msg attrs
   where
    leaves = \case
      RemoveFromPlay target -> target == toTarget attrs
      RemoveFromPlayAfterWindow target -> target == toTarget attrs
      RemoveFromGame target -> target == toTarget attrs
      AddToVictoryDisplay target -> target == toTarget attrs
      _ -> False
