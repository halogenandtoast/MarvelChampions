module Marvel.Environment.Environments.UltronDronesVision (ultronDronesVision, UltronDronesVision (..)) where

import Marvel.Prelude

import Marvel.Ability (HasAbilities)
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Environment.Cards qualified as Cards
import Marvel.Environment.Types
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref

ultronDronesVision :: EnvironmentCard UltronDronesVision
ultronDronesVision = environment UltronDronesVision Cards.ultronDronesVision

newtype UltronDronesVision = UltronDronesVision (Attrs Environment)
  deriving anyclass (IsEnvironment, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance RunMessage UltronDronesVision where
  runMessage msg (UltronDronesVision attrs) = runQueueT $ case msg of
    _ -> UltronDronesVision <$> runMessage msg attrs
