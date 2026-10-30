module Marvel.Environment.Environments.SmVenomGoblin where

import Marvel.Prelude
import Marvel.Ability.Types
import Marvel.MainScheme.MainSchemes.SmVenomGoblin qualified as Goblin
import Marvel.Card.Code
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hp
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Stats
import Marvel.Environment.Cards qualified as Cards
import Marvel.Environment.Types

newtype SmVenomGoblinEnvironment = SmVenomGoblinEnvironment (Attrs Environment)
  deriving anyclass (IsEnvironment, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

smSkiesOverNewYorkB :: EnvironmentCard SmVenomGoblinEnvironment
smSkiesOverNewYorkB = environment SmVenomGoblinEnvironment Cards.smSkiesOverNewYorkB

smLowerManhattanB :: EnvironmentCard SmVenomGoblinEnvironment
smLowerManhattanB = environment SmVenomGoblinEnvironment Cards.smLowerManhattanB

smMidtownManhattanB :: EnvironmentCard SmVenomGoblinEnvironment
smMidtownManhattanB = environment SmVenomGoblinEnvironment Cards.smMidtownManhattanB

smUpperManhattanB :: EnvironmentCard SmVenomGoblinEnvironment
smUpperManhattanB = environment SmVenomGoblinEnvironment Cards.smUpperManhattanB

instance RunMessage SmVenomGoblinEnvironment where
  runMessage msg e@(SmVenomGoblinEnvironment attrs) = runQueueT $ case msg of
    EnvironmentMessage eid EnvironmentEntered | eid == environmentId attrs -> do
      whenM ((>= 2) <$> Goblin.symbioteEnvironmentCount) $ gameOver Lost
      pure e
    _ -> SmVenomGoblinEnvironment <$> runMessage msg attrs
