module Marvel.Treachery.Treacheries.RelentlessAndroid where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ability qualified as Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Query
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Trait qualified as Trait
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types
import Marvel.Upgrade.Cards qualified as Upgrades
import Marvel.Window qualified as W
import Marvel.Environment.Cards qualified as Environments
relentlessAndroid :: TreacheryCard RelentlessAndroid
relentlessAndroid = treachery RelentlessAndroid Cards.relentlessAndroid

newtype RelentlessAndroid = RelentlessAndroid (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage RelentlessAndroid where
  runMessage msg t@(RelentlessAndroid attrs) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId attrs -> do
      drones <- selectAny (EnvironmentWithTitle "Ultron Drones")
      if drones
        then do
          putTopOfDeckIntoPlayAsDrone you
          putTopOfDeckIntoPlayAsDrone you
        else discardFrom you RandomFromHand 2
      pure t
    _ -> RelentlessAndroid <$> runMessage msg attrs
