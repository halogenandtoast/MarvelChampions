module Marvel.Ally.Allies.BrawnAmadeusChoIronheart where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Payment ()
import Marvel.Resource
import Marvel.Stats ()

brawnAmadeusChoIronheart :: AllyCard BrawnAmadeusChoIronheart
brawnAmadeusChoIronheart = ally BrawnAmadeusChoIronheart Cards.brawnAmadeusChoIronheart (Thw 2, 1) (Atk 3, 1) (HP 3)

newtype BrawnAmadeusChoIronheart = BrawnAmadeusChoIronheart (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities BrawnAmadeusChoIronheart where
  getAbilities (BrawnAmadeusChoIronheart a) =
    [limitedAbility a 1 (PerPhase 1) Resource OwnsThis NoCost (Pay $ ResourcePayment Mental) | allyExhausted a]

instance RunMessage BrawnAmadeusChoIronheart where
  runMessage msg (BrawnAmadeusChoIronheart attrs) = runQueueT $ case msg of

    _ -> BrawnAmadeusChoIronheart <$> runMessage msg attrs
