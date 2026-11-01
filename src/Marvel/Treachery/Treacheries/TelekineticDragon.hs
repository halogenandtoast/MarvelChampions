module Marvel.Treachery.Treacheries.TelekineticDragon where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Resource
import Marvel.Status
import Marvel.Minion.Minions.Chimera (printedResourcesControlled)
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

telekineticDragon :: TreacheryCard TelekineticDragon
telekineticDragon = treachery TelekineticDragon Cards.telekineticDragon

newtype TelekineticDragon = TelekineticDragon (Attrs Treachery)
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage TelekineticDragon where
  runMessage msg a@(TelekineticDragon attrs) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId attrs -> do
      n <- printedResourcesControlled you Mental
      if n == 0
        then pure $ TelekineticDragon attrs {treacherySurge = True}
        else a <$ dealIndirectDamage you attrs n
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      you <- getBoostedPlayer enemy
      let cost = ResourceCost $ Just Mental
      payable <- canAffordCost you (toRef attrs) cost
      chooseOne you $
        [labeled "$label.spendMentalResource" $ do
          continuation <- capture (pure ())
          payCostThen you attrs cost continuation
        | payable]
        <> [labeled "$label.confuseYourIdentity" (giveStatus you ConfusedStatus)]
      pure a
    _ -> TelekineticDragon <$> runMessage msg attrs
