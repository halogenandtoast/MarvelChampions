module Marvel.Treachery.Treacheries.WarDelivery where

import Marvel.Prelude
import Marvel.Card
import Marvel.Cost
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Hand
import Marvel.Identity.Types (Field (PlayerIdentityHand))
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Cards qualified as Minions
import Marvel.Projection
import Marvel.Query
import Marvel.Ref
import Marvel.Resource
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

warDelivery :: TreacheryCard WarDelivery
warDelivery = treachery WarDelivery Cards.warDelivery

newtype WarDelivery = WarDelivery (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage WarDelivery where
  runMessage msg t@(WarDelivery attrs) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId attrs -> do
      let cost = ResourceCost (Just Wild)
      affordable <- canAffordCost you (toSource attrs) cost
      chooseOrRunOne you $
        [labeled "$label.spendWildResource" (payForTreachery you cost) | affordable]
          <> [labeled "$label.declineWarDeliveryPayment" do
                currentVillainAttacks you
                warbringer <- selectOne (MinionIs Minions.warbringer)
                for_ warbringer (`enemyAttacks` you)
             ]
      pure t
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      you <- getBoostedPlayer enemy
      hand <- projectMap PlayerIdentityHand unHand you
      let n = fromIntegral $ length $ filter (elem Wild . printedResources . getCardDef) hand
      placeThreatOnEach attrs n EncounterMainScheme
      pure t
    _ -> WarDelivery <$> runMessage msg attrs
