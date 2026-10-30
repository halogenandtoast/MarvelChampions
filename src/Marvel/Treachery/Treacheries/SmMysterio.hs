module Marvel.Treachery.Treacheries.SmMysterio where

import Marvel.Prelude
import Marvel.Card
import Marvel.Cost
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Hand
import Marvel.Id
import Marvel.Identity.Types (Field (PlayerIdentityHand))
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Projection
import Marvel.Query
import Marvel.Ref
import Marvel.Resource
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

newtype SmMysterioTreachery = SmMysterioTreachery (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

smDejaVu :: TreacheryCard SmMysterioTreachery
smDejaVu = treachery SmMysterioTreachery Cards.smDejaVu

smFearmonger :: TreacheryCard SmMysterioTreachery
smFearmonger = treachery SmMysterioTreachery Cards.smFearmonger

instance RunMessage SmMysterioTreachery where
  runMessage msg t@(SmMysterioTreachery attrs) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId attrs -> do
      if toCardCode attrs == "27092"
        then do
          chooseOne you
            [ labeled "$label.smDejaVuDamage" (damageIdentity you attrs $ toDamage 1 FromTreachery)
            , labeled "$label.smDejaVuThreat" (placeThreatOnEach attrs 1 EncounterMainScheme) ]
          cardSpecific attrs "shuffleDejaVu" you
        else do
          cards <- projectMap PlayerIdentityHand unHand you
          for_ cards discardCard
          drawToHandSize you
      pure t
    CardSpecific (isTarget attrs -> True) "shuffleDejaVu" (fromJSON -> Success you) -> do
      players <- selectList AnyIdentity
      let card = MkEncounterCard (CardId $ unTreacheryId $ treacheryId attrs) (getCardDef attrs)
      chooseOne you
        [targeting player do
            removeFromGame attrs
            shuffleIntoDeck player [encounterCardInPlayerDeck player card]
        | player <- players]
      pure t
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      you <- getBoostedPlayer enemy
      if toCardCode attrs == "27092"
        then revealBoost attrs you
        else do
          let cost = MultiResourceCost [Just Mental, Just Mental]
          affordable <- canAffordCost you (toSource attrs) cost
          chooseOrRunOne you $
            [labeled "$label.smDealThisEncounterCard" (dealBoostAsEncounterCard attrs you)]
              <> [labeled "$label.smSpendTwoMentalResources" (payForTreachery you cost) | affordable]
      pure t
    _ -> SmMysterioTreachery <$> runMessage msg attrs
