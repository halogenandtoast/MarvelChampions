module Marvel.Treachery.Treacheries.HuntingtheSpiderBrideSilk where
import Marvel.Prelude
import Marvel.Id
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Matchers ()
import Marvel.Question ()
import Marvel.Window qualified as W ()
import Marvel.Trait qualified as T ()
import Marvel.Damage ()
import Marvel.Game.Source ()
import Marvel.Card
import Marvel.Card.Id ()
import Marvel.Silk.Helpers ()
import System.Random.Shuffle (shuffleM)

import Marvel.Ability
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Silk.Cards qualified as Cards
import Marvel.Treachery.Types

huntingtheSpiderBrideSilk :: TreacheryCard HuntingtheSpiderBrideSilk
huntingtheSpiderBrideSilk = treachery HuntingtheSpiderBrideSilk Cards.huntingtheSpiderBrideSilk
newtype HuntingtheSpiderBrideSilk = HuntingtheSpiderBrideSilk (Attrs Treachery)
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance RunMessage HuntingtheSpiderBrideSilk where
  runMessage msg u@(HuntingtheSpiderBrideSilk attrs) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId attrs -> do
      cards <- tuckedEncounterCards you
      when (length cards == 4) $ do
        shuffled <- shuffleM cards
        for_ (headMay shuffled) (discardTuckedEncounterCardFromEncounter you)
      let card = MkEncounterCard (CardId $ unTreacheryId $ treacheryId attrs) (getCardDef attrs)
      tuckEncounterCardFromEncounter you card
      pure u
    TreacheryMessage tid (ResolvedTreachery you) | tid == treacheryId attrs -> do
      removeFromGame attrs
      surge you
      pure u
    _ -> HuntingtheSpiderBrideSilk <$> runMessage msg attrs
