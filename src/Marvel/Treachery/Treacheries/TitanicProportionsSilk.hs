module Marvel.Treachery.Treacheries.TitanicProportionsSilk where
import Marvel.Prelude
import Marvel.Query
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Matchers
import Marvel.Question ()
import Marvel.Window qualified as W ()
import Marvel.Trait qualified as T ()
import Marvel.Damage ()
import Marvel.Game.Source
import Marvel.Card

import Marvel.Ability
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Silk.Cards qualified as Cards
import Marvel.Treachery.Types

titanicProportionsSilk :: TreacheryCard TitanicProportionsSilk
titanicProportionsSilk = treachery TitanicProportionsSilk Cards.titanicProportionsSilk
newtype TitanicProportionsSilk = TitanicProportionsSilk (Attrs Treachery)
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance RunMessage TitanicProportionsSilk where
  runMessage msg u@(TitanicProportionsSilk attrs) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery _you) | tid == treacheryId attrs -> do
      minions <- selectList (MinionIs Cards.atlasSilk)
      n <- sum <$> traverse (getCountersOn . MinionRef) minions
      when (n > 0) $ dealIndirectDamageAmongPlayers attrs n
      when (n < 3) $ gainSurge attrs
      pure u
    Boost (RevealedAsBoost target _) | isTarget attrs target -> do
      minions <- selectList (MinionIs Cards.atlasSilk)
      traverse_ (`placeCounters` 1) minions
      pure u
    _ -> TitanicProportionsSilk <$> runMessage msg attrs
