module Marvel.Treachery.Treacheries.NextEvolutionMutantSlayers where

import Marvel.Prelude
import Marvel.Card
import Marvel.Entity
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types
import Marvel.Ability (HasAbilities)
import Marvel.Game.Source
import Marvel.Trait (Trait (Marauder))
import Marvel.Modifier (HasModifiersFor)
import Marvel.Ref
import Marvel.GameValue ()
import Marvel.Stats ()
import Marvel.Hp ()
import Marvel.Message.Lifted

newtype MutantSlayersTreachery = MutantSlayersTreachery (Attrs Treachery)
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

ne40102 :: TreacheryCard MutantSlayersTreachery
ne40102 = treachery MutantSlayersTreachery Cards.ne40102

instance RunMessage MutantSlayersTreachery where
  runMessage msg e@(MutantSlayersTreachery a) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId a -> do
      titles <- map fst <$> getUniqueTitlesInPlay
      discardUntilIntoPlay you $
        CardWithType MinionType <> CardWithTrait Marauder
          <> NotCardMatching (CardMatchesAny $ map CardWithTitle titles)
      pure e
    _ -> MutantSlayersTreachery <$> runMessage msg a
