module Marvel.Treachery.Treacheries.AgeOfApocalypseInfinites where

import Marvel.Prelude
import Marvel.Card
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Ref
import Marvel.Ability (HasAbilities)
import Marvel.Modifier (HasModifiersFor)
import Marvel.SideScheme.SideSchemes.AgeOfApocalypseInfinites (placeGenePoolThreat)
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

newtype AoaInfinitesTreachery = AoaInfinitesTreachery (Attrs Treachery)
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

cullingTheWeak :: TreacheryCard AoaInfinitesTreachery
cullingTheWeak = treachery AoaInfinitesTreachery Cards.cullingTheWeak

instance RunMessage AoaInfinitesTreachery where
  runMessage msg e@(AoaInfinitesTreachery a) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery _) | tid == treacheryId a -> e <$ placeGenePoolThreat 4
    Boost (RevealedAsBoost target _) | isTarget a target -> e <$ placeGenePoolThreat 2
    _ -> AoaInfinitesTreachery <$> runMessage msg a
