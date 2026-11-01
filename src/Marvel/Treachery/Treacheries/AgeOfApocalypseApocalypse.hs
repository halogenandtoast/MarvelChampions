module Marvel.Treachery.Treacheries.AgeOfApocalypseApocalypse where

import Marvel.Prelude
import Marvel.Game.Source ()
import Marvel.Matchers
import Marvel.Query
import Marvel.Scenario.AgeOfApocalypseApocalypse
import Marvel.Trait qualified as Trait
import Marvel.Card
import Marvel.Entity
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types
import Marvel.Ability (HasAbilities)
import Marvel.Modifier (HasModifiersFor)
import Marvel.Ref
import Marvel.GameValue ()
import Marvel.Stats ()
import Marvel.Hp ()
import Marvel.Message.Lifted

newtype AoaApocalypseTreachery = AoaApocalypseTreachery (Attrs Treachery)
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

wolfAmongSheep :: TreacheryCard AoaApocalypseTreachery
wolfAmongSheep = treachery AoaApocalypseTreachery Cards.wolfAmongSheep


instance RunMessage AoaApocalypseTreachery where
  runMessage msg a@(AoaApocalypseTreachery attrs) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId attrs -> do
      prelates <- selectList $ MinionWithTrait Trait.Prelate
      if null prelates then apocalypse >>= traverse_ (`villainActivatesAgainst` you)
        else for_ prelates $ \mid -> minionActivatesAgainst mid you
      pure a
    Boost (RevealedAsBoost target _) | isTarget attrs target -> do
      prelates <- selectList $ MinionWithTrait Trait.Prelate
      if null prelates then apocalypse >>= traverse_ enemyBecomesTough else traverse_ enemyBecomesTough prelates
      pure a
    _ -> AoaApocalypseTreachery <$> runMessage msg attrs
