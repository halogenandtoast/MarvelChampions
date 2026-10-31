module Marvel.Treachery.Treacheries.AcolyteFrenzy where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Window qualified as W
import Marvel.Trait qualified as Trait
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

acolyteFrenzy :: TreacheryCard AcolyteFrenzy
acolyteFrenzy = treachery AcolyteFrenzy Cards.acolyteFrenzy

newtype AcolyteFrenzy = AcolyteFrenzy (Attrs Treachery)
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities AcolyteFrenzy
instance RunMessage AcolyteFrenzy where
  runMessage msg t@(AcolyteFrenzy attrs) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId attrs -> do
      acolytes <- selectList (MinionWithTrait Trait.Acolyte <> MinionEngagedWith (IdentityWithId you))
      if null acolytes
        then gainSurge attrs
        else for_ acolytes $ \mid -> enemyActivationAfterCurrent mid $ minionActivatesAgainst mid you
      pure t
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      you <- getBoostedPlayer enemy
      giveStatus you StunnedStatus
      giveStatus you ConfusedStatus
      pure t
    _ -> AcolyteFrenzy <$> runMessage msg attrs
