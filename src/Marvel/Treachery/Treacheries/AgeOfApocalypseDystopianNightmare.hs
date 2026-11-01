module Marvel.Treachery.Treacheries.AgeOfApocalypseDystopianNightmare where

import Marvel.Prelude
import Marvel.Card
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Ref
import Marvel.Ability (HasAbilities)
import Marvel.Damage
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Modifier (HasModifiersFor)
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

newtype AoaDystopianNightmareTreachery = AoaDystopianNightmareTreachery (Attrs Treachery)
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

warWearyDystopianNightmare :: TreacheryCard AoaDystopianNightmareTreachery
warWearyDystopianNightmare = treachery AoaDystopianNightmareTreachery Cards.warWearyDystopianNightmare

warWeary :: ReverseQueue m => Attrs Treachery -> IdentityId -> m ()
warWeary a you = do
  stunned <- identityMatches StunnedIdentity you
  if stunned then damageIdentity you a (toDamage 2 FromAbility) else stunIdentity you

instance RunMessage AoaDystopianNightmareTreachery where
  runMessage msg e@(AoaDystopianNightmareTreachery a) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId a -> e <$ warWeary a you
    Boost (RevealedAsBoost target enemy) | isTarget a target -> do
      you <- getBoostedPlayer enemy
      warWeary a you
      pure e
    _ -> AoaDystopianNightmareTreachery <$> runMessage msg a
