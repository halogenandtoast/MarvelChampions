module Marvel.Treachery.Treacheries.TentacleStrike where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Status
import Marvel.Window qualified as W
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

tentacleStrike :: TreacheryCard TentacleStrike
tentacleStrike = treachery TentacleStrike Cards.tentacleStrike

newtype TentacleStrike = TentacleStrike (Attrs Treachery)
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage TentacleStrike where
  runMessage msg e@(TentacleStrike attrs) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId attrs -> do
      stunned <- identityMatches StunnedIdentity you
      giveStatus you StunnedStatus
      damageIdentity you attrs (toDamage (if stunned then 4 else 1) FromTreachery)
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      you <- getBoostedPlayer enemy
      revealBoost attrs you
      pure e
    _ -> TentacleStrike <$> runMessage msg attrs
