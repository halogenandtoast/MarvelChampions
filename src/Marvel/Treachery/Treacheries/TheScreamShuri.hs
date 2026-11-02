module Marvel.Treachery.Treacheries.TheScreamShuri where
import Marvel.Prelude
import Marvel.Query
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Matchers
import Marvel.Question ()
import Marvel.Window qualified as W ()
import Marvel.Trait qualified as T ()
import Marvel.Damage
import Marvel.Game.Source
import Marvel.Card
import Marvel.Status

import Marvel.Ability
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.BlackPantherShuri.Cards qualified as Cards
import Marvel.Treachery.Types

theScreamShuri :: TreacheryCard TheScreamShuri
theScreamShuri = treachery TheScreamShuri Cards.theScreamShuri
newtype TheScreamShuri = TheScreamShuri (Attrs Treachery)
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance RunMessage TheScreamShuri where
  runMessage msg u@(TheScreamShuri attrs) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId attrs -> do
      characters <- selectList (CharacterControlledBy $ IdentityWithId you)
      alreadyStunned <- filterM (characterMatches $ CharacterWithStatus StunnedStatus) characters
      traverse_ (`giveStatus` StunnedStatus) characters
      damageCharacters attrs (toDamage 1 FromTreachery) alreadyStunned
      pure u
    Boost (RevealedAsBoost target _) | isTarget attrs target -> do
      you <- getActivePlayerId
      stunned <- identityMatches StunnedIdentity you
      stunIdentity you
      when stunned $ damageIdentity you attrs (toDamage 1 FromAbility)
      pure u
    _ -> TheScreamShuri <$> runMessage msg attrs
