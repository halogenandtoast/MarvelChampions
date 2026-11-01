module Marvel.Treachery.Treacheries.AssassinationAttempt where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question ()
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Window qualified as W ()
import Marvel.Trait qualified as Trait
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

assassinationAttempt :: TreacheryCard AssassinationAttempt
assassinationAttempt = treachery AssassinationAttempt Cards.assassinationAttempt

newtype AssassinationAttempt = AssassinationAttempt (Attrs Treachery)
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities AssassinationAttempt
instance RunMessage AssassinationAttempt where
  runMessage msg t@(AssassinationAttempt attrs) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId attrs -> do
      assassins <- selectList (MinionWithTrait Trait.Assassin)
      if null assassins
        then searchAndRevealEncounterCard you (CardWithType MinionType <> CardWithTrait Trait.Assassin)
        else for_ assassins (\mid -> enemyActivationAfterCurrent mid $ enemyAttacks mid you)
      pure t
    _ -> AssassinationAttempt <$> runMessage msg attrs
