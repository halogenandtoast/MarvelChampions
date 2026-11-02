module Marvel.Treachery.Treacheries.ExtremeRiskShuri where
import Marvel.Prelude
import Marvel.Id
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
import Marvel.Card.Id ()
import Marvel.Silk.Helpers ()

import Marvel.Ability
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.BlackPantherShuri.Cards qualified as Cards
import Marvel.Treachery.Types

extremeRiskShuri :: TreacheryCard ExtremeRiskShuri
extremeRiskShuri = treachery ExtremeRiskShuri Cards.extremeRiskShuri
newtype ExtremeRiskShuri = ExtremeRiskShuri (Attrs Treachery)
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance RunMessage ExtremeRiskShuri where
  runMessage msg u@(ExtremeRiskShuri attrs) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId attrs -> do
      present <- selectOne (MinionIs Cards.joystickShuri)
      case present of
        Just mid -> engageMinion mid you >> cardSpecific attrs "activate" (you,mid)
        Nothing -> searchEverywhereAndRevealEncounterCard you (CardIs Cards.joystickShuri) >> cardSpecific attrs "found" you
      pure u
    CardSpecific (isTarget attrs -> True) "found" (fromJSON -> Success (you :: IdentityId)) -> do
      present <- selectOne (MinionIs Cards.joystickShuri)
      case present of
        Nothing -> gainSurge attrs
        Just mid -> cardSpecific attrs "activate" (you,mid)
      pure u
    CardSpecific (isTarget attrs -> True) "activate" (fromJSON -> Success (you,mid)) -> do
      hero <- identityMatches HeroIdentity you
      statuses <- enemyMatches (if hero then StunnedEnemy else ConfusedEnemy) (EnemyMinionId mid)
      forbidden <- elem CannotActivate <$> getModifiers (MinionRef mid)
      present <- selectAny (MinionWithId mid)
      minionActivatesAgainst mid you
      when (statuses || forbidden || not present) (gainSurge attrs)
      pure u
    CardSpecific (isTarget attrs -> True) "activated" _ -> pure u
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      you <- getActivePlayerId
      chooseOne you [labeled "$label.done" (pure ()), labeled "$label.bpRiskBoost" (dealBoost enemy >> drawCards you 1)]
      pure u
    _ -> ExtremeRiskShuri <$> runMessage msg attrs
