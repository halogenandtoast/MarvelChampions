module Marvel.Treachery.Treacheries.ConcussiveForce where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Stats
import Marvel.Window qualified as W
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types
import Marvel.Minion.Cards qualified as Minions

concussiveForce :: TreacheryCard ConcussiveForce
concussiveForce = treachery ConcussiveForce Cards.concussiveForce

newtype ConcussiveForce = ConcussiveForce (Attrs Treachery)
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage ConcussiveForce where
  runMessage msg e@(ConcussiveForce attrs) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId attrs -> do
      hero <- identityMatches HeroIdentity you
      sinister <- selectOne (MinionIs Minions.misterSinister)
      case sinister of
        Just mid -> if hero then enemyAttacks mid you else minionSchemes mid
        Nothing -> if hero
          then damageIdentity you attrs (toDamage 2 FromAbility)
          else placeThreatOnEach attrs 2 EncounterMainScheme
      pure e
    _ -> ConcussiveForce <$> runMessage msg attrs
