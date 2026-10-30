module Marvel.Minion.Minions.MisterSinister where

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
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner
import Marvel.Status

misterSinister :: MinionCard MisterSinister
misterSinister = minion MisterSinister Cards.misterSinister (Sch 1) (Atk 1) (HP 6)

newtype MisterSinister = MisterSinister (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage MisterSinister where
  runMessage msg e@(MisterSinister attrs) = runQueueT $ case msg of
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      you <- getBoostedPlayer enemy
      stunned <- identityMatches StunnedIdentity you
      if stunned then damageIdentity you attrs (toDamage 2 FromAbility) else giveStatus you StunnedStatus
      pure e
    _ -> MisterSinister <$> runMessage msg attrs
