module Marvel.Minion.Minions.Crossfire (
  crossfire,
  Crossfire (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Keyword (Keyword (Piercing))
import Marvel.Matchers
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

crossfire :: MinionCard Crossfire
crossfire = minion Crossfire Cards.crossfire (Sch 1) (Atk 2) (HP 4)

-- | Quickstrike, and attacks that gain piercing: its keywords.
newtype Crossfire = Crossfire (Attrs Minion)
  deriving anyclass (IsMinion, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | [star] Boost: If this boost resolves during an attack, the attack gains piercing.
instance RunMessage Crossfire where
  runMessage msg e@(Crossfire attrs) = runQueueT $ case msg of
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      attacking <- selectAny $ EnemyWithId enemy <> AttackingEnemy
      when attacking $ attackGains enemy Piercing
      pure e
    _ -> Crossfire <$> runMessage msg attrs
