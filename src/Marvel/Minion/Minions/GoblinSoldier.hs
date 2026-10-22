module Marvel.Minion.Minions.GoblinSoldier (
  goblinSoldier,
  GoblinSoldier (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner
import Marvel.Game.Source
import Marvel.Damage

goblinSoldier :: MinionCard GoblinSoldier
goblinSoldier = minion GoblinSoldier Cards.goblinSoldier (Sch 1) (Atk 1) (HP 5)

newtype GoblinSoldier = GoblinSoldier (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage GoblinSoldier where
  runMessage msg e@(GoblinSoldier attrs) = runQueueT $ case msg of
    -- When Defeated: deal 1 damage to the engaged player
    MinionMessage ident MinionDefeated | ident == minionId attrs -> do
      damageIdentity (minionEngagedIdentity attrs) attrs (toDamage 1 FromAbility)
      GoblinSoldier <$> runMessage msg attrs
    -- Boost: put this minion into play engaged with you
    Boost (RevealedAsBoost target _) | isTarget attrs target -> do
      you <- getActivePlayerId
      e <$ putBoostIntoPlay target you
    _ -> GoblinSoldier <$> runMessage msg attrs
