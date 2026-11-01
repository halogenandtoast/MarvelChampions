module Marvel.Minion.Minions.Whirlwind (
  whirlwind,
  Whirlwind (..),
) where

import Marvel.Prelude

import Marvel.Attack (attackIsSimultaneousAdditional)
import Marvel.Ability
import Marvel.Cost
import Marvel.Damage
import Marvel.Matchers
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner
import Marvel.Window

whirlwind :: MinionCard Whirlwind
whirlwind = minion Whirlwind Cards.whirlwind (Sch 1) (Atk 2) (HP 6)

newtype Whirlwind = Whirlwind (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Whirlwind where
  getAbilities (Whirlwind attrs) | maybe False attackIsSimultaneousAdditional (minionAttacking attrs) = []
  getAbilities (Whirlwind a) =
    [ windowAbility
        a
        1
        (EnemyAttacked When (EnemyWithId $ EnemyMinionId $ minionId a) You)
        ForcedInterrupt
        NoCost
        $ RunAbility (toTarget a) 1
    ]

instance RunMessage Whirlwind where
  runMessage msg e@(Whirlwind attrs) = runQueueT $ case msg of
    -- Each hero has a separate attack, with each step resolved for all heroes
    -- before the next step (RR Whirlwind FAQ).
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> do
      others <- filter (/= ident) <$> selectList HeroIdentity
      simultaneousMinionAttacks (minionId attrs) ident others
      pure e
    Boost msg' -> case msg' of
      RevealedAsBoost target _ | isTarget attrs target -> do
        heroes <- selectList HeroIdentity
        for_ heroes \ident -> damageIdentity ident attrs (toDamage 1 FromAbility)
        pure e
      _ -> Whirlwind <$> runMessage msg attrs
    _ -> Whirlwind <$> runMessage msg attrs
