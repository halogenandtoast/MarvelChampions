module Marvel.Minion.Minions.Whirlwind (
  whirlwind,
  Whirlwind (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Cost
import Marvel.Damage
import Marvel.Matchers
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner
import Marvel.Window
import Marvel.Window qualified as W

whirlwind :: MinionCard Whirlwind
whirlwind = minion Whirlwind Cards.whirlwind (Sch 1) (Atk 2) (HP 6)

newtype Whirlwind = Whirlwind (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Whirlwind where
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
  runMessage msg e@(Whirlwind attrs) = case msg of
    -- the attack is resolved against each other hero once it is done with you, each
    -- defended on its own; "you" stays the player first attacked, so this does not fire
    -- again for them
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> do
      others <- filter (/= ident) <$> selectList HeroIdentity
      let
        self = EnemyMinionId (minionId attrs)
        attackDone = \case
          CheckWindows [W.Window W.After (W.EnemyAttack enemy ident')] -> enemy == self && ident' == ident
          _ -> False
      replaceMatchingMessage
        (\m -> m : map (MinionMessage (minionId attrs) . MinionBeginAttack) others)
        attackDone
      pure e
    Boost msg' -> case msg' of
      RevealedAsBoost target _ | isTarget attrs target -> do
        heroes <- selectList HeroIdentity
        pushAll
          [ IdentityMessage ident (IdentityDamaged (toSource attrs) (toDamage 1 FromAbility))
          | ident <- heroes
          ]
        pure e
      _ -> Whirlwind <$> runMessage msg attrs
    _ -> Whirlwind <$> runMessage msg attrs
