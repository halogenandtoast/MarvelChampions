module Marvel.Minion.Minions.Scorpion (
  scorpion,
  Scorpion (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Cost
import Marvel.Matchers
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner
import Marvel.Window

scorpion :: MinionCard Scorpion
scorpion = minion Scorpion Cards.scorpion (Sch 3) (Atk 3) (HP 7)

newtype Scorpion = Scorpion (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Quickstrike. [star] Forced Response: after Scorpion attacks and damages a character,
-- stun that character.
instance HasAbilities Scorpion where
  getAbilities (Scorpion a) =
    [ windowAbility a 1 (EnemyAttackedAndDamaged (EnemyWithId $ EnemyMinionId $ minionId a) AnyCharacter) ForcedResponse NoCost
        $ RunAbility (toTarget a) 1
    ]

instance RunMessage Scorpion where
  runMessage msg e@(Scorpion attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 [EnemyAttacksAndDamages _ character] _ -> do
      giveStatus character StunnedStatus
      pure e
    _ -> Scorpion <$> runMessage msg attrs
