module Marvel.Ally.Allies.Bug (
  bug,
  Bug (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.GameValue
import Marvel.Id
import Marvel.Matchers
import Marvel.Window

bug :: AllyCard Bug
bug = ally Bug Cards.bug (Thw 1, 1) (Atk 1, 1) (HP 2)

newtype Bug = Bug (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | "Hero Response: After your hero makes a basic attack, heal 1 damage from Bug."
-- Only while he has damage to heal.
instance HasAbilities Bug where
  getAbilities a@(Bug attrs) =
    [ limitedWindowAbility
        a
        1
        (MakesBasicAttack After You)
        HeroResponse
        (OwnsThis <> AllyExists (AllyWithDamage (GreaterThan $ Static 0) <> AllyWithId (allyId attrs)))
        NoCost
        $ Heal (AllyCharacter $ allyId attrs) 1
    ]

instance RunMessage Bug where
  runMessage msg (Bug attrs) = Bug <$> runMessage msg attrs
