module Marvel.Ally.Allies.Warlock (
  warlock,
  Warlock (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Id
import Marvel.Matchers
import Marvel.Resource

warlock :: AllyCard Warlock
warlock = ally Warlock Cards.warlock (Thw 1, 1) (Atk 1, 1) (HP 3)

newtype Warlock = Warlock (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

-- | Action: Spend a [mental] resource → heal up to 2 damage from Warlock (only while
-- he has damage to heal).
instance HasAbilities Warlock where
  getAbilities a@(Warlock attrs) =
    [ ability
        a
        1
        Action
        (OwnsThis <> CharacterExists (AllyCharacters (AllyWithId $ allyId attrs) <> healableCharacter))
        (ResourceCost (Just Mental))
        $ Heal (AllyCharacter $ allyId attrs) 2
    ]

instance RunMessage Warlock where
  runMessage msg (Warlock attrs) = Warlock <$> runMessage msg attrs
