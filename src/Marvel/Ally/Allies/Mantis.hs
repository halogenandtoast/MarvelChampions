module Marvel.Ally.Allies.Mantis (
  mantis,
  Mantis (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers

mantis :: AllyCard Mantis
mantis = ally Mantis Cards.mantis (Thw 2, 1) (Atk 1, 2) (HP 3)

newtype Mantis = Mantis (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | "Action: Exhaust Mantis and deal 1 damage to her → heal 3 damage from an identity."
instance HasAbilities Mantis where
  getAbilities a =
    [ ability a 1 Action (OwnsThis <> CharacterExists (IdentityCharacters healableIdentity)) (ExhaustCost <> DamageThisCost 1) $
        ChooseHeal 3 (IdentityCharacters AnyIdentity)
    ]

instance RunMessage Mantis where
  runMessage msg (Mantis attrs) = Mantis <$> runMessage msg attrs
