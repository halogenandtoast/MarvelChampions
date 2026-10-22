module Marvel.Ally.Allies.Brawn (
  brawn,
  Brawn (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner hiding (AllyAttacked)
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Window

brawn :: AllyCard Brawn
brawn = ally Brawn Cards.brawn (Thw 1, 1) (Atk 1, 1) (HP 5)

newtype Brawn = Brawn (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef, HasController)

-- | "Response: After Brawn attacks, remove 1 threat from a scheme."
instance HasAbilities Brawn where
  getAbilities (Brawn a) =
    [ limitedWindowAbility
        a
        1
        (AllyAttacked After (AllyWithId $ allyId a) AnyEnemy)
        Response
        (OwnsThis <> SchemeExists ThwartableScheme)
        NoCost
        (RemoveThreat (toSource a) 1 ThwartableScheme)
    ]

instance RunMessage Brawn where
  runMessage msg (Brawn attrs) = Brawn <$> runMessage msg attrs
