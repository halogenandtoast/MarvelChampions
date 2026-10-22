module Marvel.Ally.Allies.Mockingbird (
  mockingbird,
  Mockingbird (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Window

mockingbird :: AllyCard Mockingbird
mockingbird = ally Mockingbird Cards.mockingbird (Thw 2, 1) (Atk 2, 1) (HP 3)

newtype Mockingbird = Mockingbird (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

-- | Interrupt: When the villain initiates an attack against you, spend 1 resource of
-- any type and return Mockingbird to your hand → prevent all damage from this attack.
instance HasAbilities Mockingbird where
  getAbilities a@(Mockingbird attrs) =
    [ limitedWindowAbility
        a
        1
        (EnemyAttacked When VillainEnemy (IdentityWithId $ allyController attrs))
        Interrupt
        OwnsThis
        (ResourceCost Nothing)
        (runAbility a 1)
    ]

instance RunMessage Mockingbird where
  runMessage msg a@(Mockingbird attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget a -> True) 1 windows _ -> do
      returnToHand a
      for_ [enemy | EnemyAttack enemy _ <- windows] preventAttackDamage
      pure a
    _ -> Mockingbird <$> runMessage msg attrs
