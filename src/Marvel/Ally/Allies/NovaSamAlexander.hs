module Marvel.Ally.Allies.NovaSamAlexander (
  novaSamAlexander,
  NovaSamAlexander (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Choice
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Matchers hiding (ExhaustedAlly)
import Marvel.Resource
import Marvel.Window qualified as W

novaSamAlexander :: AllyCard NovaSamAlexander
novaSamAlexander =
  ally NovaSamAlexander Cards.novaSamAlexander (Thw 1, 1) (Atk 2, 1) (HP 3)

newtype NovaSamAlexander = NovaSamAlexander (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance HasAbilities NovaSamAlexander where
  getAbilities a@(NovaSamAlexander attrs) =
    [ limitedWindowAbility
        a
        1
        (W.EnemyWouldAttack AnyEnemy $ IdentityWithId $ allyController attrs)
        Interrupt
        OwnsThis
        (ResourceCost $ Just Energy)
        (runAbility a 1)
    ]

instance RunMessage NovaSamAlexander where
  runMessage msg a@(NovaSamAlexander attrs) = case msg of
    RanAbility ident (isTarget a -> True) 1 windows _ -> do
      for_ [enemy | W.EnemyAttack enemy _ <- windows] \enemy ->
        pushChoice ident $ DamageEnemy (toRef enemy) (toSource a) (toDamage 2 FromAbility)
      pure a
    _ -> NovaSamAlexander <$> runMessage msg attrs
