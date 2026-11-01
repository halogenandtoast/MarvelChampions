module Marvel.SideScheme.SideSchemes.HookLineAndSinker where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost.Types
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.GameValue
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W

hookLineAndSinker :: SideSchemeCard HookLineAndSinker
hookLineAndSinker = sideScheme HookLineAndSinker Cards.hookLineAndSinker (PerPlayer 2)

newtype HookLineAndSinker = HookLineAndSinker (Attrs SideScheme)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsSideScheme HookLineAndSinker where
  sideSchemeAbilities = getAbilities

instance HasAbilities HookLineAndSinker where
  getAbilities a =
    [ windowAbility a 1 (W.EnemyAttackedAnyTarget W.When $ EnemyWithTrait Trait.Brute)
        ForcedInterrupt NoCost (runAbility a 1)
    , windowAbility a 2 (W.CharacterTookIndirectDamage $ CharacterControlledBy AnyIdentity)
        ForcedResponse NoCost (runAbility a 2)
    ]

instance RunMessage HookLineAndSinker where
  runMessage msg a@(HookLineAndSinker attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ windows $ \case
        W.EnemyAttack enemy _ -> makeAttackIndirect enemy
        W.EnemyAttackAlly enemy _ _ -> makeAttackIndirect enemy
        W.EnemyAttackOnEnemy enemy _ -> makeAttackIndirect enemy
        _ -> pure ()
      pure a
    RanAbility _ (isTarget attrs -> True) 2 windows _ -> do
      for_ windows $ \case
        W.IdentityTookDamage you _ _ -> exhaust you
        W.AllyTookDamage ally _ _ -> exhaust ally
        _ -> pure ()
      pure a
    _ -> HookLineAndSinker <$> runMessage msg attrs
