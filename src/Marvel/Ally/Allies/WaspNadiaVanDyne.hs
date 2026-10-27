module Marvel.Ally.Allies.WaspNadiaVanDyne (
  waspNadiaVanDyne,
  WaspNadiaVanDyne (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Matchers
import Marvel.Trait
import Marvel.Window

waspNadiaVanDyne :: AllyCard WaspNadiaVanDyne
waspNadiaVanDyne = ally WaspNadiaVanDyne Cards.waspNadiaVanDyne (Thw 2, 1) (Atk 2, 1) (HP 3)

newtype WaspNadiaVanDyne = WaspNadiaVanDyne (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

-- | Hero Response: After Wasp enters play, deal 2 damage to an enemy if you are in Giant
-- hero form or remove 2 threat from a scheme if you are in Tiny hero form.
instance HasAbilities WaspNadiaVanDyne where
  getAbilities a =
    [ limitedWindowAbility a 1 EntersPlayThis HeroResponse (OwnsThis <> AnyCriteria [giant, tiny]) NoCost $
        runAbility a 1
    ]
   where
    giant = SelfMatches (IdentityWithTrait Giant) <> EnemyExists AnyEnemy
    tiny = SelfMatches (IdentityWithTrait Tiny) <> SchemeExists ThwartableScheme

instance RunMessage WaspNadiaVanDyne where
  runMessage msg a@(WaspNadiaVanDyne attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget a -> True) 1 _ _ -> do
      giant <- identityMatches (IdentityWithTrait Giant) ident
      if giant
        then chooseDamage ident a (toDamage 2 FromAbility) AnyEnemy
        else removeThreat ident a 2 ThwartableScheme
      pure a
    _ -> WaspNadiaVanDyne <$> runMessage msg attrs
