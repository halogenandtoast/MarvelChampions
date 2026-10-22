module Marvel.Ally.Allies.SpiderGirlAnyaCorazon (
  spiderGirlAnyaCorazon,
  SpiderGirlAnyaCorazon (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Id
import Marvel.Matchers
import Marvel.Query
import Marvel.Window

spiderGirlAnyaCorazon :: AllyCard SpiderGirlAnyaCorazon
spiderGirlAnyaCorazon = ally SpiderGirlAnyaCorazon Cards.spiderGirlAnyaCorazon (Thw 1, 1) (Atk 2, 1) (HP 2)

newtype SpiderGirlAnyaCorazon = SpiderGirlAnyaCorazon (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

-- | Response: After you play Spider-Girl from your hand, stun and confuse a minion.
instance HasAbilities SpiderGirlAnyaCorazon where
  getAbilities a =
    [limitedWindowAbility a 1 (PlayThis After) Response (OwnsThis <> MinionExists AnyMinion) NoCost $ runAbility a 1]

instance RunMessage SpiderGirlAnyaCorazon where
  runMessage msg a@(SpiderGirlAnyaCorazon attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget a -> True) 1 _ _ -> do
      minions <- selectList AnyMinion
      chooseOne ident
        [ targeting (MinionRef m) do
            stun (EnemyMinionId m) a
            confuse (EnemyMinionId m) a
        | m <- minions
        ]
      pure a
    _ -> SpiderGirlAnyaCorazon <$> runMessage msg attrs
