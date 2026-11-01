module Marvel.Ally.Allies.HawkeyeKateBishop (
  hawkeyeKateBishop,
  HawkeyeKateBishop (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Card ()
import Marvel.Damage
import Marvel.Matchers
import Marvel.Payment ()

hawkeyeKateBishop :: AllyCard HawkeyeKateBishop
hawkeyeKateBishop = ally HawkeyeKateBishop Cards.hawkeyeKateBishop (Thw 1, 1) (Atk 2, 1) (HP 2)

newtype HawkeyeKateBishop = HawkeyeKateBishop (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

-- | Action: Exhaust this ally and discard 1 card from your hand → deal X damage to an
-- enemy, where X is the number of printed resources on that card.
instance HasAbilities HawkeyeKateBishop where
  getAbilities a =
    [ability a 1 Action (OwnsThis <> EnemyExists DamageableEnemy) (ExhaustCost <> DiscardHandCardsCost 1 (Just 1)) $ runAbility a 1]

instance RunMessage HawkeyeKateBishop where
  runMessage msg a@(HawkeyeKateBishop attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget a -> True) 1 _ payment -> do
      let x = fromIntegral . length $ concatMap (printedResources . getCardDef) (paymentDiscards payment)
      chooseDamage ident a (toDamage x FromAbility) AnyEnemy
      pure a
    _ -> HawkeyeKateBishop <$> runMessage msg attrs
