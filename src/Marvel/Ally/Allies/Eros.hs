module Marvel.Ally.Allies.Eros (eros, Eros (..)) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Payment ()
import Marvel.Query
import Marvel.Resource
import Marvel.Window qualified as W

newtype ErosPayment = ErosPayment {mentalPaid :: Natural}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype Eros = Eros (Attrs Ally `With` ErosPayment)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

eros :: AllyCard Eros
eros = ally (Eros . (`With` ErosPayment 0)) Cards.eros (Thw 2, 1) (Atk 1, 1) (HP 2)

instance IsAlly Eros where
  toAllyAttrs (Eros (attrs `With` _)) = attrs

instance HasAbilities Eros where
  getAbilities a@(Eros (attrs `With` meta)) =
    [ limitedWindowAbility a 1
        (W.PlayedFromHand (IdentityWithId $ allyController attrs) $ CardIs Cards.eros)
        Response (OwnsThis <> EnemyExists (MinionEnemy <> EnemyCanBeConfused)) NoCost (runAbility a 1)
    | mentalPaid meta > 0 ]

-- | Each mental resource makes a separate minion choice, after the previous confusion
-- has resolved (RR v1.8 Eros errata). A minion already confused is not a new target.
instance RunMessage Eros where
  runMessage msg a@(Eros (attrs `With` meta)) = runQueueT $ case msg of
    AllyMessage aid (AllyPaidWith payment) | aid == allyId attrs -> do
      n <- count (== Mental) <$> paymentResources payment
      attrs' <- runMessage msg attrs
      pure $ Eros (attrs' `With` ErosPayment n)
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      cardSpecific attrs "erosConfuse" (mentalPaid meta)
      pure a
    CardSpecific (isTarget attrs -> True) "erosConfuse" (fromJSON -> Success (remaining :: Natural)) -> do
      minions <- selectList (MinionEnemy <> EnemyCanBeConfused)
      when (remaining > 0 && not (null minions)) $ chooseOne (allyController attrs)
        [targeting (enemyRef minion) do
          confuse minion attrs
          cardSpecific attrs "erosConfuse" (remaining - 1)
        | minion <- minions]
      pure a
    _ -> Eros . (`With` meta) <$> runMessage msg attrs
