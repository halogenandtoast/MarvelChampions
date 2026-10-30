module Marvel.Ally.Allies.MoonGirlLunellaLafayette where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Card.Def
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Payment
import Marvel.Resource
import Marvel.Window qualified as W

newtype MoonGirlPayment = MoonGirlPayment {mentalPaid :: Natural}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype MoonGirlLunellaLafayette = MoonGirlLunellaLafayette (Attrs Ally `With` MoonGirlPayment)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

moonGirlLunellaLafayette :: AllyCard MoonGirlLunellaLafayette
moonGirlLunellaLafayette = ally (MoonGirlLunellaLafayette . (`With` MoonGirlPayment 0)) Cards.moonGirlLunellaLafayette (Thw 2, 2) (Atk 2, 2) (HP 3)

instance IsAlly MoonGirlLunellaLafayette where
  toAllyAttrs (MoonGirlLunellaLafayette (attrs `With` _)) = attrs

instance HasAbilities MoonGirlLunellaLafayette where
  getAbilities a@(MoonGirlLunellaLafayette (attrs `With` meta)) =
    [ limitedWindowAbility a 1
        (W.PlayedFromHand (IdentityWithId $ allyController attrs) $ CardIs Cards.moonGirlLunellaLafayette)
        Response OwnsThis NoCost (runAbility a 1)
    | mentalPaid meta > 0 ]

instance RunMessage MoonGirlLunellaLafayette where
  runMessage msg a@(MoonGirlLunellaLafayette (attrs `With` meta)) = runQueueT $ case msg of
    AllyMessage aid (AllyPaidWith payment) | aid == allyId attrs -> do
      n <- fromIntegral . count (== Mental) <$> paymentResources payment
      attrs' <- runMessage msg attrs
      pure $ MoonGirlLunellaLafayette (attrs' `With` MoonGirlPayment n)
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      drawCards you (mentalPaid meta)
      pure a
    _ -> MoonGirlLunellaLafayette . (`With` meta) <$> runMessage msg attrs
