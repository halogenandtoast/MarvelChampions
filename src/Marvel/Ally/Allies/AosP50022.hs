{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.Ally.Allies.AosP50022 where

import Marvel.Prelude
import Marvel.Question
import Marvel.Id
import Marvel.Projection
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Payment
import Marvel.Query
import Marvel.Resource.Types qualified as R
import Marvel.Trait qualified as T
import Marvel.Window qualified as W
import Marvel.AgentsOfShield.PlayerHelpers

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner

aosP50022 :: AllyCard AosP50022
aosP50022 = ally AosP50022 Cards.aosP50022 (Thw 2, 1) (Atk 2, 1) (HP 3)

newtype AosP50022 = AosP50022 (Attrs Ally)
  deriving anyclass (IsAlly)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities AosP50022 where
  getAbilities (AosP50022 attrs) =
    [limitedWindowAbility attrs 1 (W.TreacheryResolvedBy $ IdentityWithId $ allyController attrs) ForcedResponse OwnsThis NoCost (runAbility attrs 1)]

instance HasModifiersFor AosP50022 where
  getModifiersFor _ target (AosP50022 attrs) = pure [CannotDefend | target == toRef attrs]

instance RunMessage AosP50022 where
  runMessage msg card@(AosP50022 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      canPay <- canAffordCost you (toSource attrs) (ResourceCost $ Just R.Mental)
      chooseOne you $
        [ labeled "$label.aosGrantWardPay" (payCostThenWithPayment you attrs (ResourceCost $ Just R.Mental) attrs "grantWardPaid" ()) | canPay]
        <> [ labeled "$label.aosGrantWardDamage" do
            attack <- getModifiedAttack attrs
            damageIdentity you attrs (toDamage attack FromAbility)
            removeFromGame attrs]
      pure card
    _ -> AosP50022 <$> runMessage msg attrs
