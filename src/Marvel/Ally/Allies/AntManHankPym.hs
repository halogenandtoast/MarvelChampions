module Marvel.Ally.Allies.AntManHankPym (
  antManHankPym,
  AntManHankPym (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Modifier
import Marvel.Payment
import Marvel.Ref

antManHankPym :: AllyCard AntManHankPym
antManHankPym = ally AntManHankPym Cards.antManHankPym (Thw 2, 1) (Atk 2, 1) (HP 0)

newtype AntManHankPym = AntManHankPym (Attrs Ally)
  deriving anyclass (IsAlly, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

-- | Ant-Man gets +1 hit point for each pym counter on him.
instance HasModifiersFor AntManHankPym where
  getModifiersFor _ (AllyRef aid) (AntManHankPym a)
    | aid == allyId a = pure [HitPointModifier (allyCounters a)]
  getModifiersFor _ _ _ = pure []

-- | Interrupt: When Ant-Man enters play, place 1 pym counter on him (to a maximum of 4)
-- for each resource you overpaid for Ant-Man's cost. His cost is 0, so every resource
-- paid is overpaid; with none he has no hit points and is defeated.
instance RunMessage AntManHankPym where
  runMessage msg (AntManHankPym attrs) = runQueueT $ case msg of
    AllyMessage aid (AllyPaidWith payment) | aid == allyId attrs -> do
      overpaid <- min 4 . fromIntegral . length <$> paymentResources payment
      when (overpaid == 0) $ defeatAlly aid
      AntManHankPym . (\a -> a {allyCounters = overpaid}) <$> runMessage msg attrs
    _ -> AntManHankPym <$> runMessage msg attrs
