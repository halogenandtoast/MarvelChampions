module Marvel.Ally.Allies.GoliathBillFoster (
  goliathBillFoster,
  GoliathBillFoster (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Modifier
import Marvel.Ref

goliathBillFoster :: AllyCard GoliathBillFoster
goliathBillFoster = ally (GoliathBillFoster . (`With` Meta False)) Cards.goliathBillFoster (Thw 2, 1) (Atk 1, 2) (HP 4)

-- | Whether he has grown this phase.
newtype Meta = Meta {grown :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype GoliathBillFoster = GoliathBillFoster (Attrs Ally `With` Meta)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance IsAlly GoliathBillFoster where
  toAllyAttrs (GoliathBillFoster (a `With` _)) = a

instance IsRef GoliathBillFoster where
  toRef = toRef . toAllyAttrs

-- | Action: Goliath gets +4 ATK until the end of the phase. At the end of the phase,
-- discard Goliath. (Max once per phase.)
instance HasAbilities GoliathBillFoster where
  getAbilities a = [limitedAbility a 1 (PerPhase 1) Action OwnsThis NoCost $ runAbility a 1]

instance HasModifiersFor GoliathBillFoster where
  getModifiersFor _ (AllyRef aid) (GoliathBillFoster (a `With` meta))
    | aid == allyId a && grown meta = pure [AttackModifier 4]
  getModifiersFor _ _ _ = pure []

instance RunMessage GoliathBillFoster where
  runMessage msg g@(GoliathBillFoster (attrs `With` meta)) = runQueueT $ case msg of
    RanAbility _ (isTarget g -> True) 1 _ _ -> pure . GoliathBillFoster $ attrs `With` Meta True
    EndPhase _ | grown meta -> do
      removeFromPlay attrs
      pure . GoliathBillFoster $ attrs `With` Meta False
    _ -> GoliathBillFoster . (`With` meta) <$> runMessage msg attrs
