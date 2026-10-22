module Marvel.Upgrade.Upgrades.Shrink (
  shrink,
  Shrink (..),
  shrinkEffect,
  ShrinkEffect,
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Cost
import Marvel.Criteria
import Marvel.Effect.Types
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Queue
import Marvel.Ref
import Marvel.Trait qualified as Trait
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Window qualified as W

shrink :: UpgradeCard Shrink
shrink = upgrade Shrink Cards.shrink

newtype Shrink = Shrink (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Shrink where
  getAbilities a@(Shrink attrs) =
    [ limitedWindowAbility
        a
        1
        (W.PlaysEvent W.When (IdentityWithId $ upgradeController attrs) (CardWithTrait Trait.Thwart))
        HeroInterrupt
        OwnsThis
        ExhaustCost
        (runAbility a 1)
    ]

instance RunMessage Shrink where
  runMessage msg a@(Shrink attrs) = case msg of
    RanAbility _ (isTarget a -> True) 1 windows _ -> do
      pushAll
        [ CreatedEffect Cards.shrink (toSource a) (EventEntity eid)
        | W.EventPlayed _ eid _ <- windows
        ]
      pure a
    _ -> Shrink <$> runMessage msg attrs

-- | Each instance of threat the event removes is increased by 2, while it resolves.
newtype ShrinkEffect = ShrinkEffect (Attrs Effect)
  deriving anyclass (IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

shrinkEffect :: CardEffect ShrinkEffect
shrinkEffect = effectWith ShrinkEffect Cards.shrink (modifiersL .~ [ThreatRemovedModifier 2])

instance RunMessage ShrinkEffect where
  runMessage msg (ShrinkEffect attrs) = ShrinkEffect <$> runMessage msg attrs
