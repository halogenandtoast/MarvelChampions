module Marvel.Upgrade.Upgrades.Embiggen (
  embiggen,
  Embiggen (..),
  embiggenEffect,
  EmbiggenEffect,
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
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Trait qualified as Trait
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Window qualified as W

embiggen :: UpgradeCard Embiggen
embiggen = upgrade Embiggen Cards.embiggen

newtype Embiggen = Embiggen (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Embiggen where
  getAbilities a@(Embiggen attrs) =
    [ limitedWindowAbility
        a
        1
        (W.PlaysEvent W.When (IdentityWithId $ upgradeController attrs) (CardWithTrait Trait.Attack))
        HeroInterrupt
        OwnsThis
        ExhaustCost
        (runAbility a 1)
    ]

instance RunMessage Embiggen where
  runMessage msg a@(Embiggen attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget a -> True) 1 windows _ -> do
      for_ [eid | W.EventPlayed _ eid _ <- windows] $ createEffect Cards.embiggen a . EventEntity
      pure a
    _ -> Embiggen <$> runMessage msg attrs

-- | Each instance of damage the event deals is increased by 2, while it resolves.
newtype EmbiggenEffect = EmbiggenEffect (Attrs Effect)
  deriving anyclass (IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

embiggenEffect :: CardEffect EmbiggenEffect
embiggenEffect = effectWith EmbiggenEffect Cards.embiggen (modifiersL .~ [DamageDealtModifier 2])

instance RunMessage EmbiggenEffect where
  runMessage msg (EmbiggenEffect attrs) = EmbiggenEffect <$> runMessage msg attrs
