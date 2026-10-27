module Marvel.Upgrade.Upgrades.ReactorCore (
  reactorCore,
  reactorCoreEffect,
  ReactorCore (..),
  ReactorCoreEffect (..),
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
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

reactorCore :: UpgradeCard ReactorCore
reactorCore = upgrade ReactorCore Cards.reactorCore

newtype ReactorCore = ReactorCore (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Action: Exhaust Reactor Core and discard the top 2 cards of your deck (the top
-- card instead if you control the Milano) → reduce the resource cost of the next event
-- you play this turn by 1. The discard is the cost, so whether you control the Milano is
-- decided as it is paid. A future target: no event need be in hand (RR Target).
instance HasAbilities ReactorCore where
  getAbilities a =
    [ ability
        a
        1
        HeroAction
        OwnsThis
        (ExhaustCost <> IfControlsMilanoCost DiscardTopOfDeckCost (DiscardTopOfDeckCost <> DiscardTopOfDeckCost))
        (runAbility a 1)
    ]

instance RunMessage ReactorCore where
  runMessage msg u@(ReactorCore attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      createEffect Cards.reactorCore attrs (IdentityEntity $ IdentityWithId you)
      pure u
    _ -> ReactorCore <$> runMessage msg attrs

event :: CardMatcher
event = CardWithType EventType

-- | The next event the player plays this turn costs 1 less.
newtype ReactorCoreEffect = ReactorCoreEffect (Attrs Effect)
  deriving anyclass (IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

reactorCoreEffect :: CardEffect ReactorCoreEffect
reactorCoreEffect =
  effectWith ReactorCoreEffect Cards.reactorCore $
    (modifiersL .~ [ResourceCostReductionFor event 1])
      . (endsL ?~ DisableAtEndOfTurn)

instance RunMessage ReactorCoreEffect where
  runMessage msg e@(ReactorCoreEffect attrs) = runQueueT $ case msg of
    IdentityMessage ident (PlayedCard card _) | cardMatch event card -> do
      whenM (effectValidFor attrs (toRef ident)) $ disableEffect (effectId attrs)
      pure e
    _ -> ReactorCoreEffect <$> runMessage msg attrs
