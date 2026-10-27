module Marvel.Support.Supports.Regroup (
  regroup,
  Regroup (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types
import Marvel.Window qualified as W

regroup :: SupportCard Regroup
regroup = support Regroup Cards.regroup

newtype Regroup = Regroup (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Interrupt: When an ally is defeated by an enemy attack, return it to its owner's hand
-- instead of discarding it. Forced Interrupt: When the round ends, discard this card.
instance HasAbilities Regroup where
  getAbilities a@(Regroup attrs) =
    [ limitedWindowAbility a 1 (W.AllyDefeatedByAttack W.When AnyAlly) Interrupt OwnsThis NoCost $
        runAbility a 1
    , windowAbility a 2 W.RoundEnds ForcedInterrupt NoCost $ DiscardTarget (toRef attrs)
    ]

instance RunMessage Regroup where
  runMessage msg s@(Regroup attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ [ally | W.DefeatedAlly ally (Just _) <- windows] \ally ->
        insteadOf (== RemoveFromPlay (toTarget ally)) (returnToHand ally)
      pure s
    _ -> Regroup <$> runMessage msg attrs
