module Marvel.Ally.Allies.Wong (
  wong,
  Wong (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Invocation

wong :: AllyCard Wong
wong = ally Wong Cards.wong (Thw 1, 1) (Atk 2, 1) (HP 3)

newtype Wong = Wong (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance HasAbilities Wong where
  getAbilities a = [ability a 1 Action OwnsThis ExhaustCost (runAbility a 1)]

instance RunMessage Wong where
  runMessage msg w@(Wong attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      canHeal <- identityMatches healableIdentity you
      chooseOne you $
        [labeled (countLabel "healYourIdentity" 1) (heal you 1) | canHeal]
        <> [labeled "$label.discardTopInvocation" (invoke you DiscardTopInvocation)]
      pure w
    _ -> Wong <$> runMessage msg attrs
