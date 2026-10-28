module Marvel.Obligation.Obligations.LossOfControl (
  lossOfControl,
  LossOfControl (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Obligation.Types
import Marvel.Question
import Marvel.Ref

lossOfControl :: ObligationCard LossOfControl
lossOfControl = obligation LossOfControl Cards.lossOfControl

newtype LossOfControl = LossOfControl (Attrs Obligation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
  deriving anyclass (IsObligation)

-- | You cannot change energy forms.
instance HasModifiersFor LossOfControl where
  getModifiersFor _ (IdentityRef ident) (LossOfControl a)
    | obligationOwner a == Just ident = pure [CannotChangeEnergyForm]
  getModifiersFor _ _ _ = pure []

-- | Alter-Ego Action: Exhaust Monica Rambeau → remove Loss of Control from the game.
instance HasAbilities LossOfControl where
  getAbilities (LossOfControl a) = case obligationOwner a of
    Just _ -> [ability a 1 AlterEgoAction NoCriteria ExhaustCost (runAbility a 1)]
    Nothing -> []

-- | Given to the Monica Rambeau player, it stays in their play area.
instance RunMessage LossOfControl where
  runMessage msg o@(LossOfControl attrs) = runQueueT $ case msg of
    ObligationMessage ident (ResolvedObligation _) | ident == obligationId attrs -> pure o
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      removeFromGame attrs
      pure o
    _ -> LossOfControl <$> runMessage msg attrs
