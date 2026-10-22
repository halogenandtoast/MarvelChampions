module Marvel.Obligation.Obligations.FearOfKang (
  fearOfKang,
  FearOfKang (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Card.Def
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Obligation.Types
import Marvel.Question
import Marvel.Ref

fearOfKang :: ObligationCard FearOfKang
fearOfKang = obligation FearOfKang Cards.fearOfKang

newtype FearOfKang = FearOfKang (Attrs Obligation)
  deriving anyclass (IsObligation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | You cannot attack Kang: any character of that title.
instance HasModifiersFor FearOfKang where
  getModifiersFor _ (IdentityRef ident) (FearOfKang a) | obligationOwner a == Just ident = pure [CannotAttackTitled "Kang"]
  getModifiersFor _ _ _ = pure []

-- | Alter-Ego Action: discard a random card from your hand → discard this obligation.
instance HasAbilities FearOfKang where
  getAbilities (FearOfKang a) = case obligationOwner a of
    Just _ -> [ability a 1 AlterEgoAction (ExtendedCardExists $ InHandOf You $ BasicCardMatches AnyCard) NoCost $ runAbility a 1]
    Nothing -> []

-- | It stays in your play area once revealed.
instance RunMessage FearOfKang where
  runMessage msg o@(FearOfKang attrs) = runQueueT $ case msg of
    ObligationMessage ident (ResolvedObligation _) | ident == obligationId attrs -> pure o
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      discardFrom you RandomFromHand 1
      removeFromPlay attrs
      pure o
    _ -> FearOfKang <$> runMessage msg attrs
