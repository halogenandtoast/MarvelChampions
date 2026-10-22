module Marvel.Obligation.Obligations.Depowered (
  depowered,
  Depowered (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Obligation.Types
import Marvel.Ref

depowered :: ObligationCard Depowered
depowered = obligation Depowered Cards.depowered

newtype Depowered = Depowered (Attrs Obligation)
  deriving anyclass (IsObligation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | You cannot play hero-specific cards. An Invocation card's ability is resolved, not
-- played (RR FAQ, Depowered #20), so it is not stopped.
instance HasModifiersFor Depowered where
  getModifiersFor _ (IdentityRef ident) (Depowered a) | obligationOwner a == Just ident = pure [CannotPlayCards HeroSpecificCard]
  getModifiersFor _ _ _ = pure []

-- | Alter-Ego Action: discard a hero-specific card from your hand → discard this
-- obligation.
instance HasAbilities Depowered where
  getAbilities (Depowered a) = discardToDiscardAbility a HeroSpecificCard

-- | It stays in your play area once revealed.
instance RunMessage Depowered where
  runMessage msg o@(Depowered attrs) = runQueueT $ case msg of
    ObligationMessage ident (ResolvedObligation _) | ident == obligationId attrs -> pure o
    RanAbility you (isTarget attrs -> True) 1 _ _ -> o <$ discardToDiscard attrs you HeroSpecificCard
    _ -> Depowered <$> runMessage msg attrs
