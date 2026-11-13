module Marvel.Ally.Allies.AgentCoulson (
  agentCoulson,
  AgentCoulson (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Trait
import Marvel.Window

agentCoulson :: AllyCard AgentCoulson
agentCoulson = ally AgentCoulson Cards.agentCoulson (Thw 2, 1) (Atk 1, 1) (HP 3)

newtype AgentCoulson = AgentCoulson (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance HasAbilities AgentCoulson where
  getAbilities a =
    [ limitedWindowAbility a 1 EntersPlayThis Response OwnsThis NoCost $
        runAbility a 1
    ]

-- | Search the deck and the discard pile for a Preparation card, take it, and shuffle
-- the deck.
instance RunMessage AgentCoulson where
  runMessage msg a@(AgentCoulson attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget a -> True) 1 _ _ -> do
      search ident (SearchIdentityDeckAndDiscardPile ident) (CardWithTrait Preparation) SearchDrawOne ShuffleBackIn
      pure a
    _ -> AgentCoulson <$> runMessage msg attrs
