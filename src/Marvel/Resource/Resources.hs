module Marvel.Resource.Resources (afterSpent) where

import Marvel.Prelude

import Marvel.Card.Def (getCardDef, sameCard)
import Marvel.Card.PlayerCard.Types (PlayerCard)
import Marvel.Id (IdentityId)
import Marvel.Message.Lifted
import Marvel.Resource.Cards qualified as Cards
import Marvel.Resource.Resources.PymParticles
import Marvel.Resource.Resources.Salvage

-- | What a resource card does after it is spent: "after you spend this card". The cards
-- spent with it are on their way to the discard pile.
afterSpent :: (ReverseQueue m) => IdentityId -> [PlayerCard] -> PlayerCard -> m ()
afterSpent you spent card
  | getCardDef card `sameCard` Cards.pymParticles = pymParticlesSpent you
  | getCardDef card `sameCard` Cards.salvage = salvageSpent you spent
  | otherwise = pure ()
