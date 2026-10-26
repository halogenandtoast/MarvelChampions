module Marvel.Resource.Resources (afterSpent) where

import Marvel.Prelude

import Marvel.Card.Def (getCardDef, sameCard)
import Marvel.Card.PlayerCard.Types (PlayerCard)
import Marvel.Id (IdentityId)
import Marvel.Message.Lifted
import Marvel.Resource.Cards qualified as Cards
import Marvel.Resource.Resources.PymParticles

-- | What a resource card does after it is spent: "after you spend this card".
afterSpent :: (ReverseQueue m) => IdentityId -> PlayerCard -> m ()
afterSpent you card
  | getCardDef card `sameCard` Cards.pymParticles = pymParticlesSpent you
  | otherwise = pure ()
