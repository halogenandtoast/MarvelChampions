module Marvel.Resource.Resources (beforeSpent, afterSpent) where

import Marvel.Prelude
import Marvel.CivilWar.RewardResources (leaderRewardSpent,leaderRewardWhenSpent)

import Marvel.Card.Def (getCardDef, sameCard, cdCanPayForOthersWithTrait)
import Marvel.Matchers
import Marvel.Ref ()
import Marvel.Card.PlayerCard.Types (PlayerCard)
import Marvel.Id (IdentityId)
import Marvel.Message.Lifted
import Marvel.Resource.Cards qualified as Cards
import Marvel.Resource.Resources.PymParticles
import Marvel.Resource.Resources.Salvage
import Marvel.Resource.Resources.AdamWarlock

-- | Interrupts for resources actually spent, including contributors and held cards.
beforeSpent :: ReverseQueue m => IdentityId -> PlayerCard -> m ()
beforeSpent = leaderRewardWhenSpent

-- | What a resource card does after it is spent: "after you spend this card". The cards
-- spent with it are on their way to the discard pile.
afterSpent :: (ReverseQueue m) => IdentityId -> IdentityId -> [PlayerCard] -> PlayerCard -> m ()
afterSpent recipient you spent card
  | getCardDef card `sameCard` Cards.strokeOfGenius = chooseOne you
      [ labeled "$label.strokeOfGenius" (placeCounters you 1 >> drawCards you 1)
      , labeled "$label.noResponse" (pure ())
      ]
  | getCardDef card `sameCard` Cards.pymParticles = pymParticlesSpent you
  | getCardDef card `sameCard` Cards.salvage = salvageSpent you spent
  -- Adam Warlock
  | getCardDef card `sameCard` Cards.audacity = audacitySpent you
  | getCardDef card `sameCard` Cards.determination = determinationSpent you
  | getCardDef card `sameCard` Cards.innovation = innovationSpent you
  | getCardDef card `sameCard` Cards.preservation = preservationSpent you
  | Just trait <- cdCanPayForOthersWithTrait (getCardDef card) = do
      civilian <- identityMatches (IdentityWithTrait trait) you
      damaged <- identityMatches IdentityWithAnyDamage recipient
      when (civilian && damaged) $ chooseOne you
        [ labeled "$label.everydayHeroHeal" (heal recipient 1)
        , labeled "$label.noResponse" (pure ())
        ]
  | otherwise = leaderRewardSpent you card
