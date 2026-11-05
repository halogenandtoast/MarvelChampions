module Marvel.Game.Runner where

import {-# SOURCE #-} Marvel.Card.Def (CardMatcher)
import Marvel.Card.PlayerCard.Types (PlayerCard)
import Marvel.Game.Class (HasGame)
import Marvel.Id (IdentityId)
import Marvel.Matchers.Types (ExtendedCardMatcher)
import Marvel.Prelude
import Marvel.Queue (HasQueue)
import Marvel.Ref (Source, Target)

gameSelectExtendedCard ::
  ( MonadThrow m
  , HasGame m
  , HasQueue m
  ) =>
  ExtendedCardMatcher ->
  m (HashSet PlayerCard)
playableClawsEvents :: (HasGame m, HasQueue m) => IdentityId -> Source -> m [PlayerCard]
playableCardsWithReductionAfterExhausting :: (HasGame m, HasQueue m) => IdentityId -> Target -> CardMatcher -> Natural -> m [PlayerCard]
