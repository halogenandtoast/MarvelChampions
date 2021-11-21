module Marvel.Card.PlayerCard where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Card.Id
import Marvel.Id

data PlayerCard = PlayerCard
  { pcCardId :: CardId
  , pcCardDef :: CardDef
  , pcOwner :: Maybe IdentityId
  , pcController :: Maybe IdentityId
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

instance HasCardCode PlayerCard where
  toCardCode = toCardCode . getCardDef

instance HasCardDef PlayerCard where
  getCardDef = pcCardDef

instance HasResources PlayerCard where
  resourcesFor x mc = do
    guard $ Just x /= mc
    map snd $ filter isValidResource $ cdResources $ getCardDef x
   where
    isValidResource (restriction, _) = case restriction of
      PrintedResource -> True
      ResourceForCardsMatching matcher -> maybe False (cardMatch matcher) mc
