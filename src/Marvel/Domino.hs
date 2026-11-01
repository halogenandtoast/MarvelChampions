module Marvel.Domino where
import Marvel.Prelude
import Marvel.Card
import Marvel.Game.Source
import Marvel.Deck
import Marvel.Discard
import Marvel.Game.Source
import {-# SOURCE #-} Marvel.Game (isBlanked)
import Marvel.Hero.Cards qualified as Heroes
import Marvel.Identity.Types
import Marvel.Matchers
import Marvel.Modifier
import Marvel.Projection
import Marvel.Query
import Marvel.Resource.Types
import Marvel.Id

-- Printed icons only. Wild icons remain wild when duplicated: Luck Be a Lady
-- resolves a separate choice for each, instead of generating payment resources.
discardedResources :: HasGame m => IdentityId -> [Card] -> m [Resource]
discardedResources you cards = do
  domino <- identityMatches (HeroIdentity <> IdentityIs Heroes.dominoHero) you
  blank <- isBlanked you
  pure $ concatMap (\r -> if domino && not blank && r == Wild then [Wild, Wild] else [r])
    (concatMap (printedResources . getCardDef) cards)

discardedResourceCount :: HasGame m => IdentityId -> [Card] -> m Natural
discardedResourceCount you cards = fromIntegral . length <$> discardedResources you cards

-- Exact original card identity, across an immediate empty-deck reshuffle. A
-- response cannot take the card if another response has already moved it.
cardInDeckOrDiscard :: HasGame m => IdentityId -> PlayerCard -> m Bool
cardInDeckOrDiscard you card = do
  deck <- projectMap PlayerIdentityDeck unDeck you
  discard <- projectMap PlayerIdentityDiscard unDiscard you
  pure $ card `elem` deck || card `elem` discard
