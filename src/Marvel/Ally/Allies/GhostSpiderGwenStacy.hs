module Marvel.Ally.Allies.GhostSpiderGwenStacy where

import Marvel.Prelude
import Marvel.Matchers
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Card ()
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Deck
import Marvel.Identity.Types (Field (PlayerIdentityDeck))
import Marvel.Projection
import Marvel.Query ()
import Marvel.Question ()
import Marvel.Window qualified as W

ghostSpiderGwenStacy :: AllyCard GhostSpiderGwenStacy
ghostSpiderGwenStacy = ally GhostSpiderGwenStacy Cards.ghostSpiderGwenStacy (Thw 1, 1) (Atk 2, 1) (HP 3)

newtype GhostSpiderGwenStacy = GhostSpiderGwenStacy (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities GhostSpiderGwenStacy where
  getAbilities a@(GhostSpiderGwenStacy attrs) =
    [ limitedWindowAbility a 1 (W.AllyLeavesPlay W.When (IdentityWithId $ allyController attrs) (CardIs Cards.ghostSpiderGwenStacy))
        Interrupt OwnsThis NoCost (runAbility a 1) ]

instance RunMessage GhostSpiderGwenStacy where
  runMessage msg a@(GhostSpiderGwenStacy attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      deck <- projectMap PlayerIdentityDeck unDeck you
      let found = filter (cardMatch $ HeroSpecificCard <> CardWithType EventType) deck
      unless (null found) $ focusing (map PlayerCard found) $ chooseOne you [targeting card (addToHand you card) | card <- found]
      shuffleDeck you
      pure a
    _ -> GhostSpiderGwenStacy <$> runMessage msg attrs
