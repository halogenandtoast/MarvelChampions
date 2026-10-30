module Marvel.Ally.Allies.MonicaChang where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Card
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Deck
import Marvel.Discard
import Marvel.Hand
import Marvel.Identity.Types (Field (PlayerIdentityDeck, PlayerIdentityDiscard, PlayerIdentityHand))
import Marvel.Matchers
import Marvel.Projection
import Marvel.Query
import Marvel.Question (runAbility)
import Marvel.Support.Cards qualified as Supports
import Marvel.Window qualified as W

monicaChang :: AllyCard MonicaChang
monicaChang = ally MonicaChang Cards.monicaChang (Thw 1, 1) (Atk 1, 1) (HP 2)

newtype MonicaChang = MonicaChang (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities MonicaChang where
  getAbilities a = [limitedWindowAbility a 1 W.EntersPlayThis Response OwnsThis NoCost (runAbility a 1)]

instance RunMessage MonicaChang where
  runMessage msg a@(MonicaChang attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      deck <- projectMap PlayerIdentityDeck unDeck you
      hand <- projectMap PlayerIdentityHand unHand you
      discard <- projectMap PlayerIdentityDiscard unDiscard you
      let found = filter (cardMatch $ CardIs Supports.surveillanceTeam) (deck <> hand <> discard)
      unless (null found) $ focusing (map PlayerCard found) $
        chooseOne you [targeting card (putIntoPlay you card) | card <- found]
      shuffleDeck you
      cardSpecific attrs "addSnoopCounters" you
      pure a
    CardSpecific (isTarget attrs -> True) "addSnoopCounters" (fromJSON -> Success you) -> do
      teams <- selectList (SupportMatches [SupportControlledBy (IdentityWithId you), SupportNamed "Surveillance Team"])
      for_ teams (`addSupportUses` 1)
      pure a
    _ -> MonicaChang <$> runMessage msg attrs
