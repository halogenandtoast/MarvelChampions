module Marvel.AlterEgo.AlterEgos.SamAlexander where

import Marvel.Prelude
import Marvel.Ability
import Marvel.AlterEgo.Cards qualified as Cards
import Marvel.AlterEgo.Runner
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Card.Types
import Marvel.Cost
import Marvel.Criteria
import Marvel.Deck
import Marvel.Discard
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Identity.Types (Field (PlayerIdentityDeck, PlayerIdentityDiscard))
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.Payment
import Marvel.Projection
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.Resource
import Marvel.Stats
import Marvel.Upgrade.Cards qualified as Upgrades

samAlexander :: AlterEgoCard SamAlexander
samAlexander = alterEgo SamAlexander Cards.samAlexander (HP $ Static 10) (HandSize 6) (Rec 3) [Obligations.weightOfTheWorld]

newtype SamAlexander = SamAlexander (Attrs AlterEgo)
  deriving anyclass (IsAlterEgo, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities SamAlexander where
  getAbilities a = [ability a 1 AlterEgoAction IsSelf (ResourceCost Nothing) (runAbility a 1)]

instance RunMessage SamAlexander where
  runMessage msg ae@(SamAlexander attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ payment -> do
      wild <- paidWith Wild payment
      if not wild
        then search you (SearchIdentityDeckAndDiscardPile you) (CardIs Upgrades.supernovaHelmet) SearchDrawOne ShuffleBackIn
        else do
          deck <- projectMap PlayerIdentityDeck unDeck you
          discard <- projectMap PlayerIdentityDiscard unDiscard you
          let found = filter (cardMatch $ CardIs Upgrades.supernovaHelmet) (deck <> discard)
          if null found
            then chooseOne you [labeled "$label.noMatchingCardsFound" (pure ())]
            else chooseOrRunOneSearched you (map PlayerCard found) [targeting c (putIntoPlay you c) | c <- found]
          shuffleDeck you
      pure ae
    _ -> SamAlexander <$> runMessage msg attrs
