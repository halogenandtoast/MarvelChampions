module Marvel.Resource.Resources.Domino (discardResponseAbilities, runDiscardResponse) where
import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Domino
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.Window qualified as W

-- All options live in the SAME response window as The Painted Lady. The exact
-- id and current zone stop an already claimed card from being claimed twice.
discardResponseAbilities :: IdentityId -> [PlayerCard] -> [Ability]
discardResponseAbilities you cards =
  [ label responseLabel $ limitedWindowAbility (CardIdRef $ pcCardId card) 1
      (W.CardsDiscardedFromPlayerDeck $ IdentityWithId you) Response
      (SelfMatches (IdentityWithId you)
        <> (if toCardCode card == "40057" then ExtendedCardExists (CardWithId (pcCardId card) <> CanEnterPlay) else NoCriteria)
        <> AnyCriteria
        [ExtendedCardExists (InDeckOf (IdentityWithId you) $ CardWithId $ pcCardId card)
        ,ExtendedCardExists (InDiscardOf (IdentityWithId you) $ CardWithId $ pcCardId card)])
      NoCost (runAbility (CardIdRef $ pcCardId card) 1)
  | card <- cards
  , let code = toCardCode card
  , (eligibleCode,responseLabel) <- [("40043","$label.dominoJackpot"),("40060","$label.dominoDiggingDeep"),("40057","$label.dominoWhiteFox")]
  , code == eligibleCode
  ]

runDiscardResponse :: ReverseQueue m => IdentityId -> PlayerCard -> m ()
runDiscardResponse you card = whenM (cardInDeckOrDiscard you card) $ case toCardCode card of
  "40043" -> shuffleIntoDeck you [card]
  "40060" -> addToHand you card
  "40057" -> putIntoPlay you card
  _ -> pure ()
