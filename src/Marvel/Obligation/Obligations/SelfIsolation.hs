module Marvel.Obligation.Obligations.SelfIsolation where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Window qualified as W
import Marvel.Deck
import Marvel.Discard
import Marvel.Hand
import Marvel.Identity.Types (Field (PlayerIdentityHand, PlayerIdentityDeck, PlayerIdentityDiscard))
import Marvel.Projection
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Obligation.Types

selfIsolation :: ObligationCard SelfIsolation
selfIsolation = obligation SelfIsolation Cards.selfIsolation

newtype SelfIsolation = SelfIsolation (Attrs Obligation)
  deriving anyclass (IsObligation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities SelfIsolation where
  getAbilities a@(SelfIsolation attrs) =
    [ limitedWindowAbility a 1 (W.UsesRecoveryAt W.After $ IdentityWithId you) Response (SelfMatches $ IdentityWithId you) NoCost (runAbility a 1)
    | Just you <- [obligationOwner attrs]
    ]

instance RunMessage SelfIsolation where
  runMessage msg o@(SelfIsolation attrs) = runQueueT $ case msg of
    ObligationMessage oid (RevealObligation you) | oid == obligationId attrs -> do
      hand <- projectMap PlayerIdentityHand unHand you
      deck <- projectMap PlayerIdentityDeck unDeck you
      discarded <- projectMap PlayerIdentityDiscard unDiscard you
      allies <- selectList (AllyWithTitle "Honey Badger" <> AllyControlledBy (IdentityWithId you))
      inPlay <- for allies \aid -> (aid,) <$> getsGame (\g -> case cardInPlay g (AllyRef aid) of Just (PlayerCard card) -> Just card; _ -> Nothing)
      case headMay [(Nothing, card) | card <- hand <> deck <> discarded, toCardCode card == "43003"]
        <|> headMay [(Just aid, card) | (aid, Just card) <- inPlay] of
        Nothing -> do
          removeFromPlay attrs
          dealEncounterCard you
        Just (aid, card) -> do
          maybe (removeFromGame $ CardIdRef $ pcCardId card) removeFromGame aid
          cardSpecific attrs "holdHoneyBadger" card
      shuffleDeck you
      pure $ SelfIsolation attrs {obligationOwner = Just you}
    CardSpecific (isTarget attrs -> True) "holdHoneyBadger" (fromJSON -> Success card) ->
      pure $ SelfIsolation attrs {obligationHeldCards = [card]}
    ObligationMessage oid (ResolvedObligation _) | oid == obligationId attrs -> pure o
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> o <$ removeFromPlay attrs
    _ -> SelfIsolation <$> runMessage msg attrs
