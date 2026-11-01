module Marvel.Obligation.Obligations.LostVisor where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue ()
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Stats ()
import Marvel.Window qualified as W ()
import Marvel.Deck
import Marvel.Discard
import Marvel.Hand
import Marvel.Identity.Types
import Marvel.Projection
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Obligation.Types
import Marvel.Upgrade.Cards qualified as Upgrades
import Marvel.Upgrade.Types qualified as Upgrade ()

lostVisor :: ObligationCard LostVisor
lostVisor = obligation LostVisor Cards.lostVisor

newtype LostVisor = LostVisor (Attrs Obligation)
  deriving anyclass IsObligation
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor LostVisor where
  getModifiersFor _ (IdentityRef you) (LostVisor attrs) = pure
    [CannotAttack | obligationOwner attrs == Just you]
  getModifiersFor _ _ _ = pure []

instance HasAbilities LostVisor where
  getAbilities a@(LostVisor attrs) =
    [ ability a 1 AlterEgoAction (SelfMatches $ IdentityWithId owner) ExhaustCost (runAbility a 1)
    | Just owner <- [obligationOwner attrs]
    ]

instance RunMessage LostVisor where
  runMessage msg e@(LostVisor attrs) = runQueueT $ case msg of
    ObligationMessage oid (RevealObligation you) | oid == obligationId attrs -> do
      hand <- projectMap PlayerIdentityHand unHand you
      deck <- projectMap PlayerIdentityDeck unDeck you
      discard <- projectMap PlayerIdentityDiscard unDiscard you
      upgrades <- selectList (UpgradeIs Upgrades.rubyQuartzVisor <> upgradeControlledBy you)
      inPlay <- for upgrades (\uid -> (uid,) <$> getsGame (\g -> case cardInPlay g (UpgradeRef uid) of Just (PlayerCard card) -> Just card; _ -> Nothing))
      case headMay [(Nothing, card) | card <- hand <> deck <> discard, toCardCode card == "33003"]
        <|> headMay [(Just uid, card) | (uid, Just card) <- inPlay] of
        Nothing -> pure ()
        Just (uid, card) -> do
          maybe (removeFromGame (CardIdRef $ pcCardId card)) removeFromGame uid
          cardSpecific attrs "holdVisor" card
      shuffleDeck you
      pure $ LostVisor attrs {obligationOwner = Just you}
    CardSpecific (isTarget attrs -> True) "holdVisor" (fromJSON -> Success card) ->
      pure $ LostVisor attrs {obligationHeldCards = [card]}
    ObligationMessage oid (ResolvedObligation _) | oid == obligationId attrs -> pure e
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      for_ (obligationHeldCards attrs) (addToHand you)
      cardSpecific attrs "clearVisor" ()
      removeFromGame attrs
      pure e
    CardSpecific (isTarget attrs -> True) "clearVisor" _ -> pure $ LostVisor attrs {obligationHeldCards = []}
    _ -> LostVisor <$> runMessage msg attrs
