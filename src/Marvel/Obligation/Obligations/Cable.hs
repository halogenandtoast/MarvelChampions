module Marvel.Obligation.Obligations.Cable where

import Marvel.Hero.Cards qualified as HeroCards
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Prelude
import Data.List ()
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Status ()
import Marvel.Game.Source
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Payment ()
import Marvel.Query
import Marvel.Resource ()
import Marvel.Trait qualified as Trait ()
import Marvel.Window qualified as W ()
import Marvel.Keyword qualified as Keyword ()
import Marvel.Identity.Types
import Marvel.Projection
import Marvel.Deck
import Marvel.Discard
import Marvel.SideScheme.Types ()

import Marvel.Ability
import Marvel.Modifier
import Marvel.Entity
import Marvel.Card
import Marvel.Ref
import Marvel.Message.Lifted
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Obligation.Types
import Marvel.Stats ()
import Marvel.GameValue ()
import Marvel.Hand

cable40031 :: ObligationCard Cable40031
cable40031 = obligation Cable40031 Cards.cable40031

newtype Cable40031 = Cable40031 (Attrs Obligation)
  deriving anyclass (IsObligation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Cable40031 where
  runMessage msg a@(Cable40031 attrs) = runQueueT $ case msg of
    ObligationMessage oid (RevealObligation revealedFor) | oid == obligationId attrs -> do
      owner <- selectOne (IdentityIs HeroCards.cable40001a)
      let you = fromMaybe revealedFor owner
      deck <- projectMap PlayerIdentityDeck unDeck you
      discard <- projectMap PlayerIdentityDiscard unDiscard you
      hand <- projectMap PlayerIdentityHand unHand you
      victory <- getPlayerVictoryDisplay
      let cards = filter (\c -> toCardCode c == "40006" && pcOwner c == Just you) (deck <> discard <> hand <> victory)
      case headMay cards of
        Nothing -> cardSpecific attrs "attachPurge" you
        Just card -> do
          cardSpecific GameRef "removePlayerVictoryCard" (toCardId card)
          putIntoPlay you card
          cardSpecific attrs "attachPurge" you
      shuffleDeck you
      pure a
    ObligationMessage oid (ResolvedObligation _) | oid == obligationId attrs, isJust (obligationSideScheme attrs) -> pure a
    CardSpecific (isTarget attrs -> True) "attachPurge" (fromJSON -> Success you) -> do
      matching <- selectList (SideSchemeIs Schemes.cable40006)
      case headMay matching of
        Nothing -> removeFromPlay attrs >> dealEncounterCard you
        Just sid -> attachObligationToSideScheme (obligationId attrs) sid
      pure a
    _ -> Cable40031 <$> runMessage msg attrs
