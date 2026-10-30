module Marvel.Obligation.Obligations.WorriedFather where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Deck
import Marvel.Discard
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Hand
import Marvel.Identity.Types (Field (PlayerIdentityHand, PlayerIdentityDeck, PlayerIdentityDiscard))
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Obligation.Types
import Marvel.Projection
import Marvel.Query
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.Support.Cards qualified as Supports

worriedFather :: ObligationCard WorriedFather
worriedFather = obligation WorriedFather Cards.worriedFather

newtype WorriedFather = WorriedFather (Attrs Obligation)
  deriving anyclass (IsObligation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities WorriedFather where
  getAbilities a@(WorriedFather attrs) =
    [ ability a 1 AlterEgoAction NoCriteria
        (ExhaustCost <> RemoveFromGameCost (toRef attrs)) (runAbility a 1)
    | isJust (obligationOwner attrs)
    ]

instance RunMessage WorriedFather where
  runMessage msg o@(WorriedFather attrs) = runQueueT $ case msg of
    ObligationMessage oid (RevealObligation you) | oid == obligationId attrs -> do
      attrs' <- runMessage msg attrs
      georges <- selectList (SupportMatches [SupportNamed "George Stacy", SupportControlledBy $ IdentityWithId you])
      hand <- projectMap PlayerIdentityHand unHand you
      deck <- projectMap PlayerIdentityDeck unDeck you
      discarded <- projectMap PlayerIdentityDiscard unDiscard you
      let cards = filter (cardMatch $ CardIs Supports.georgeStacy) (hand <> deck <> discarded)
      held <- case georges of
        sid : _ -> do
          card <- getsGame (\g -> cardInPlay g $ SupportRef sid)
          removeFromGame sid
          pure [c | Just (PlayerCard c) <- [card]]
        [] -> case cards of
          c : _ -> removeFromGame c >> pure [c]
          [] -> pure []
      shuffleDeck you
      pure $ WorriedFather $ attrs' & heldCardsL <>~ held
    -- The obligation stays in the player's play area, with George attached facedown.
    ObligationMessage oid (ResolvedObligation _) | oid == obligationId attrs -> pure o
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      traverse_ (addToHand you) (obligationHeldCards attrs)
      pure $ WorriedFather $ attrs & heldCardsL .~ []
    _ -> WorriedFather <$> runMessage msg attrs
