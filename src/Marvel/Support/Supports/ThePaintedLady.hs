module Marvel.Support.Supports.ThePaintedLady where
import Marvel.Prelude
import Marvel.Card
import Marvel.Game.Source
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Domino
import Marvel.Deck
import Marvel.Discard
import Marvel.Hand
import Marvel.Id
import Marvel.Identity.Types
import Marvel.Matchers
import Marvel.Payment
import Marvel.Projection
import Marvel.Query
import Marvel.Question
import Marvel.Resource.Types
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.GameValue
import Marvel.Stats
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types

thePaintedLady :: SupportCard ThePaintedLady
thePaintedLady = support ThePaintedLady Cards.thePaintedLady
newtype ThePaintedLady = ThePaintedLady (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities ThePaintedLady where
  getAbilities a@(ThePaintedLady attrs) =
    [limitedWindowAbility a 1 (W.CardsDiscardedBy $ IdentityWithId $ supportController attrs)
      Response OwnsThis NoCost (runAbility a 1) | length (supportHeldCards attrs) < 3]
    <> [ability a 2 AlterEgoAction OwnsThis ExhaustCost (runAbility a 2) | notNull $ supportHeldCards attrs]

instance RunMessage ThePaintedLady where
  runMessage msg a@(ThePaintedLady attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows _ -> do
      let cards = [card | W.PlayerDiscardedFromDeck actor _ card <- windows, actor == you]
               <> [encounterCardInPlayerDeck you card | W.PlayerDiscardedFromEncounterDeck actor card <- windows, actor == you]
      available <- filterM (paintedCardAvailable you) cards
      chooseOne you [targeting card (cardSpecific attrs "hold" card) | card <- available]
      pure a
    CardSpecific (isTarget attrs -> True) "hold" (fromJSON -> Success card) -> do
      present <- paintedCardAvailable (supportController attrs) card
      if present && length (supportHeldCards attrs) < 3
        then do
          case pcOwner card of
            Nothing -> removeFromGame (CardIdRef $ pcCardId card)
            Just _ -> removeFromGame card
          pure $ ThePaintedLady attrs {supportHeldCards = supportHeldCards attrs <> [card]}
        else pure a
    RanAbility you (isTarget attrs -> True) 2 _ _ -> do
      chooseOne you [targeting card (addToHand you card) | card <- supportHeldCards attrs]
      pure a
    IdentityMessage _ (AddToHand card) -> pure $ ThePaintedLady attrs {supportHeldCards = filter (/= card) $ supportHeldCards attrs}
    _ -> ThePaintedLady <$> runMessage msg attrs

paintedCardAvailable :: HasGame m => IdentityId -> PlayerCard -> m Bool
paintedCardAvailable you card = do
  unused <- discardResponseAvailable (pcCardId card)
  present <- case pcOwner card of
    Just owner -> cardInDeckOrDiscard owner card
    Nothing -> elem (pcCardId card) . map ecCardId <$> getEncounterDiscard
  pure $ unused && present
