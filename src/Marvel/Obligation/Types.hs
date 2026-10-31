module Marvel.Obligation.Types where

import Marvel.Prelude

import Data.Typeable
import Marvel.Card
import Marvel.Entity
import Marvel.Id hiding (ObligationId)
import Marvel.Id as X (ObligationId)
import Marvel.Ability
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Cost.Types (Cost (NoCost))
import Marvel.Criteria (Criteria (ExtendedCardExists))
import Marvel.Game.Source
import Marvel.Hand
import Marvel.Identity.Types (Field (PlayerIdentityHand))
import Marvel.Matchers (ExtendedCardMatcher (BasicCardMatches, InHandOf), IdentityMatcher (You))
import Marvel.Projection
import Marvel.Question (Choice (RunAbility))
import Marvel.Resource.Types (Resource (Wild))

data Obligation = forall a. (IsObligation a) => Obligation a

instance Show Obligation where
  show (Obligation a) = show a

instance Eq Obligation where
  Obligation (a :: a) == Obligation (b :: b) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

instance ToJSON Obligation where
  toJSON (Obligation a) = toJSON a

data SomeObligationCard
  = forall a.
    (IsObligation a) =>
    SomeObligationCard
      (ObligationCard a)

liftObligationCard ::
  (forall a. ObligationCard a -> b) -> SomeObligationCard -> b
liftObligationCard f (SomeObligationCard a) = f a

someObligationCardCode :: SomeObligationCard -> CardCode
someObligationCardCode = liftObligationCard toCardCode

instance Entity Obligation where
  type Id Obligation = ObligationId
  data Attrs Obligation = ObligationAttrs
    { obligationId :: ObligationId
    , obligationCardDef :: CardDef
    , obligationSurge :: Bool
    , obligationHeldCards :: [PlayerCard]
    -- ^ player cards facedown under it, discarded with it
    , obligationOwner :: Maybe IdentityId
    -- ^ the player whose play area it is in
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)
  data Field Obligation :: Type -> Type where
    ObligationId :: Field Obligation ObligationId
    ObligationCardDef :: Field Obligation CardDef
    ObligationSurge :: Field Obligation Bool
    ObligationHeldCards :: Field Obligation [PlayerCard]
    ObligationOwner :: Field Obligation (Maybe IdentityId)
  field fld o =
    let ObligationAttrs {..} = toAttrs o
     in case fld of
          ObligationId -> obligationId
          ObligationCardDef -> obligationCardDef
          ObligationSurge -> obligationSurge
          ObligationHeldCards -> obligationHeldCards
          ObligationOwner -> obligationOwner
  toId = obligationId . toAttrs
  toAttrs (Obligation a) = toObligationAttrs a

instance RunMessage Obligation where
  runMessage msg (Obligation a) = Obligation <$> runMessage msg a

instance IsRef Obligation where
  toRef = ObligationRef . toId

instance HasAbilities Obligation where
  getAbilities (Obligation a) = getAbilities a

instance HasModifiersFor Obligation where
  getBlankingModifiersFor source target (Obligation a) = getBlankingModifiersFor source target a
  getModifiersFor source target (Obligation a) = getModifiersFor source target a

instance IsCard Obligation where
  toCard = toCard . toAttrs

instance HasCardDef Obligation where
  getCardDef = getCardDef . toAttrs

class (Typeable a, Show a, Eq a, ToJSON a, FromJSON a, HasAbilities a, HasModifiersFor a, RunMessage a) => IsObligation a where
  toObligationAttrs :: a -> Attrs Obligation
  default toObligationAttrs :: (Coercible a (Attrs Obligation)) => a -> Attrs Obligation
  toObligationAttrs = coerce

type ObligationCard a = CardBuilder ObligationId a

surgeL :: Lens' (Attrs Obligation) Bool
surgeL = lens obligationSurge $ \m x -> m {obligationSurge = x}

heldCardsL :: Lens' (Attrs Obligation) [PlayerCard]
heldCardsL = lens obligationHeldCards $ \m x -> m {obligationHeldCards = x}

instance HasCardCode (Attrs Obligation) where
  toCardCode = toCardCode . obligationCardDef

obligation :: (Attrs Obligation -> a) -> CardDef -> CardBuilder ObligationId a
obligation f cardDef =
  CardBuilder
    { cbCardDef = cardDef
    , cbCardBuilder = \cardDef mid ->
        f $
          ObligationAttrs
            { obligationId = mid
            , obligationCardDef = cardDef
            , obligationSurge = False
            , obligationHeldCards = []
            , obligationOwner = Nothing
            }
    }

instance IsRef (Attrs Obligation) where
  toRef = ObligationRef . obligationId

instance IsCard (Attrs Obligation) where
  toCard a =
    EncounterCard $
      MkEncounterCard
        { ecCardId = CardId $ unObligationId $ obligationId a
        , ecCardDef = getCardDef a
        }

instance HasCardDef (Attrs Obligation) where
  getCardDef = obligationCardDef

instance RunMessage (Attrs Obligation) where
  runMessage msg attrs = runQueueT $ case msg of
    RemoveFromGame (CardIdRef cid) -> pure attrs {obligationHeldCards = filter ((/= cid) . pcCardId) (obligationHeldCards attrs)}
    ObligationMessage ident msg' | ident == obligationId attrs -> case msg' of
      ObligationOwnedBy identityId -> pure attrs {obligationOwner = Just identityId}
      RevealObligation identityId -> pure attrs {obligationOwner = Just identityId}
      -- resolved, it is discarded as a treachery is, never having entered play (an
      -- obligation that stays answers this itself): not a card discarded from play
      ResolvedObligation identityId -> do
        active <- obligationIsInPlay (obligationId attrs)
        when active do
          for_ (obligationHeldCards attrs) discardCard
          removeFromGame attrs
          discardCard attrs
          when (obligationSurge attrs) $ surge identityId
        pure $ attrs & heldCardsL .~ []
      _ -> pure attrs
    GainSurge target | isTarget attrs target -> pure $ attrs & surgeL .~ True
    -- the cards under it go to their owners' discard piles as it leaves play
    RemoveFromPlay target | isTarget attrs target -> do
      for_ (obligationHeldCards attrs) discardCard
      pure $ attrs & heldCardsL .~ []
    _ -> pure attrs

-- | "Alter-Ego Action: Discard a card matching from your hand → discard this
-- obligation", as The Once and Future Kang's obligations have: the ability, offered
-- while there is such a card in your hand.
discardToDiscardAbility :: Attrs Obligation -> CardMatcher -> [Ability]
discardToDiscardAbility a matcher = case obligationOwner a of
  Just _ -> [ability a 1 AlterEgoAction (ExtendedCardExists $ InHandOf You $ BasicCardMatches matcher) NoCost $ RunAbility (toTarget a) 1]
  Nothing -> []

-- | Its resolution: the player discards the card of their choice, then the obligation.
discardToDiscard :: (ReverseQueue m) => Attrs Obligation -> IdentityId -> CardMatcher -> m ()
discardToDiscard a you matcher = do
  hand <- filter (cardMatch matcher) . unHand <$> project PlayerIdentityHand you
  unless (null hand) do
    chooseOne you [targeting c (discardCard c) | c <- hand]
    removeFromPlay a

-- | "A [resource] resource": a card with the resource's icon, or a wild one.
withResource :: Resource -> CardMatcher
withResource r = CardMatchesAny [CardWithResource r, CardWithResource Wild]
