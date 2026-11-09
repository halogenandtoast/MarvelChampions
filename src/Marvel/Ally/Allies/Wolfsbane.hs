module Marvel.Ally.Allies.Wolfsbane where
import Marvel.Prelude
import Marvel.Card
import Marvel.Game.Source ()
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Domino
import Marvel.Deck ()
import Marvel.Discard ()
import Marvel.Hand ()
import Marvel.Id ()
import Marvel.Identity.Types
import Marvel.Matchers
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Query ()
import Marvel.Question
import Marvel.Resource.Types ()
import Marvel.Trait qualified as Trait ()
import Marvel.Window qualified as W
import Marvel.Ability
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.GameValue ()
import Marvel.Stats
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner

wolfsbane :: AllyCard Wolfsbane
wolfsbane = ally (Wolfsbane . (`With` emptyMemory)) Cards.wolfsbane (Thw 2, 1) (Atk 1, 1) (HP 3)
newtype Wolfsbane = Wolfsbane (Attrs Ally `With` WolfsbaneMemory)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

-- Keep explicit shuffles separate from an automatic empty-deck reset while
-- the discard's responses resolve. Older saves stored only the named type.
data WolfsbaneMemory = WolfsbaneMemory
  { namedType :: Maybe CardType
  , movedCards :: [CardId]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

emptyMemory :: WolfsbaneMemory
emptyMemory = WolfsbaneMemory Nothing []

instance FromJSON WolfsbaneMemory where
  parseJSON Null = pure emptyMemory
  parseJSON value@(String _) = (\typ -> WolfsbaneMemory (Just typ) []) <$> parseJSON value
  parseJSON value = withObject "WolfsbaneMemory" (\o -> WolfsbaneMemory <$> o .:? "namedType" <*> o .:? "movedCards" .!= []) value

instance HasAbilities Wolfsbane where
  getAbilities a@(Wolfsbane (attrs `With` _)) = [limitedWindowAbility a 1
    (W.AllyThwarted W.After (AllyWithId $ allyId attrs) AnyScheme) Response OwnsThis NoCost (runAbility a 1)]


instance IsAlly Wolfsbane where
  toAllyAttrs (Wolfsbane (attrs `With` _)) = attrs
instance IsRef Wolfsbane where
  toRef = toRef . toAllyAttrs
instance HasModifiersFor Wolfsbane

instance RunMessage Wolfsbane where
  runMessage msg a@(Wolfsbane (attrs `With` remembered)) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      chooseOne you [labeled ("$label.name" <> tshow typ) (cardSpecific attrs "named" typ)
        | typ <- [AllyType, EventType, ResourceType, SupportType, UpgradeType, PlayerSideSchemeType, AttachmentType, EnvironmentType, MinionType, ObligationType, SideSchemeType, TreacheryType, MainSchemeType, VillainType, HeroType, AlterEgoType]]
      pure a
    CardSpecific (isTarget attrs -> True) "named" (fromJSON -> Success typ) -> do
      discardFromDeck (allyController attrs) 1 attrs
      pure $ Wolfsbane (attrs `With` WolfsbaneMemory (Just typ) [])
    WithDiscarded (isTarget attrs -> True) FromDeck cards -> do
      let you = allyController attrs
      eligible <- filterM (cardInDeckOrDiscard you) [card | PlayerCard card <- cards, Just (cdCardType $ getCardDef card) == namedType remembered, pcCardId card `notElem` movedCards remembered]
      for_ eligible $ \card -> chooseOne you
        [targeting card (addToHand you card), labeled "$label.noResponse" (pure ())]
      pure $ Wolfsbane (attrs `With` emptyMemory)
    CardSpecific (CardIdRef cid) "discardResponseMoved" _ | isJust (namedType remembered) ->
      pure $ Wolfsbane (attrs `With` remembered {movedCards = movedCards remembered <> [cid]})
    _ -> Wolfsbane . (`With` remembered) <$> runMessage msg attrs
