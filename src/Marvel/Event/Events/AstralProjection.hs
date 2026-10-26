module Marvel.Event.Events.AstralProjection (
  astralProjection,
  AstralProjection (..),
) where

import Marvel.Prelude

import Marvel.Boost
import Marvel.Card
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref

astralProjection :: EventCard AstralProjection
astralProjection =
  event (AstralProjection . (`With` Meta Nothing)) Cards.astralProjection

-- | The scheme chosen.
newtype Meta = Meta {scheme :: Maybe SchemeId}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype AstralProjection = AstralProjection (Attrs Event `With` Meta)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance IsEvent AstralProjection where
  toEventAttrs (AstralProjection (attrs `With` _)) = attrs

instance HasModifiersFor AstralProjection

instance IsRef AstralProjection where
  toRef = toRef . toEventAttrs

-- | Choose a scheme, then look at the top card of the encounter deck: 3 threat is
-- removed from it, and 1 more for each boost icon on that card.
instance RunMessage AstralProjection where
  runMessage msg e@(AstralProjection (attrs `With` meta)) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent identityId _ _) | ident == eventId attrs -> do
      schemes <- selectList ThwartableScheme
      chooseOnePrompted identityId "$label.chooseSchemeToThwart"
        [targeting s $ cardSpecific attrs "scheme" s | s <- schemes]
      pure e
    CardSpecific (isTarget attrs -> True) "scheme" (fromJSON -> Success s) -> do
      lookAtTopOfEncounterDeck 1 attrs
      pure . AstralProjection $ attrs `With` Meta (Just s)
    SearchFoundCards (isTarget attrs -> True) cards -> do
      countBoostIcons attrs () [c | EncounterCard c <- cards]
      pure e
    WithBoostIcons (isTarget attrs -> True) _ (sum . map snd -> boosts) | Just s <- scheme meta -> do
      thwartScheme s attrs (3 + boosts)
      pure e
    _ -> AstralProjection . (`With` meta) <$> runMessage msg attrs
