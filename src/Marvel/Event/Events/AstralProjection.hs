module Marvel.Event.Events.AstralProjection (
  astralProjection,
  AstralProjection (..),
) where

import Marvel.Prelude

import Marvel.Boost
import Marvel.Card
import Marvel.Choice
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Id
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Queue
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
  runMessage msg e@(AstralProjection (attrs `With` meta)) = case msg of
    EventMessage ident (PlayedEvent identityId _ _) | ident == eventId attrs -> do
      schemes <- selectList ThwartableScheme
      push $
        Ask identityId $
          prompted "$label.chooseSchemeToThwart" $
            ChooseOne
              [ TargetLabel (toRef s) [Run [cardSpecific attrs "scheme" s]]
              | s <- schemes
              ]
      pure e
    CardSpecific (isTarget attrs -> True) "scheme" (fromJSON -> Success s) -> do
      push $ LookAtTopOfEncounterDeck (toTarget attrs)
      pure . AstralProjection $ attrs `With` Meta (Just s)
    LookedAt (isTarget attrs -> True) cards | Just s <- scheme meta -> do
      let boosts = sum $ map (boostCount . cdBoostIcons . getCardDef) cards
      pushChoice (eventController attrs) $ ThwartScheme (toRef s) (toSource attrs) (3 + boosts)
      pure e
    _ -> AstralProjection . (`With` meta) <$> runMessage msg attrs
