module Marvel.Event.Events.HulkSmash (
  hulkSmash,
  HulkSmash (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Keyword
import Marvel.Message
import Marvel.Modifier
import Marvel.Payment
import Marvel.Ref
import Marvel.Resource
import Marvel.Window qualified as W

hulkSmash :: EventCard HulkSmash
hulkSmash = event (HulkSmash . (`With` Meta False)) Cards.hulkSmash

-- | Whether the basic attack it answers gains overkill, while it is made.
newtype Meta = Meta {overkill :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype HulkSmash = HulkSmash (Attrs Event `With` Meta)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsEvent HulkSmash where
  toEventAttrs (HulkSmash (attrs `With` _)) = attrs

-- | "If you paid for this card using only [physical] resources, that attack gains
-- overkill."
instance HasModifiersFor HulkSmash where
  getModifiersFor _ (IdentityRef ident) (HulkSmash (attrs `With` meta))
    | ident == eventController attrs && overkill meta = pure [KeywordModifier Overkill]
  getModifiersFor _ _ _ = pure []

instance RunMessage HulkSmash where
  runMessage msg (HulkSmash (attrs `With` meta)) = case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      -- "you get +10 ATK for that attack"
      PlayedEvent identityId payment _ -> do
        addToBasicPower identityId 10
        physical <- paidUsingOnly Physical payment
        pure $ HulkSmash (attrs `With` Meta physical)
      _ -> HulkSmash . (`With` meta) <$> runMessage msg attrs
    -- that attack is over
    CheckWindows ws
      | W.Window W.After (W.MadeBasicAttack (eventController attrs)) `elem` ws ->
          pure $ HulkSmash (attrs `With` Meta False)
    _ -> HulkSmash . (`With` meta) <$> runMessage msg attrs
