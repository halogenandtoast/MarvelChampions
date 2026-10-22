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
hulkSmash = event (HulkSmash . (`With` Meta Nothing)) Cards.hulkSmash

-- | While the basic attack it answers is made: whether that attack gains overkill.
newtype Meta = Meta {smashing :: Maybe Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype HulkSmash = HulkSmash (Attrs Event `With` Meta)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsEvent HulkSmash where
  toEventAttrs (HulkSmash (attrs `With` _)) = attrs

-- | "You get +10 ATK for that attack", with overkill if paid for using only [physical]
-- resources.
instance HasModifiersFor HulkSmash where
  getModifiersFor _ (IdentityRef ident) (HulkSmash (attrs `With` meta))
    | ident == eventController attrs
    , Just overkill <- smashing meta =
        pure $ AttackModifier 10 : [KeywordModifier Overkill | overkill]
  getModifiersFor _ _ _ = pure []

instance RunMessage HulkSmash where
  runMessage msg (HulkSmash (attrs `With` meta)) = case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent _ payment _ -> do
        overkill <- paidUsingOnly Physical payment
        pure $ HulkSmash (attrs `With` Meta (Just overkill))
      _ -> HulkSmash . (`With` meta) <$> runMessage msg attrs
    -- that attack is over
    CheckWindows ws
      | W.Window W.After (W.MadeBasicAttack (eventController attrs)) `elem` ws ->
          pure $ HulkSmash (attrs `With` Meta Nothing)
    _ -> HulkSmash . (`With` meta) <$> runMessage msg attrs
