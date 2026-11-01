module Marvel.Event.Events.MadeOfRage (
  madeOfRage,
  MadeOfRage (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Keyword
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Window qualified as W

madeOfRage :: EventCard MadeOfRage
madeOfRage = event (MadeOfRage . (`With` Meta False)) Cards.madeOfRage

-- | Whether the basic attack it answers gains overkill, while it is made.
newtype Meta = Meta {overkill :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype MadeOfRage = MadeOfRage (Attrs Event `With` Meta)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsEvent MadeOfRage where
  toEventAttrs (MadeOfRage (attrs `With` _)) = attrs

-- | "If you paid for this card using only [physical] resources, that attack gains
-- overkill."
instance HasModifiersFor MadeOfRage where
  getModifiersFor _ (IdentityRef ident) (MadeOfRage (attrs `With` meta))
    | ident == eventController attrs && overkill meta = pure [KeywordModifier Overkill]
  getModifiersFor _ _ _ = pure []

instance RunMessage MadeOfRage where
  runMessage msg (MadeOfRage (attrs `With` meta)) = runQueueT $ case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      -- "you get +10 ATK for that attack"
      PlayedEvent identityId _ _ -> do
        addToBasicPower identityId 6
        let physical = True
        pure $ MadeOfRage (attrs `With` Meta physical)
      _ -> MadeOfRage . (`With` meta) <$> runMessage msg attrs
    -- that attack is over
    CheckWindows ws
      | W.Window W.After (W.MadeBasicAttack (eventController attrs)) `elem` ws ->
          pure $ MadeOfRage (attrs `With` Meta False)
    _ -> MadeOfRage . (`With` meta) <$> runMessage msg attrs
