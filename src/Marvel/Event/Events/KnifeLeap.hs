module Marvel.Event.Events.KnifeLeap (
  knifeLeap,
  KnifeLeap (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Card.Id
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Id
import Marvel.Identity.Types (Field (PlayerIdentityCounters))
import Marvel.Keyword
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Projection
import Marvel.Ref
import Marvel.Window qualified as W

knifeLeap :: EventCard KnifeLeap
knifeLeap = event (KnifeLeap . (`With` Meta False)) Cards.knifeLeap

-- | Whether the basic attack it answers is being made.
newtype Meta = Meta {leaping :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype KnifeLeap = KnifeLeap (Attrs Event `With` Meta)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsEvent KnifeLeap where
  toEventAttrs (KnifeLeap (attrs `With` _)) = attrs

-- | In hand, it costs 1 less for each vengeance counter on Drax (his identity); while
-- the attack it answers is made, the hero gets +5 ATK and the attack gains overkill and
-- piercing.
instance HasModifiersFor KnifeLeap where
  getModifiersFor _ (CardIdRef cid) (KnifeLeap (attrs `With` _))
    | cid == CardId (unEventId $ eventId attrs) = do
        n <- project PlayerIdentityCounters (eventController attrs)
        pure [ResourceCostReduction n | n > 0]
  getModifiersFor _ (IdentityRef ident) (KnifeLeap (attrs `With` meta))
    | ident == eventController attrs && leaping meta =
        pure [AttackModifier 5, KeywordModifier Overkill, KeywordModifier Piercing]
  getModifiersFor _ _ _ = pure []

instance RunMessage KnifeLeap where
  runMessage msg (KnifeLeap (attrs `With` meta)) = runQueueT $ case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      -- "you get +5 ATK for that attack"
      PlayedEvent {} -> pure $ KnifeLeap (attrs `With` Meta True)
      _ -> KnifeLeap . (`With` meta) <$> runMessage msg attrs
    -- that attack is over
    CheckWindows ws
      | W.Window W.After (W.MadeBasicAttack (eventController attrs)) `elem` ws ->
          pure $ KnifeLeap (attrs `With` Meta False)
    _ -> KnifeLeap . (`With` meta) <$> runMessage msg attrs
