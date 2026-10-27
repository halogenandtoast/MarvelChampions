module Marvel.Event.Events.LeadingBlow (
  leadingBlow,
  LeadingBlow (..),
) where

import Marvel.Prelude

import Marvel.Boost
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Payment
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Window qualified as W

leadingBlow :: EventCard LeadingBlow
leadingBlow = event (LeadingBlow . (`With` Meta Nothing False)) Cards.leadingBlow

-- | While the basic attack it answers is made: the ATK it takes away, and whether the
-- hero has been readied for it.
data Meta = Meta {penalty :: Maybe Natural, readiedHero :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype LeadingBlow = LeadingBlow (Attrs Event `With` Meta)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsEvent LeadingBlow where
  toEventAttrs (LeadingBlow (attrs `With` _)) = attrs

-- | "Reduce your hero's ATK for that attack by the number of printed boost icons on that
-- card."
instance HasModifiersFor LeadingBlow where
  getModifiersFor _ (IdentityRef ident) (LeadingBlow (attrs `With` meta))
    | ident == eventController attrs
    , Just n <- penalty meta
    , n > 0 =
        pure [AttackPenalty n]
  getModifiersFor _ _ _ = pure []

-- | Hero Interrupt: When your hero makes a basic attack, discard the top card of the
-- encounter deck → reduce your hero's ATK for that attack by the number of printed boost
-- icons on that card. If that attack still deals damage, ready your hero. It deals
-- damage when an enemy takes damage from it (rulings).
instance RunMessage LeadingBlow where
  runMessage msg (LeadingBlow (attrs `With` meta)) = runQueueT $ case msg of
    -- the card discarded to pay for it
    EventMessage ident (PlayedEvent _ payment _) | ident == eventId attrs -> do
      let n = sum [boostCount (cdBoostIcons $ getCardDef card) | card <- paymentEncounterDiscards payment]
      pure $ LeadingBlow (attrs `With` Meta (Just n) False)
    CheckWindows ws
      | W.Window W.After (W.MadeBasicAttack (eventController attrs)) `elem` ws ->
          LeadingBlow . (`With` Meta Nothing False) <$> runMessage msg attrs
      | Just _ <- penalty meta
      , not (readiedHero meta)
      , or [damageSource dmg == FromPlayerAttack (eventController attrs) | W.Window W.After (W.EnemyTookDamageFrom _ dmg _) <- ws] -> do
          ready (eventController attrs)
          LeadingBlow . (`With` meta {readiedHero = True}) <$> runMessage msg attrs
    _ -> LeadingBlow . (`With` meta) <$> runMessage msg attrs
