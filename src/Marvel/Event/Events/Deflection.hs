module Marvel.Event.Events.Deflection (
  deflection,
  Deflection (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Window qualified as W
import Marvel.Id
import Marvel.Damage
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref

deflection :: EventCard Deflection
deflection = event Deflection Cards.deflection

newtype Deflection = Deflection (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Interrupt: When an identity would take any amount of damage from an attack,
-- prevent up to 5 of that damage. Discard cards from the top of your deck equal to the
-- amount prevented this way. The player chooses how much (rulings).
instance RunMessage Deflection where
  runMessage msg e@(Deflection attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ (Just (W.IdentityTakeDamage hero _))) | ident == eventId attrs -> do
      -- what is left of the damage once other interrupts prevented some; each amount its
      -- own step: preventing edits the queue, which a choice would do as it is asked
      pending <- pendingDamageTo hero
      chooseOne you
        [ labeled (countLabel "preventDamage" n) $ cardSpecific attrs "prevent" (hero, n)
        | n <- [1 .. min 5 pending]
        ]
      pure e
    CardSpecific (isTarget attrs -> True) "prevent" (fromJSON -> Success (hero :: IdentityId, n :: Natural)) -> do
      prevented <- min n <$> pendingDamageTo hero
      preventDamage hero prevented
      discardTopOfDeck (eventController attrs) prevented
      pure e
    _ -> Deflection <$> runMessage msg attrs
