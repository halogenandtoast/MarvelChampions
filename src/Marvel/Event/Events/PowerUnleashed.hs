module Marvel.Event.Events.PowerUnleashed (
  powerUnleashed,
  PowerUnleashed (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Card.Id (CardId (..))
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref

powerUnleashed :: EventCard PowerUnleashed
powerUnleashed = event PowerUnleashed Cards.powerUnleashed

newtype PowerUnleashed = PowerUnleashed (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Deal 5 damage to the villain and remove 5 threat from the main scheme, as one
-- effect; neither an attack nor a thwart. Then it is removed from the game.
instance RunMessage PowerUnleashed where
  runMessage msg e@(PowerUnleashed attrs) = runQueueT $ case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent you _ _ -> do
        -- "the villain" and "the main scheme" you choose where there are two (Tower
        -- Defense, MTS rulebook pp.10-11), chosen first, so nothing is asked inside the
        -- one effect
        removable <- selectList ThwartableScheme
        schemes <- filter (`elem` removable) <$> selectList MainScheme
        let withScheme k = case schemes of
              _ : _ : _ -> chooseOne you [targeting (schemeRef s) (k $ thwartScheme s attrs 5) | s <- schemes]
              _ -> k $ for_ (headMay schemes) \s -> thwartScheme s attrs 5
        -- with no villain in play (as between one Kang and the next), only the threat
        villain <- selectAny ActiveVillain
        if villain
          then withTheVillain you \v -> withScheme (dealDamageAnd v attrs (toDamage 5 FromAbility))
          else withScheme id
        pure e
      ResolvedEvent -> do
        removeFromGame (CardIdRef . CardId . unEventId $ eventId attrs)
        pure e
      _ -> PowerUnleashed <$> runMessage msg attrs
    _ -> PowerUnleashed <$> runMessage msg attrs
