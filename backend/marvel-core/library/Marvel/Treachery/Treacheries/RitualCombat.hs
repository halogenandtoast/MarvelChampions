module Marvel.Treachery.Treacheries.RitualCombat
  ( ritualCombat
  , RitualCombat(..)
  ) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message
import Marvel.Source
import Marvel.Target
import Marvel.Treachery.Attrs
import Marvel.Treachery.Cards qualified as Cards

ritualCombat :: TreacheryCard RitualCombat
ritualCombat = treachery RitualCombat Cards.ritualCombat

newtype RitualCombat = RitualCombat TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage RitualCombat where
  runMessage msg t@(RitualCombat attrs) = case msg of
    TreacheryMessage treacheryId msg' | toId attrs == treacheryId ->
      case msg' of
        RevealTreachery _ -> pure t
        _ -> RitualCombat <$> runMessage msg attrs
    _ -> RitualCombat <$> runMessage msg attrs
