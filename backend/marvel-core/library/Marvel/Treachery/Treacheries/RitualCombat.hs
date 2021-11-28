module Marvel.Treachery.Treacheries.RitualCombat
  ( ritualCombat
  , RitualCombat(..)
  ) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message
import Marvel.Queue
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
        RevealTreachery ident -> do
          push $ DiscardTopOfEncounterDeck 1 (Just $ toTarget attrs)
          pure . RitualCombat $ attrs & resolverL ?~ ident
        _ -> RitualCombat <$> runMessage msg attrs
    WithDiscarded target _ cards | isTarget attrs target -> do
      pure t
    _ -> RitualCombat <$> runMessage msg attrs
