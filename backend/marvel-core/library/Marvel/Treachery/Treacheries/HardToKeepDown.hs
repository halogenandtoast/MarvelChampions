module Marvel.Treachery.Treacheries.HardToKeepDown where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message
import Marvel.Query
import Marvel.Queue
import Marvel.Source
import Marvel.Target
import Marvel.Treachery.Attrs
import Marvel.Treachery.Cards qualified as Cards

hardToKeepDown :: TreacheryCard HardToKeepDown
hardToKeepDown = treachery HardToKeepDown Cards.hardToKeepDown

newtype HardToKeepDown = HardToKeepDown TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage HardToKeepDown where
  runMessage msg t@(HardToKeepDown attrs) = case msg of
    TreacheryMessage tid msg' | tid == toId attrs -> case msg' of
      RevealTreachery ident -> do
        damaged <- selectAny $ ActiveVillain <> VillainWithAnyDamage
        if damaged
          then do
            villainId <- selectJust ActiveVillain
            push $ VillainMessage villainId (VillainHealed 4)
            pure t
          else pure . HardToKeepDown $ attrs & surgeL .~ True
        pure t
      _ -> HardToKeepDown <$> runMessage msg attrs
    _ -> HardToKeepDown <$> runMessage msg attrs
