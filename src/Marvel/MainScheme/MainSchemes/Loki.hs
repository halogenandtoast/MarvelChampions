module Marvel.MainScheme.MainSchemes.Loki (
  allHailKingLoki,
  AllHailKingLoki (..),
) where

import Marvel.Prelude

import Marvel.Card
import Marvel.Entity
import Marvel.GameValue
import Marvel.MainScheme.Cards qualified as Cards
import Marvel.MainScheme.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.SideScheme.Cards qualified as SideSchemes

allHailKingLoki :: MainSchemeCard AllHailKingLoki
allHailKingLoki = mainScheme AllHailKingLoki Cards.allHailKingLoki (PerPlayer 12) (PerPlayer 1) (PerPlayer 1)

newtype AllHailKingLoki = AllHailKingLoki (Attrs MainScheme)
  deriving anyclass (IsMainScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | 1A Setup: put War in Asgard into play (the encounter deck searched, then shuffled),
-- and reveal the top card of the Infinity Stone deck; the Lokis are the scenario's to set
-- aside and reveal, as it starts. 1B's Forced Interrupt and victory condition are the
-- scenario's ('ResolveVillainDefeat'). Completed: the players lose.
instance RunMessage AllHailKingLoki where
  runMessage msg ms@(AllHailKingLoki attrs) = runQueueT $ case msg of
    SetupMainScheme -> do
      first <- selectJust FirstPlayer
      searchAndPutIntoPlayEncounterCard first (CardIs SideSchemes.warInAsgard)
      revealTopInfinityStone first
      pure ms
    AdvanceMainScheme ((== mainSchemeId attrs) -> True) -> ms <$ gameOver Lost
    _ -> AllHailKingLoki <$> runMessage msg attrs
