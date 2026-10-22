module Marvel.MainScheme.MainSchemes.AttackOnMountAthena where

import Marvel.Prelude

import Marvel.Area (experimentalWeaponsDeck)
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.EncounterSet qualified as EncounterSet
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.MainScheme.Cards qualified as Cards
import Marvel.MainScheme.Types
import Marvel.Message.Lifted

attackOnMountAthena :: MainSchemeCard AttackOnMountAthena
attackOnMountAthena =
  mainScheme AttackOnMountAthena Cards.attackOnMountAthena (PerPlayer 3) (Static 0) (PerPlayer 1)

newtype AttackOnMountAthena = AttackOnMountAthena (Attrs MainScheme)
  deriving anyclass (IsMainScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | 1A: the Experimental Weapons set is made a deck of its own, next to the main scheme
-- (rulebook p.5).
instance RunMessage AttackOnMountAthena where
  runMessage msg ms@(AttackOnMountAthena attrs) = runQueueT $ case msg of
    SetupMainScheme -> do
      weapons <- filter ((== Just EncounterSet.ExperimentalWeapons) . cdEncounterSet . getCardDef) <$> getEncounterDeck
      createDeck experimentalWeaponsDeck weapons
      pure ms
    AdvanceMainScheme -> ms <$ nextMainScheme
    _ -> AttackOnMountAthena <$> runMessage msg attrs
