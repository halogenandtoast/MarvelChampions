module Marvel.MainScheme.MainSchemes.SmSandman where

import Marvel.Prelude
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Entity
import Marvel.Environment.Cards qualified as Environments
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.MainScheme.Cards qualified as Cards
import Marvel.MainScheme.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref

newtype SmSandmanMainScheme = SmSandmanMainScheme (Attrs MainScheme)
  deriving anyclass IsMainScheme
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

smHaplessPedestrians :: MainSchemeCard SmSandmanMainScheme
smHaplessPedestrians = mainScheme SmSandmanMainScheme Cards.smHaplessPedestrians (PerPlayer 9) (PerPlayer 2) (PerPlayer 1)

instance RunMessage SmSandmanMainScheme where
  runMessage msg e@(SmSandmanMainScheme attrs) = runQueueT $ case msg of
    AdvanceMainScheme sid | sid == mainSchemeId attrs -> e <$ gameOver Lost
    SetupMainScheme -> do
      first <- selectJust FirstPlayer
      searchAndPutIntoPlayEncounterCard first (CardIs Environments.smCityStreets)
      cardSpecific attrs "initialSand" ()
      pure e
    CardSpecific (isTarget attrs -> True) "initialSand" _ -> do
      streets <- selectList (EnvironmentIs Environments.smCityStreets)
      for_ streets (`placeCounters` 4)
      pure e
    PlaceAccelerationToken mid | mid == mainSchemeId attrs -> do
      first <- selectJust FirstPlayer
      forcedResponse attrs (dealIndirectDamage first attrs 3)
      pure e
    _ -> SmSandmanMainScheme <$> runMessage msg attrs
