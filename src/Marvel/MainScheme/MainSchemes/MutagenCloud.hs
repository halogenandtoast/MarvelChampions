module Marvel.MainScheme.MainSchemes.MutagenCloud where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.GameValue
import Marvel.MainScheme.Cards qualified as Cards
import Marvel.MainScheme.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Trait (Trait (Goblin))

-- | Its acceleration is X, the number of Goblin enemies (Green Goblin included) in
-- play, counted as the threat is placed.
mutagenCloud :: MainSchemeCard MutagenCloud
mutagenCloud =
  mainScheme MutagenCloud Cards.mutagenCloud (PerPlayer 11) (PerPlayer 4) (Static 0)

newtype MutagenCloud = MutagenCloud (Attrs MainScheme)
  deriving anyclass (IsMainScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage MutagenCloud where
  runMessage msg ms@(MutagenCloud attrs) = runQueueT $ case msg of
    AccelerateMainScheme -> do
      goblins <- selectListCount (EnemyWithTrait Goblin)
      MutagenCloud <$> runMessage msg attrs {mainSchemeAcceleration = Static (fromIntegral goblins)}
    AdvanceMainScheme ((== mainSchemeId attrs) -> True) -> ms <$ gameOver Lost
    _ -> MutagenCloud <$> runMessage msg attrs
