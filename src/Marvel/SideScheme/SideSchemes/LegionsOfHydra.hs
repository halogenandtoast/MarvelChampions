module Marvel.SideScheme.SideSchemes.LegionsOfHydra (
  legionsOfHydra,
  LegionsOfHydra (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Cards qualified as Cards
import Marvel.Modifier
import Marvel.Query
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.Trait

legionsOfHydra :: SideSchemeCard LegionsOfHydra
legionsOfHydra = sideScheme LegionsOfHydra Cards.legionsOfHydra (Static 3)

newtype LegionsOfHydra = LegionsOfHydra (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage LegionsOfHydra where
  runMessage msg s@(LegionsOfHydra attrs) = runQueueT $ case msg of
    -- if Madame Hydra is not in play she is found and engages you, then 2 more threat
    -- is placed here for each Hydra enemy in play, counted once she is
    SideSchemeMessage ident RevealSideScheme | ident == sideSchemeId attrs -> do
      inPlay <- selectAny (MinionIs Cards.madameHydra)
      unless inPlay $ searchForAndRevealScheme Cards.madameHydra
      sideSchemeRevealedThen ident
      pure s
    SideSchemeMessage ident SideSchemeRevealedThen | ident == sideSchemeId attrs -> do
      hydra <- selectListCount (MinionWithTrait Hydra)
      placeThreat ident (2 * hydra)
      pure s
    _ -> LegionsOfHydra <$> runMessage msg attrs
