module Marvel.SideScheme.SideSchemes.DreadfulDeeds where

import Marvel.Prelude
import Marvel.Ability (HasAbilities)
import Marvel.Aspect (Aspect (Pool))
import Marvel.Game.Source
import Marvel.Query
import Marvel.Matchers
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.GameValue

dreadfulDeeds :: SideSchemeCard DreadfulDeeds
dreadfulDeeds = sideScheme DreadfulDeeds Cards.dreadfulDeeds (Static 2)

newtype DreadfulDeeds = DreadfulDeeds (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage DreadfulDeeds where
  runMessage msg s@(DreadfulDeeds attrs) = runQueueT $ case msg of
    SideSchemeMessage sid RevealSideScheme | sid == sideSchemeId attrs -> do
      players <- selectList AnyIdentity
      pink <- filterM (\you -> controlsAspectCard you Pool) players
      placeThreat (sideSchemeId attrs) (2 * fromIntegral (length pink))
      pure s
    _ -> DreadfulDeeds <$> runMessage msg attrs
