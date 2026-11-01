module Marvel.SideScheme.SideSchemes.AgeOfApocalypseInfinites where

import Marvel.Prelude
import Marvel.Card
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Ref
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Id
import Marvel.Matchers
import Marvel.Modifier
import Marvel.Query
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.Window qualified as W

newtype AoaInfinitesSideScheme = AoaInfinitesSideScheme (Attrs SideScheme)
  deriving anyclass (IsSideScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

genePool :: SideSchemeCard AoaInfinitesSideScheme
genePool = sideScheme AoaInfinitesSideScheme Cards.genePool (Static 4)

genePoolThreat :: HasGame m => m Natural
genePoolThreat = do
  schemes <- selectList (SideSchemeIs Cards.genePool)
  sum <$> traverse (getSchemeThreatAmount . SchemeSideSchemeId) schemes

placeGenePoolThreat :: ReverseQueue m => Natural -> m ()
placeGenePoolThreat n = selectList (SideSchemeIs Cards.genePool) >>= traverse_ (`placeThreat` n)

instance HasModifiersFor AoaInfinitesSideScheme where
  getModifiersFor _ target (AoaInfinitesSideScheme a) | target == toRef a = pure [CannotLeavePlay]
  getModifiersFor _ _ _ = pure []

instance RunMessage AoaInfinitesSideScheme where
  runMessage msg e@(AoaInfinitesSideScheme a) = runQueueT $ case msg of
    CheckWindows windows -> do
      -- Consequential defeat is included in the same batch as the generic defeat.
      let consequential = [toCardId c | W.Window W.After (W.AllyCardDefeatedByConsequentialDamage _ c) <- windows]
          defeated = [c | W.Window W.After (W.AllyCardDefeated _ c) <- windows, toCardId c `notElem` consequential]
      for_ defeated $ \_ -> placeThreat (sideSchemeId a) 3
      pure e
    _ -> AoaInfinitesSideScheme <$> runMessage msg a
