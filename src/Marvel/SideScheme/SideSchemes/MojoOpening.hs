module Marvel.SideScheme.SideSchemes.MojoOpening where

import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Environment.Cards qualified as Environments
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.MojoOpening
import Marvel.Prelude
import Marvel.Query
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

newtype MojoOpeningSideScheme = MojoOpeningSideScheme (Attrs SideScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

mojo39008, mojo39016 :: SideSchemeCard MojoOpeningSideScheme
mojo39008 = sideScheme MojoOpeningSideScheme Cards.mojo39008 (PerPlayer 4)
mojo39016 = sideScheme MojoOpeningSideScheme Cards.mojo39016 (PerPlayer 3)

instance IsSideScheme MojoOpeningSideScheme where
  sideSchemeAbilities = getAbilities

instance HasModifiersFor MojoOpeningSideScheme where
  getModifiersFor _ target (MojoOpeningSideScheme a)
    | toCardCode a == "39016", target == toRef a = pure [CannotBeDefeated, CannotLeavePlay]
    | otherwise = pure []

instance HasAbilities MojoOpeningSideScheme where
  getAbilities (MojoOpeningSideScheme a) =
    [ability a 1 HeroAction NoCriteria (DamageCost 2) (runAbility a 1) | toCardCode a == "39016"]

instance RunMessage MojoOpeningSideScheme where
  runMessage msg e@(MojoOpeningSideScheme a) = runQueueT $ case msg of
    SideSchemeMessage sid RevealSideScheme | sid == sideSchemeId a, toCardCode a == "39008" -> do
      cheering <- selectAny (EnvironmentIs Environments.mojo39003b)
      when cheering $ do
        players <- fromIntegral <$> getStartingPlayerCount
        placeThreat (SchemeSideSchemeId sid) players
      MojoOpeningSideScheme <$> runMessage msg a
    SideSchemeMessage sid DefeatSideScheme | sid == sideSchemeId a, toCardCode a == "39008" -> do
      players <- fromIntegral <$> getStartingPlayerCount
      rateChallengers players
      MojoOpeningSideScheme <$> runMessage msg a
    CardSpecific (isTarget a -> True) "resolveThwart" (fromJSON -> Success (source, _ :: Natural)) | toCardCode a == "39016" -> afterRemoval source =<< runMessage msg a
    SideSchemeMessage sid (SideSchemeThreatRemoved source _) | sid == sideSchemeId a, toCardCode a == "39016" -> afterRemoval source =<< runMessage msg a
    CardSpecific (isTarget a -> True) "searchShow" (fromJSON -> Success you) -> do
      revealShow you
      players <- fromIntegral <$> getStartingPlayerCount
      placeThreat (SchemeSideSchemeId $ sideSchemeId a) (3 * players)
      pure e
    RanAbility you (isTarget a -> True) 1 _ _ -> do
      removeThreat you a 3 (SchemeWithId $ SchemeSideSchemeId $ sideSchemeId a)
      pure e
    _ -> MojoOpeningSideScheme <$> runMessage msg a
   where
    afterRemoval source updated = do
      when (sideSchemeThreat a > 0 && sideSchemeThreat updated == 0) do
        you <- maybe getYou pure =<< getSourceController source
        cardSpecific a "searchShow" you
      pure $ MojoOpeningSideScheme updated
