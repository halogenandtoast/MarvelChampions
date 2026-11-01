module Marvel.SideScheme.SideSchemes.AgeOfApocalypseDystopianNightmare where

import Marvel.Prelude
import Marvel.Card
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Ref
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Modifier (HasModifiersFor)
import Marvel.Query ()
import Marvel.Id
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

newtype AoaDystopianNightmareSideScheme = AoaDystopianNightmareSideScheme (Attrs SideScheme `With` Maybe IdentityId)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsSideScheme AoaDystopianNightmareSideScheme where
  toSideSchemeAttrs (AoaDystopianNightmareSideScheme (a `With` _)) = a

targetedForExtermination :: SideSchemeCard AoaDystopianNightmareSideScheme
targetedForExtermination = sideScheme (\a -> AoaDystopianNightmareSideScheme (a {sideSchemeCrisis = True} `With` Nothing)) Cards.targetedForExtermination (Static 3)

instance RunMessage AoaDystopianNightmareSideScheme where
  runMessage msg e@(AoaDystopianNightmareSideScheme (a `With` player)) = runQueueT $ case msg of
    SideSchemeMessage sid (SideSchemeThwarted source _) | sid == sideSchemeId a -> do
      you <- getSourceController source
      a' <- runMessage msg a
      pure $ AoaDystopianNightmareSideScheme (a' `With` you)
    SideSchemeMessage sid (SideSchemeThreatRemoved source _) | sid == sideSchemeId a -> do
      you <- getSourceController source
      a' <- runMessage msg a
      pure $ AoaDystopianNightmareSideScheme (a' `With` you)
    SideSchemeMessage sid DefeatSideScheme | sid == sideSchemeId a -> do
      you <- maybe getActivePlayerId pure player
      confuseIdentity you
      pure e
    _ -> (\a' -> AoaDystopianNightmareSideScheme (a' `With` player)) <$> runMessage msg a
