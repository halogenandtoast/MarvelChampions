module Marvel.SideScheme.SideSchemes.AgeOfApocalypseEnSabahNur where

import Marvel.Prelude
import Marvel.Game.Source
import Marvel.Id
import Marvel.Query ()
import Marvel.Scenario.AgeOfApocalypseEnSabahNur
import Marvel.Status ()
import Marvel.Card
import Marvel.Entity
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.Ability ()
import Marvel.Modifier (HasModifiersFor)
import Marvel.Ref
import Marvel.GameValue
import Marvel.Stats ()
import Marvel.Hp ()
import Marvel.Message.Lifted

newtype AoaEnSabahNurSideScheme = AoaEnSabahNurSideScheme (Attrs SideScheme `With` Maybe IdentityId)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

sourceOfPower :: SideSchemeCard AoaEnSabahNurSideScheme
sourceOfPower = sideScheme (AoaEnSabahNurSideScheme . (`With` Nothing)) Cards.sourceOfPower (Static 5)

pluggedIn :: SideSchemeCard AoaEnSabahNurSideScheme
pluggedIn = sideScheme (\a -> AoaEnSabahNurSideScheme (a {sideSchemeCrisis = True} `With` Nothing)) Cards.pluggedIn (Static 5)

giantGrowth :: SideSchemeCard AoaEnSabahNurSideScheme
giantGrowth = sideScheme (AoaEnSabahNurSideScheme . (`With` Nothing)) Cards.giantGrowth (Static 5)


instance IsSideScheme AoaEnSabahNurSideScheme where
  toSideSchemeAttrs (AoaEnSabahNurSideScheme (attrs `With` _)) = attrs

instance HasCardCode AoaEnSabahNurSideScheme where
  toCardCode (AoaEnSabahNurSideScheme (attrs `With` _)) = toCardCode attrs

instance IsRef AoaEnSabahNurSideScheme where
  toRef (AoaEnSabahNurSideScheme (attrs `With` _)) = toRef attrs

instance RunMessage AoaEnSabahNurSideScheme where
  runMessage msg a@(AoaEnSabahNurSideScheme (attrs `With` defeatedBy)) = runQueueT $ case msg of
    SideSchemeMessage sid (SideSchemeThwarted source _) | sid == sideSchemeId attrs -> do
      you <- maybe getActivePlayerId pure =<< getSourceController source
      attrs' <- runMessage msg attrs
      pure $ AoaEnSabahNurSideScheme $ attrs' `With` Just you
    SideSchemeMessage sid DefeatSideScheme | sid == sideSchemeId attrs -> do
      you <- getDefeatedBy attrs
      same <- apocalypseInForm form
      if same then enSabahNur >>= traverse_ (`villainActivatesAgainst` you)
        else do
          changeApocalypseForm form
          enSabahNur >>= traverse_ enemyBecomesTough
      pure a
    _ -> AoaEnSabahNurSideScheme . (`With` defeatedBy) <$> runMessage msg attrs
   where
    form = case toCardCode attrs of
      "45153" -> "Biomorph"
      "45154" -> "Cyberpath"
      _ -> "Giant"
