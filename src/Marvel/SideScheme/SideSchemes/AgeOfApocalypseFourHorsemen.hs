module Marvel.SideScheme.SideSchemes.AgeOfApocalypseFourHorsemen where

import Marvel.Prelude
import Marvel.Id
import Marvel.Game.Source
import Marvel.Damage
import Marvel.Matchers
import Marvel.Query
import Marvel.Question ()
import Marvel.MainScheme.Cards qualified as MainSchemes
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

newtype AoaFourHorsemenSideScheme = AoaFourHorsemenSideScheme (Attrs SideScheme `With` Maybe IdentityId)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

theRavagesOfWar :: SideSchemeCard AoaFourHorsemenSideScheme
theRavagesOfWar = sideScheme (AoaFourHorsemenSideScheme . (`With` Nothing)) Cards.theRavagesOfWar (Static 6)

aTimeOfFamine :: SideSchemeCard AoaFourHorsemenSideScheme
aTimeOfFamine = sideScheme (AoaFourHorsemenSideScheme . (`With` Nothing)) Cards.aTimeOfFamine (Static 6)

plagueAndPestilence :: SideSchemeCard AoaFourHorsemenSideScheme
plagueAndPestilence = sideScheme (\a -> AoaFourHorsemenSideScheme (a {sideSchemeCrisis = True} `With` Nothing)) Cards.plagueAndPestilence (Static 6)

theSpecterOfDeath :: SideSchemeCard AoaFourHorsemenSideScheme
theSpecterOfDeath = sideScheme (AoaFourHorsemenSideScheme . (`With` Nothing)) Cards.theSpecterOfDeath (Static 6)

instance IsSideScheme AoaFourHorsemenSideScheme where
  toSideSchemeAttrs (AoaFourHorsemenSideScheme (attrs `With` _)) = attrs

instance HasCardCode AoaFourHorsemenSideScheme where
  toCardCode (AoaFourHorsemenSideScheme (attrs `With` _)) = toCardCode attrs

instance IsRef AoaFourHorsemenSideScheme where
  toRef (AoaFourHorsemenSideScheme (attrs `With` _)) = toRef attrs
instance RunMessage AoaFourHorsemenSideScheme where
  runMessage msg s@(AoaFourHorsemenSideScheme (attrs `With` defeatedBy)) = runQueueT $ case msg of
    SideSchemeMessage sid (SideSchemeThwarted source _) | sid == sideSchemeId attrs -> do
      you <- maybe getActivePlayerId pure =<< getSourceController source
      attrs' <- runMessage msg attrs
      pure $ AoaFourHorsemenSideScheme $ attrs' `With` Just you
    SideSchemeMessage sid DefeatSideScheme | sid == sideSchemeId attrs -> do
      you <- maybe getActivePlayerId pure defeatedBy
      case toCardCode attrs of
        "45086" -> do
          upgrades <- selectList $ UpgradeControlledBy $ IdentityWithId you
          supports <- selectList $ SupportControlledBy $ IdentityWithId you
          chooseAndDiscardControlled you $ map UpgradeRef upgrades <> map SupportRef supports
        "45087" -> discardFromDeck you 10 attrs
        "45088" -> createEffect MainSchemes.theHorsemenOfApocalypse attrs $ IdentityEntity $ IdentityWithId you
        "45089" -> do
          characters <- selectList $ CharacterControlledBy $ IdentityWithId you
          damageCharacters attrs (toDamage 1 FromAbility) characters
        _ -> pure ()
      pure s
    _ -> AoaFourHorsemenSideScheme . (`With` defeatedBy) <$> runMessage msg attrs
