module Marvel.SideScheme.SideSchemes.RenderMedicalAid where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question ()
import Marvel.Ref
import Marvel.Window qualified as W ()
import Marvel.GameValue
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

renderMedicalAid :: SideSchemeCard RenderMedicalAid
renderMedicalAid = sideScheme RenderMedicalAid Cards.renderMedicalAid (PerPlayer 3)
newtype RenderMedicalAid = RenderMedicalAid (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage RenderMedicalAid where
  runMessage msg a@(RenderMedicalAid attrs) = runQueueT $ case msg of
    SideSchemeMessage sid DefeatSideScheme | sid == sideSchemeId attrs -> do
      players <- getPlayers
      for_ players $ \you -> cardSpecific attrs "heal" (you, 5 :: Natural)
      pure a
    CardSpecific (isTarget attrs -> True) "heal" (fromJSON -> Success (you :: IdentityId, remaining :: Natural)) -> do
      when (remaining > 0) do
        characters <- selectList (CharacterControlledBy (IdentityWithId you) <> CharacterWithAnyDamage <> CharacterCanBeHealed)
        unless (null characters) $ chooseOne you
          [targeting (characterRef character) (heal character 1 >> cardSpecific attrs "heal" (you, remaining - 1)) | character <- characters]
      pure a
    _ -> RenderMedicalAid <$> runMessage msg attrs
