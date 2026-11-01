module Marvel.SideScheme.SideSchemes.RulerOfTheAstralPlane where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question ()
import Marvel.Ref
import Marvel.Status ()
import Marvel.Window qualified as W ()
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.GameValue
import Marvel.Trait qualified as Trait
import Marvel.Attachment.Cards qualified as Attachments

rulerOfTheAstralPlane :: SideSchemeCard RulerOfTheAstralPlane
rulerOfTheAstralPlane = sideScheme RulerOfTheAstralPlane Cards.rulerOfTheAstralPlane (Static 6)

newtype RulerOfTheAstralPlane = RulerOfTheAstralPlane (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage RulerOfTheAstralPlane where
  runMessage msg s@(RulerOfTheAstralPlane attrs) = runQueueT $ case msg of
    SideSchemeMessage sid DefeatSideScheme | sid == sideSchemeId attrs -> do
      you <- getDefeatedBy attrs
      possessed <- selectList (AttachmentIs Attachments.possessed)
      unless (null possessed) $ chooseOrRunOne you
        [targeting aid (removeFromPlay aid) | aid <- possessed]
      pure s
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      you <- getBoostedPlayer enemy
      controlled <- selectAny (MinionWithTrait Trait.Controlled <> MinionEngagedWith (IdentityWithId you))
      when controlled $ revealBoost attrs you
      pure s
    _ -> RulerOfTheAstralPlane <$> runMessage msg attrs
