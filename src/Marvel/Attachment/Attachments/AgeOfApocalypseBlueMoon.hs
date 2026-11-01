module Marvel.Attachment.Attachments.AgeOfApocalypseBlueMoon where

import Marvel.Prelude
import Marvel.Keyword ()
import Marvel.Criteria ()
import Marvel.Ability
import Marvel.AgeOfApocalypse.Settings
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card
import Marvel.Entity
import Marvel.Cost
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W

newtype AoaBlueMoonAttachment = AoaBlueMoonAttachment (Attrs Attachment)
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

imperialGuardsman :: AttachmentCard AoaBlueMoonAttachment
imperialGuardsman = attachment AoaBlueMoonAttachment Cards.imperialGuardsman

instance HasModifiersFor AoaBlueMoonAttachment where
  getModifiersFor _ target (AoaBlueMoonAttachment attrs)
    | Just enemy <- attachmentEnemy attrs, target == enemyRef enemy = pure [HitPointModifier 4, TraitModifier Trait.ImperialGuard]
  getModifiersFor _ _ _ = pure []

instance HasAbilities AoaBlueMoonAttachment where
  getAbilities a@(AoaBlueMoonAttachment attrs) =
    [windowAbility a 1 (W.MinionDefeated W.When $ MinionWithId mid) ForcedInterrupt NoCost (runAbility a 1)
      | Just (EnemyMinionId mid) <- [attachmentEnemy attrs]]

instance RunMessage AoaBlueMoonAttachment where
  runMessage msg e@(AoaBlueMoonAttachment attrs) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId attrs -> do
      minions <- selectList AnyMinion
      if null minions then surge you >> removeFromPlay attrs
      else chooseOne you [targeting mid (attachTo aid mid) | mid <- minions]
      pure e
    RanAbility you (isTarget attrs -> True) 1 windows _ -> do
      controller <- case [source | W.DefeatedMinion _ _ source _ <- windows] of
        source : _ -> getSourceController source
        [] -> pure Nothing
      resolveSettingSpecial (fromMaybe you controller)
      pure e
    _ -> AoaBlueMoonAttachment <$> runMessage msg attrs
