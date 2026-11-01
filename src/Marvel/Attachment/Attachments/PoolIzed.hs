module Marvel.Attachment.Attachments.PoolIzed where

import Marvel.Prelude
import Marvel.Ability (HasAbilities)
import Marvel.Card
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Query
import Marvel.Id
import Marvel.Attachment.Attachments.AllyAsMinion
import Marvel.Trait qualified as Trait
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types

poolIzed :: AttachmentCard PoolIzed
poolIzed = attachment PoolIzed Cards.poolIzed

newtype PoolIzed = PoolIzed (Attrs Attachment)
  deriving anyclass (IsAttachment, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor PoolIzed where
  getBlankingModifiersFor _ (MinionRef mid) (PoolIzed attrs) = pure [Blank | attachedAllyMinion attrs == Just mid]
  getBlankingModifiersFor _ _ _ = pure []
  getModifiersFor source target card@(PoolIzed attrs) = do
    traits <- allyMinionTrait Trait.Pool attrs target
    blanking <- getBlankingModifiersFor source target card
    pure $ traits <> blanking

instance RunMessage PoolIzed where
  runMessage msg a@(PoolIzed attrs) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId attrs -> do
      allies <- filterM (fmap not . (\ally -> characterHasAttachment (AllyCharacter ally) Cards.poolIzed)) =<< selectList AnyAlly
      ranked <- for allies $ \ally -> do
        def <- getsGame (`abilityCardDef` (AllyRef ally))
        pure (ally, fromMaybe 0 $ cdCost =<< def)
      let highest = maximum (0 : map snd ranked)
      eligible <- filterM (\(ally,_) -> do
        mods <- getModifiers (AllyRef ally)
        pure $ CannotHaveAttachedCards `notElem` mods && CannotHaveAttachedEncounterCards `notElem` mods)
        [(ally,cost) | (ally,cost) <- ranked, cost == highest]
      if null eligible then surge you >> removeFromPlay attrs else
        chooseOrRunOne you [targeting (AllyRef ally) $ do
          let mid = MinionId $ unAllyId ally
          allyBecomesMinion ally
          attachTo aid (EnemyMinionId mid)
          minionStep mid MinionCheckHitPoints
          | (ally,_) <- eligible]
      pure a
    _ -> do
      revertsToAlly attrs msg
      PoolIzed <$> runMessage msg attrs
