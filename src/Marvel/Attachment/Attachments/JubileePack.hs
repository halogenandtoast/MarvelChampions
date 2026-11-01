module Marvel.Attachment.Attachments.JubileePack where
import Marvel.Prelude
import Marvel.Ability
import Marvel.Attachment.Attachments.AllyAsMinion
import Marvel.Attachment.Types
import Marvel.Card
import Marvel.Entity ()
import Marvel.Game.Source
import Marvel.Id
import Marvel.Jubilee.Cards qualified as Cards
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Trait

newtype JubileeAttachment = JubileeAttachment (Attrs Attachment)
  deriving anyclass (IsAttachment, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
jubilee47026, jubilee47027 :: AttachmentCard JubileeAttachment
jubilee47026 = attachment JubileeAttachment Cards.jubilee47026
jubilee47027 = attachment JubileeAttachment Cards.jubilee47027

instance HasModifiersFor JubileeAttachment where
  getBlankingModifiersFor _ (MinionRef mid) (JubileeAttachment attrs) =
    pure [Blank | toCardCode attrs == "47027", attachedAllyMinion attrs == Just mid]
  getBlankingModifiersFor _ _ _ = pure []
  getModifiersFor _ target (JubileeAttachment attrs) = case toCardCode attrs of
    "47027" -> do
      traits <- allyMinionTrait Regressed attrs target
      pure $ traits <> [SchemePenalty 1 | case target of MinionRef mid -> attachedAllyMinion attrs == Just mid; _ -> False] <> [Blank | case target of MinionRef mid -> attachedAllyMinion attrs == Just mid; _ -> False]
    "47026" -> pure [modifier | attachmentEnemy attrs == targetEnemy target, isJust (attachmentEnemy attrs), modifier <- [AttackModifier 1, HitPointModifier 3, TraitModifier Aerial]]
    _ -> pure []
   where
    targetEnemy (MinionRef mid) = Just (EnemyMinionId mid)
    targetEnemy _ = Nothing

instance RunMessage JubileeAttachment where
  runMessage msg a@(JubileeAttachment attrs) = runQueueT $ case msg of
    AttachmentMessage ident (RevealAttachment you) | ident == attachmentId attrs -> do
      case toCardCode attrs of
        "47026" -> do
          minions <- filterM (fmap (\ms -> CannotHaveAttachedCards `notElem` ms && CannotHaveAttachedEncounterCards `notElem` ms) . getModifiers . MinionRef) =<< selectList AnyMinion
          valued <- for minions $ \mid -> (mid,) <$> getEnemyRemainingHitPoints (EnemyMinionId mid)
          if null valued then surge you >> removeFromPlay attrs
          else do
            let least = minimum (map snd valued)
            chooseOrRunOne you [targeting (MinionRef mid) $ attachTo ident (EnemyMinionId mid) | (mid,hp) <- valued, hp == least]
        "47027" -> do
          allies <- filterM attachable =<< selectList AnyAlly
          valued <- for allies $ \aid -> (aid,) . fromMaybe 0 . (cdCost =<<) <$> getsGame (`abilityCardDef` AllyRef aid)
          if null valued then surge you >> removeFromPlay attrs
          else do
            let greatest = maximum (map snd valued)
            chooseOrRunOne you [targeting (AllyRef aid) do
              let mid = MinionId $ unAllyId aid
              allyBecomesMinion aid
              attachTo ident (EnemyMinionId mid)
              minionStep mid MinionCheckHitPoints
              | (aid,cost) <- valued, cost == greatest]
        _ -> pure ()
      pure a
    _ -> do
      when (toCardCode attrs == "47027") $ revertsToAlly attrs msg
      JubileeAttachment <$> runMessage msg attrs
   where
    attachable aid = do
      modifiers <- getModifiers (AllyRef aid)
      pure $ CannotHaveAttachedCards `notElem` modifiers && CannotHaveAttachedEncounterCards `notElem` modifiers
