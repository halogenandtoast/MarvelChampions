module Marvel.Attachment.Attachments.SmSinisterSix where

import Marvel.Prelude
import Marvel.Ability.Types
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Villain.Villains.SmSinisterSix

newtype SmSinisterSixAttachment = SmSinisterSixAttachment (Attrs Attachment)
  deriving anyclass (IsAttachment, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

smHeightenedMorale, smTauntingPresence, smTeamLeader, smTakeOneForTheTeam :: AttachmentCard SmSinisterSixAttachment
smHeightenedMorale = attachment SmSinisterSixAttachment Cards.smHeightenedMorale
smTauntingPresence = attachment SmSinisterSixAttachment Cards.smTauntingPresence
smTeamLeader = attachment SmSinisterSixAttachment Cards.smTeamLeader
smTakeOneForTheTeam = attachment SmSinisterSixAttachment Cards.smTakeOneForTheTeam

instance HasModifiersFor SmSinisterSixAttachment where
  getModifiersFor _ target (SmSinisterSixAttachment a)
    | attached a = case (toCardCode a, target) of
        ("27103", VillainRef vid) | attachmentEnemy a == Just (EnemyVillainId vid) ->
          pure . pure . AttackModifier . fromIntegral . length =<< selectList AnyVillain
        ("27104", SideSchemeRef _) -> do
          def <- getsGame (`abilityCardDef` target)
          -- Official FFG ruling #158 (2022-09-29): the defeated villain's own
          -- threat-removal effect may clear Light at the End despite this card.
          alive <- maybe (pure False) (\case EnemyVillainId vid -> (> 0) <$> getVillainHitPoints vid; _ -> pure False) (attachmentEnemy a)
          pure [ThreatCannotBeRemoved | alive, maybe False ((== "27102") . toBaseCardCode . cdCardCode) def]
        ("27105", AttachmentRef aid) | aid == attachmentId a -> pure [CrisisIcon]
        ("27106", VillainRef vid) -> do
          protected <- selectAny (EnemyWithId (EnemyVillainId vid) <> EnemyWithAttachment (AttachmentIs Cards.smTakeOneForTheTeam))
          pure [CannotBeAttacked | not protected]
        _ -> pure []
  getModifiersFor _ _ _ = pure []

instance RunMessage SmSinisterSixAttachment where
  runMessage msg e@(SmSinisterSixAttachment attrs) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment _) | aid == attachmentId attrs -> do
      present <- sixVillains
      if null present
        then ambushThen attrs "ambushAttach" ()
        else do
          scored <- for present \(vid, code) -> do
            score <- case toCardCode attrs of
              "27103" -> pure $ fromIntegral $ activationOrder code
              "27105" -> pure $ fromIntegral $ 7 - activationOrder code
              "27104" -> getVillainHitPoints vid
              _ -> getVillainAttackValue vid
            pure (vid, score)
          let best = maximum $ map snd scored
              targets = [vid | (vid, score) <- scored, score == best]
          first <- selectJust FirstPlayer
          chooseOrRunOne first [targeting (VillainRef vid) (attachTo aid vid) | vid <- targets]
      pure e
    CardSpecific (isTarget attrs -> True) "ambushAttach" (fromJSON -> Success (entered, _ :: Value)) -> do
      case entered :: Maybe VillainId of
        Nothing -> removeFromPlay attrs
        Just vid -> attachTo (attachmentId attrs) vid
      pure e
    _ -> SmSinisterSixAttachment <$> runMessage msg attrs
