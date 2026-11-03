module Marvel.CivilWar.Taskmaster where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Attachment.Types
import Marvel.Card
import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.Helpers
import Marvel.Cost
import Marvel.Criteria
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Keyword qualified as K
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Runner
import Marvel.Resource
import Marvel.SideScheme.Types
import Marvel.Treachery.Types

newtype Taskmaster = Taskmaster (Attrs Minion)
  deriving anyclass (IsMinion,HasModifiersFor,HasAbilities)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

taskmaster :: MinionCard Taskmaster
taskmaster = minion Taskmaster C.cw57022 (Sch 1) (Atk 1) (HP 6)

instance RunMessage Taskmaster where
  runMessage msg e@(Taskmaster a) = runQueueT $ case msg of
    MinionMessage mid (RevealMinion you) | mid == minionId a -> e <$ minionActivatesAgainst mid you
    _ -> Taskmaster <$> runMessage msg a

newtype TaskmasterEquipment = TaskmasterEquipment (Attrs Attachment)
  deriving anyclass IsAttachment
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

taskmasterEquipment :: [AttachmentCard TaskmasterEquipment]
taskmasterEquipment = map (attachment TaskmasterEquipment) [C.cw57023,C.cw57024]

instance HasModifiersFor TaskmasterEquipment where
  getModifiersFor _ target (TaskmasterEquipment a)
    | Just target == (enemyRef <$> attachmentEnemy a) || Just target == (AllyRef <$> attachmentAlly a) =
        pure $ if toCardCode a == "57023" then [AttackModifier 1,KeywordModifier K.Piercing] else [SchemeModifier 1,DamageReduction 1]
  getModifiersFor _ _ _ = pure []

instance HasAbilities TaskmasterEquipment where
  getAbilities (TaskmasterEquipment a) =
    [ability a 1 HeroAction NoCriteria (MultiResourceCost $ map Just [Mental,Physical]) (DiscardTarget $ toRef a)]
instance RunMessage TaskmasterEquipment where
  runMessage msg e@(TaskmasterEquipment a) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId a -> do
      minions <- asYou you $ selectList $ MinionIs C.cw57022
      allies <- asYou you $ selectList $ AllyIs C.cw57022
      eligibleMinions <- filterM (canAttach . toTarget) minions
      eligibleAllies <- filterM (canAttach . toTarget) allies
      choices <- if null eligibleMinions && null eligibleAllies then do
        leader <- enemyLeaderFor you
        eligible <- filterM (canAttach . toTarget) [vid | Just vid <- [leader]]
        pure [targeting (VillainRef vid) (attachTo aid vid) | vid <- eligible]
      else pure $ [targeting (MinionRef mid) (attachTo aid mid) | mid <- eligibleMinions]
        <> [targeting (AllyRef ally) (attachToAlly aid ally) | ally <- eligibleAllies]
      if null choices then removeFromPlay a else chooseOrRunOne you choices
      pure e
    _ -> TaskmasterEquipment <$> runMessage msg a
   where
    canAttach target = do
      modifiers <- getModifiers target
      pure $ all (`notElem` modifiers) [CannotHaveAttachedCards,CannotHaveAttachedEncounterCards]

newtype MimickedMove = MimickedMove (Attrs Treachery)
  deriving anyclass IsTreachery
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

mimickedMove :: TreacheryCard MimickedMove
mimickedMove = treachery MimickedMove C.cw57025

instance RunMessage MimickedMove where
  runMessage msg e@(MimickedMove a) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId a -> do
      taskmasters <- asYou you $ selectList $ MinionIs C.cw57022
      leader <- friendlyLeaderFor you
      let attacks = [(mid,vid) | mid <- taskmasters,Just vid <- [leader]]
      if null attacks then toughAndBoost you
      else do
        first <- asYou you $ selectJust FirstPlayer
        chooseOrRunOne you [targeting (enemyRef $ EnemyMinionId mid)
          (enemyActivationAfterCurrent mid $ minionAttacksEnemyAs mid first vid) | (mid,vid) <- attacks]
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget a target -> e <$ dealBoost (enemyRef enemy)
    _ -> MimickedMove <$> runMessage msg a

toughAndBoost :: ReverseQueue m => IdentityId -> m ()
toughAndBoost you = enemyLeaderFor you >>= traverse_ (\vid -> enemyBecomesTough vid >> dealBoost vid)

newtype TaskmastersAcademy = TaskmastersAcademy (Attrs SideScheme)
  deriving anyclass (IsSideScheme,HasModifiersFor)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

taskmastersAcademy :: SideSchemeCard TaskmastersAcademy
taskmastersAcademy = sideScheme TaskmastersAcademy C.cw57026 (Static 3)

instance RunMessage TaskmastersAcademy where
  runMessage msg (TaskmastersAcademy a) = runQueueT $ case msg of
    SideSchemeMessage sid DefeatSideScheme | sid == sideSchemeId a -> do
      leader <- getAreaOfTarget a >>= getActiveVillainIn
      for_ leader $ \vid -> enemyBecomesTough vid >> dealBoost vid
      TaskmastersAcademy <$> runMessage msg a
    _ -> TaskmastersAcademy <$> runMessage msg a
