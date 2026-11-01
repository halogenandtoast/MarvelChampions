module Marvel.Attachment.Attachments.AgeOfApocalypseUnus where

import Marvel.Prelude
import Marvel.Card
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Ref
import Marvel.Ability
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Attachment.Attachments.Thanos (discardAfterBasicAttack)
import Marvel.Cost
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Modifier
import Marvel.Query
import Marvel.Question (runAbility)
import Marvel.Resource
import Marvel.SideScheme.SideSchemes.AgeOfApocalypseInfinites (placeGenePoolThreat)
import Marvel.Trait (Trait (Infinite))
import Marvel.Window qualified as W

newtype AoaUnusAttachment = AoaUnusAttachment (Attrs Attachment `With` Bool)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsAttachment AoaUnusAttachment where
  toAttachmentAttrs (AoaUnusAttachment (a `With` _)) = a

prelateSidearm, prelateArmor, geneticExperiments :: AttachmentCard AoaUnusAttachment
prelateSidearm = attachment (AoaUnusAttachment . (`With` False)) Cards.prelateSidearm
prelateArmor = attachment (AoaUnusAttachment . (`With` False)) Cards.prelateArmor
geneticExperiments = attachment (AoaUnusAttachment . (`With` False)) Cards.geneticExperiments

instance HasModifiersFor AoaUnusAttachment where
  getModifiersFor _ target (AoaUnusAttachment (a `With` _))
    | Just enemy <- attachmentEnemy a, target == enemyRef enemy = pure $ case toCardCode a of
        "45063" -> [AttackModifier 1]
        "45064" -> [SchemeModifier 1]
        _ -> [AttackModifier 1, SchemeModifier 1, HitPointModifier 2]
  getModifiersFor _ _ _ = pure []

instance HasAbilities AoaUnusAttachment where
  getAbilities (AoaUnusAttachment (a `With` defeated)) = case attachmentEnemy a of
    Nothing -> []
    Just enemy -> case toCardCode a of
      "45063" -> discardAfterBasicAttack a [Energy, Physical]
        <> [windowAbility a 2 (W.EnemyAttacked W.After (EnemyWithId enemy) AnyIdentity) ForcedResponse NoCost (runAbility a 2) | defeated]
      "45064" -> discardAfterBasicAttack a [Mental, Physical]
        <> [windowAbility a 2 (W.VillainSchemed W.After $ VillainWithId vid) ForcedResponse NoCost (runAbility a 2) | EnemyVillainId vid <- [enemy]]
      _ -> [windowAbility a 2 (W.MinionDefeated W.When $ MinionWithId mid) ForcedInterrupt NoCost (runAbility a 2) | EnemyMinionId mid <- [enemy]]

instance RunMessage AoaUnusAttachment where
  runMessage msg e@(AoaUnusAttachment (a `With` defeated)) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId a -> do
      if toCardCode a == "45066" then do
        minions <- selectList (MinionWithTrait Infinite)
        if null minions then surge you >> removeFromPlay a else chooseOrRunOne you [targeting (enemyRef $ EnemyMinionId mid) (attachTo aid mid) | mid <- minions]
      else villainNamed "Unus" >>= traverse_ (attachTo aid)
      pure e
    Boost (RevealedAsBoost target _) | isTarget a target, toCardCode a == "45066" -> do
      you <- getYou
      minions <- selectList (MinionWithTrait Infinite)
      unless (null minions) $ chooseOrRunOne you [targeting (enemyRef $ EnemyMinionId mid) (do
        card <- getBoostCard (toTarget a)
        for_ card $ \c -> consumeBoostCard c >> attachEncounterCard c mid) | mid <- minions]
      pure e
    CheckWindows windows | toCardCode a == "45063" -> do
      let killed = any (\case W.Window W.When (W.DefeatedAlly _ enemy) -> enemy == attachmentEnemy a; _ -> False) windows
          beginning = any (\case W.Window W.When (W.EnemyAttack enemy _) -> Just enemy == attachmentEnemy a; W.Window W.When (W.EnemyAttackAlly enemy _ _) -> Just enemy == attachmentEnemy a; _ -> False) windows
      a' <- runMessage msg a
      pure $ AoaUnusAttachment (a' `With` (killed || (defeated && not beginning)))
    RanAbility _ (isTarget a -> True) 2 _ _ -> do
      case toCardCode a of
        "45063" -> placeGenePoolThreat 1
        "45064" -> traverse_ enemyBecomesTough (attachmentEnemy a)
        _ -> placeGenePoolThreat 2
      pure $ AoaUnusAttachment (a `With` False)
    _ -> (\a' -> AoaUnusAttachment (a' `With` defeated)) <$> runMessage msg a
