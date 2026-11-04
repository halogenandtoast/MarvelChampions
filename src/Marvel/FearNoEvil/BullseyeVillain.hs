module Marvel.FearNoEvil.BullseyeVillain where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Attachment.Types
import Marvel.Card
import Marvel.Cost
import Marvel.Damage (damageAmount,damageUnpreventable)
import Marvel.Difficulty
import Marvel.Entity
import Marvel.FearNoEvil.Cards qualified as Cards
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hp
import Marvel.Id
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Stats
import Marvel.Villain.Types qualified as V
import Marvel.Window qualified as W

newtype Bullseye = Bullseye (Attrs V.Villain)
  deriving newtype (Show,Eq,ToJSON,FromJSON,IsRef)
  deriving anyclass (V.IsVillain,HasModifiersFor)
bullseyeStages :: [V.VillainCard Bullseye]
bullseyeStages = [V.villainWith Bullseye def (Sch sch) (Atk atk) (HP $ PerPlayer hp) (V.stageL .~ stage)
  | (def,sch,atk,hp,stage) <- [(Cards.street60065,0,1,14,1),(Cards.street60066,1,2,16,2),(Cards.street60067,1,3,18,3)]]
instance HasAbilities Bullseye where
  getAbilities (Bullseye a) =
    [windowAbility a 1 (W.VillainRevealed W.When (VillainWithId $ V.villainId a) W.RevealedFromVillain) WhenRevealed NoCost (runAbility a 1)
    ,windowAbility a 2 (W.EnemyActivates W.When (EnemyWithId $ V.toEnemyId a) AnyIdentity) ForcedInterrupt NoCost (runAbility a 2)]
instance RunMessage Bullseye where
  runMessage msg e@(Bullseye a) = runQueueT $ case msg of
    VillainMessage vid VillainDefeated | vid == V.villainId a -> do
      expert <- (== Expert) <$> getDifficulty
      case V.villainStage a of
        1 -> replaceVillain vid "60066" Advanced >> pure e
        2 | expert -> replaceVillain vid "60067" Advanced >> pure e
        _ -> Bullseye <$> runMessage msg a
    RanAbility you (isTarget a -> True) 1 _ _ -> do
      case V.villainStage a of
        1 -> do
          cards <- findEncounterCards $ CardIs Cards.street60068a
          for_ (headMay cards) $ \card -> setAside [EncounterCard card]
        2 -> do
          cards <- findEncounterCards $ CardIs Cards.street60068a
          for_ (headMay cards) $ \card -> attachEncounterToCharacter card (VillainCharacter $ V.villainId a)
        _ -> searchEverywhereAndRevealEncounterCard you (CardIs Cards.street60073)
      pure e
    RanAbility _ (isTarget a -> True) 2 _ _ -> do
      untilEnemyActivationEnds a (V.villainId a) [AdditionalActivationBoostIcons 1]
      when (isJust $ V.villainAttacking a) $ attackGains (V.villainId a) Ranged
      pure e
    _ -> Bullseye <$> runMessage msg a

newtype BullseyeSpine = BullseyeSpine (Attrs Attachment)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)
  deriving anyclass (IsAttachment,HasModifiersFor)
bullseyeSpines :: [AttachmentCard BullseyeSpine]
bullseyeSpines = map (attachment BullseyeSpine) [Cards.street60068a,Cards.street60068b]
instance HasAbilities BullseyeSpine where
  getAbilities (BullseyeSpine a) = case attachmentEnemy a of
    Just enemy@(EnemyVillainId vid) ->
      [windowAbility a 1 (W.EnemyWouldTakeDamage $ EnemyWithId enemy) ForcedInterrupt NoCost (runAbility a 1) | toCardCode a == "60068a"]
      <> [windowAbility a 2 (W.VillainSchemed W.After $ VillainWithId vid) ForcedResponse NoCost (runAbility a 2) | toCardCode a == "60068b"]
    _ -> []
instance RunMessage BullseyeSpine where
  runMessage msg e@(BullseyeSpine a) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment _) | aid == attachmentId a -> do
      host <- villainNamed "Bullseye"
      for_ host $ attachTo aid
      pure e
    RanAbility _ (isTarget a -> True) 1 windows _ -> do
      for_ (attachmentEnemy a) $ \enemy ->
        for_ (headMay [damage | W.EnemyWouldBeDamaged target _ damage <- windows,target == enemy,not $ damageUnpreventable damage,damageAmount damage > 3]) $ \damage -> do
          let prevented = damageAmount damage - 3
          preventEnemyDamage enemy prevented
          damageAttachment (attachmentId a) prevented
      pure e
    AttachmentMessage aid (AttachmentDamaged n) | aid == attachmentId a -> do
      let total = attachmentDamage a + n
      pure $ BullseyeSpine a {attachmentDamage=total,attachmentCardDef=if toCardCode a == "60068a" && total >= 6 then Cards.street60068b else attachmentCardDef a}
    RanAbility _ (isTarget a -> True) 2 _ _ -> do
      let remaining = subtractNatural 3 (attachmentDamage a)
      pure $ BullseyeSpine a {attachmentDamage=remaining,attachmentCardDef=if remaining == 0 then Cards.street60068a else attachmentCardDef a}
    _ -> BullseyeSpine <$> runMessage msg a
