module Marvel.FearNoEvil.Hammerhead where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Attachment.Types
import Marvel.Attack
import Marvel.Card
import Marvel.Cost
import Marvel.Damage
import Marvel.Criteria
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
import Marvel.Minion.Types
import Marvel.Minion.Runner ()
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.SideScheme.Types
import Marvel.Stats
import Marvel.Treachery.Types
import Marvel.Trait qualified as T
import Marvel.Villain.Types qualified as V
import Marvel.Window qualified as W

newtype Hammerhead = Hammerhead (Attrs V.Villain)
  deriving newtype (Show,Eq,ToJSON,FromJSON,IsRef)
  deriving anyclass (V.IsVillain,HasModifiersFor)
hammerheadStages :: [V.VillainCard Hammerhead]
hammerheadStages = [V.villainWith Hammerhead def (Sch sch) (Atk atk) (HP $ PerPlayer hp) (V.stageL .~ stage)
  | (def,sch,atk,hp,stage) <- [(Cards.street60086,2,1,14,1),(Cards.street60087,2,2,15,2),(Cards.street60088,3,3,16,3)]]
instance HasAbilities Hammerhead where
  getAbilities (Hammerhead a) = [windowAbility a 1 (W.EnemyAttackedAndDamaged (EnemyWithId $ V.toEnemyId a) AnyCharacter) ForcedResponse NoCost (runAbility a 1)]
instance RunMessage Hammerhead where
  runMessage msg e@(Hammerhead a) = runQueueT $ case msg of
    VillainMessage vid VillainDefeated | vid == V.villainId a -> do
      expert <- (== Expert) <$> getDifficulty
      case V.villainStage a of
        1 -> replaceVillain vid "60087" Advanced >> pure e
        2 | expert -> replaceVillain vid "60088" Advanced >> pure e
        _ -> Hammerhead <$> runMessage msg a
    RanAbility _ (isTarget a -> True) 1 windows _ -> do
      for_ [character | W.EnemyAttacksAndDamages enemy character _ <- windows,enemy == V.toEnemyId a] $ \character ->
        whenM (characterMatches AnyCharacter character) $ do
          already <- characterHasCondition StunnedStatus character
          giveStatus character StunnedStatus
          when already $ damageCharacter character a (toDamage (if V.villainStage a == 3 then 2 else 1) FromAbility)
      pure e
    _ -> Hammerhead <$> runMessage msg a

newtype HammerheadEquipment = HammerheadEquipment (Attrs Attachment)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)
  deriving anyclass IsAttachment
hammerheadEquipment :: [AttachmentCard HammerheadEquipment]
hammerheadEquipment = map (attachment HammerheadEquipment) [Cards.street60089,Cards.street60090]
instance HasModifiersFor HammerheadEquipment where
  getModifiersFor _ target (HammerheadEquipment a) = pure $ case attachmentEnemy a of
    Just enemy | target == enemyRef enemy -> if toCardCode a == "60089" then [AttackModifier 1] else [SchemeModifier 1,KeywordModifier Stalwart]
    _ -> []
instance HasAbilities HammerheadEquipment where
  getAbilities (HammerheadEquipment a) =
    [windowAbility a 1 (W.EnemyAttackedSurvivingCharacter $ EnemyWithId enemy) ForcedResponse NoCost (runAbility a 1) | Just enemy <- [attachmentEnemy a],toCardCode a == "60089"]
    <> [ability a 2 HeroAction NoCriteria (PrintedResourceCost [Energy,Mental,Physical]) (runAbility a 2) | toCardCode a == "60090",isJust $ attachmentEnemy a]
instance RunMessage HammerheadEquipment where
  runMessage msg e@(HammerheadEquipment a) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment _) | aid == attachmentId a -> do
      villainNamed "Hammerhead" >>= maybe (removeFromPlay a) (attachTo aid)
      pure e
    RanAbility you (isTarget a -> True) 1 _ _ -> do
      for_ (attachmentEnemy a) $ \enemy -> do
        current <- getEnemyAttack enemy
        for_ current $ \attack -> do
          let cost = DiscardCost $ toTarget a
          payable <- canAffordCost you (toSource a) cost
          when payable $ payCostThenWithPayment you a cost GameRef "repeatEnemyAttackAfterPayment" (enemy,attackCharacter attack)
      pure e
    RanAbility _ (isTarget a -> True) 2 _ _ -> removeFromPlay a >> pure e
    Boost (RevealedAsBoost target enemy) | isTarget a target, toCardCode a == "60090" -> giveStatus (toCharacter enemy) ToughStatus >> pure e
    _ -> HammerheadEquipment <$> runMessage msg a

newtype MaggiaMinion = MaggiaMinion (Attrs Minion)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)
  deriving anyclass IsMinion
maggiaMinions :: [MinionCard MaggiaMinion]
maggiaMinions = [minion MaggiaMinion def (Sch sch) (Atk atk) (HP hp) | (def,sch,atk,hp) <- [(Cards.street60091,1,1,5),(Cards.street60092,0,2,3),(Cards.street60093,1,1,4)]]
instance HasModifiersFor MaggiaMinion where
  getModifiersFor _ target (MaggiaMinion a)
    | target /= toRef a = pure []
    | toCardCode a == "60091" = do
        characters <- selectList AnyCharacter
        values <- traverse characterThwartValue characters
        pure [SchemeModifier $ maximum (0:values)]
    | toCardCode a == "60092" = do
        enemies <- selectList $ EnemyWithTrait T.Maggia
        pure [SchemeModifier $ fromIntegral $ length enemies]
    | otherwise = pure []
instance HasAbilities MaggiaMinion where
  getAbilities (MaggiaMinion a) = [windowAbility a 1 (W.EnemyActivates W.When (EnemyWithId $ toEnemyId a) StunnedIdentity) ForcedInterrupt NoCost (runAbility a 1) | toCardCode a == "60093"]
instance RunMessage MaggiaMinion where
  runMessage msg e@(MaggiaMinion a) = runQueueT $ case msg of
    MinionMessage mid (RevealMinion _) | mid == minionId a, toCardCode a == "60091" -> do
      updated <- runMessage msg a
      minionSchemes mid
      pure $ MaggiaMinion updated
    RanAbility _ (isTarget a -> True) 1 _ _ -> dealBoost (minionId a) >> pure e
    _ -> MaggiaMinion <$> runMessage msg a

newtype HammerheadScheme = HammerheadScheme (Attrs SideScheme)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)
  deriving anyclass HasModifiersFor
instance IsSideScheme HammerheadScheme where
  sideSchemeAbilities = getAbilities
hammerheadSchemes :: [SideSchemeCard HammerheadScheme]
hammerheadSchemes = [sideScheme HammerheadScheme Cards.street60094 (Static 6),sideScheme HammerheadScheme Cards.street60095 (Static 5)]
instance HasAbilities HammerheadScheme where
  getAbilities (HammerheadScheme a) = [windowAbility a 1 (W.MinionDefeated W.After AnyMinion) ForcedResponse NoCost (runAbility a 1) | toCardCode a == "60094"]
instance RunMessage HammerheadScheme where
  runMessage msg e@(HammerheadScheme a) = runQueueT $ case msg of
    RanAbility you (isTarget a -> True) 1 windows _ -> do
      for_ [mid | W.DefeatedMinion mid _ _ _ <- windows] $ \mid -> withTheVillain you $ giveDefeatedMinionAsBoost mid
      pure e
    SideSchemeMessage sid RevealSideScheme | sid == sideSchemeId a, toCardCode a == "60095" -> do
      you <- maybe getYou pure (sideSchemeRevealedBy a)
      minions <- selectList AnyMinion
      scored <- for minions $ \mid -> (mid,) <$> getEnemyRemainingHitPoints (EnemyMinionId mid)
      case scored of
        [] -> discardUntilAndReveal you (CardWithType MinionType)
        _ -> do
          let lowest = minimum $ map snd scored
          chooseOne you [targeting mid (defeatMinionBy a mid >> discardUntilAndReveal you (CardWithType MinionType)) | (mid,hp) <- scored,hp == lowest]
      pure e
    _ -> HammerheadScheme <$> runMessage msg a

newtype HeadFirst = HeadFirst (Attrs Treachery)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)
  deriving anyclass IsTreachery
headFirst :: TreacheryCard HeadFirst
headFirst = treachery HeadFirst Cards.street60096
instance RunMessage HeadFirst where
  runMessage msg e@(HeadFirst a) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId a -> do
      hero <- identityMatches HeroIdentity you
      if not hero then gainSurge a else do
        stunned <- characterHasCondition StunnedStatus (IdentityCharacter you)
        villainNamed "Hammerhead" >>= traverse_ (\vid -> villainStep vid $ VillainAttacksWithBonus you (if stunned then 2 else 0) [Overkill | stunned])
      pure e
    _ -> HeadFirst <$> runMessage msg a
