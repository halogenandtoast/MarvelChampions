module Marvel.Attachment.Attachments.MutantGenesis where

import Marvel.Id
import Marvel.Prelude
import Marvel.Entity
import Marvel.Card
import Marvel.GameValue
import Marvel.Message.Lifted
import Marvel.Ref
import Marvel.Attachment.Types
import Marvel.Ability
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Query
import Marvel.Projection
import Marvel.Modifier
import Marvel.Question
import Marvel.Resource
import Marvel.Status
import Marvel.Keyword
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Minion.Types qualified as M
import Marvel.Attachment.Cards qualified as Cards

newtype MutantGenesisAttachment = MutantGenesisAttachment (Attrs Attachment)
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

mgAdamantiumClaws :: AttachmentCard MutantGenesisAttachment
mgAdamantiumClaws = attachment MutantGenesisAttachment Cards.mgAdamantiumClaws

mgAnimalFerocity :: AttachmentCard MutantGenesisAttachment
mgAnimalFerocity = attachment MutantGenesisAttachment Cards.mgAnimalFerocity

mgHomoSuperior :: AttachmentCard MutantGenesisAttachment
mgHomoSuperior = attachment MutantGenesisAttachment Cards.mgHomoSuperior

mgGauntletBeam :: AttachmentCard MutantGenesisAttachment
mgGauntletBeam = attachment MutantGenesisAttachment Cards.mgGauntletBeam

mgLearningAI :: AttachmentCard MutantGenesisAttachment
mgLearningAI = attachment MutantGenesisAttachment Cards.mgLearningAI

mgAdaptiveArmor :: AttachmentCard MutantGenesisAttachment
mgAdaptiveArmor = attachment MutantGenesisAttachment Cards.mgAdaptiveArmor

mgEnergyBarrier :: AttachmentCard MutantGenesisAttachment
mgEnergyBarrier = attachment MutantGenesisAttachment Cards.mgEnergyBarrier

mgTargetedForElimination :: AttachmentCard MutantGenesisAttachment
mgTargetedForElimination = attachment MutantGenesisAttachment Cards.mgTargetedForElimination

mgUnitUpgrade :: AttachmentCard MutantGenesisAttachment
mgUnitUpgrade = attachment MutantGenesisAttachment Cards.mgUnitUpgrade

mgStunBeam :: AttachmentCard MutantGenesisAttachment
mgStunBeam = attachment MutantGenesisAttachment Cards.mgStunBeam

mgPhysicalStrain :: AttachmentCard MutantGenesisAttachment
mgPhysicalStrain = attachment MutantGenesisAttachment Cards.mgPhysicalStrain

mgMagnetosHelmet :: AttachmentCard MutantGenesisAttachment
mgMagnetosHelmet = attachment MutantGenesisAttachment Cards.mgMagnetosHelmet

mgMagnetosArmor :: AttachmentCard MutantGenesisAttachment
mgMagnetosArmor = attachment MutantGenesisAttachment Cards.mgMagnetosArmor

mgMagneticBubble :: AttachmentCard MutantGenesisAttachment
mgMagneticBubble = attachment MutantGenesisAttachment Cards.mgMagneticBubble

mgWrappedInMetal :: AttachmentCard MutantGenesisAttachment
mgWrappedInMetal = attachment MutantGenesisAttachment Cards.mgWrappedInMetal

mgNanoSentinelTech :: AttachmentCard MutantGenesisAttachment
mgNanoSentinelTech = attachment MutantGenesisAttachment Cards.mgNanoSentinelTech

instance HasModifiersFor MutantGenesisAttachment where
  getModifiersFor _ (VillainRef vid) (MutantGenesisAttachment a)
    | Just enemy <- attachmentEnemy a, toCardCode a `elem` ["32094", "32095", "32096"] = do
        recipient <- case enemy of
          EnemyVillainId attached -> pure $ Just attached
          EnemyMinionId _ -> selectOne ActiveVillain
        let stat = case toCardCode a of
              "32094" -> [AttackModifier 1]
              "32095" -> [SchemeModifier 1]
              _ -> []
            text = case toCardCode a of
              "32094" -> [KeywordModifier Piercing, KeywordModifier Ranged]
              "32095" -> [KeywordModifier $ Retaliate 1]
              _ -> [HitPointModifier 8]
        pure $ [m | enemy == EnemyVillainId vid, m <- stat] <> [m | recipient == Just vid, m <- text]
  getModifiersFor _ target (MutantGenesisAttachment a)
    | Just enemy <- attachmentEnemy a, target == enemyRef enemy = pure $ case toCardCode a of
        "32067" -> [AttackModifier 1, KeywordModifier Piercing]
        "32068" -> [KeywordModifier Stalwart]
        "32077" -> [HitPointModifier 5]
        "32094" -> [AttackModifier 1]
        "32095" -> [SchemeModifier 1]
        "32096" -> []
        "32103" -> [AttackModifier 2]
        "32115" -> [AttackModifier 1, SchemeModifier 1, HitPointModifier 2, KeywordModifier $ Retaliate 1]
        "32116" -> [AttackModifier 1]
        "32145b" -> [LoseKeyword Steady]
        "32147" -> [SchemeModifier 1, CannotBeConfused]
        "32148" -> [AttackModifier 1, CannotBeStunned]
        "32149" -> [KeywordModifier $ Retaliate 1]
        "32170" -> [HitPointModifier 4, TraitModifier Trait.Sentinel]
        _ -> []
    | IdentityRef you <- target, attachmentIdentity a == Just you = case toCardCode a of
        "32107" -> do
          sentinel <- selectAny (MinionWithTrait Trait.Sentinel <> MinionEngagedWith (IdentityWithId you))
          hero <- selectAny (IdentityWithId you <> HeroIdentity)
          pure [CannotChangeForm | sentinel && hero]
        "32150" -> pure [CannotAttack, CannotThwart, CannotDefend, CannotRecover]
        _ -> pure []
  getModifiersFor _ _ _ = pure []

instance HasAbilities MutantGenesisAttachment where
  getAbilities (MutantGenesisAttachment a) =
    [ability a 1 HeroAction NoCriteria (MultiResourceCost $ map Just resources) (DiscardTarget $ toRef a) | not (null resources)]
    <> [ability a 1 Action (SelfMatches $ IdentityWithId you) (ExhaustCharacterCost (IdentityCharacters You) <> extraCost) (DiscardTarget $ toRef a) | Just you <- [attachmentIdentity a], code `elem` ["32107", "32150"]]
    <> [windowAbility a 2 (W.EnemyAttacked W.After (EnemyWithId enemy) AnyIdentity) ForcedResponse NoCost (runAbility a 2) | Just enemy <- [attachmentEnemy a], code == "32103"]
    <> [windowAbility a 3 (W.EnemyAttackedAndDamaged (EnemyWithId enemy) AnyCharacter) ForcedResponse NoCost (runAbility a 3) | Just enemy <- [attachmentEnemy a], code == "32116"]
    <> [windowAbility a 4 (W.MakesBasicAttackAgainst W.After You (EnemyWithId enemy)) HeroResponse (MultiResourceCost $ map Just [Energy, Mental, Physical]) (DiscardTarget $ toRef a) | Just enemy <- [attachmentEnemy a], code `elem` ["32147", "32148"]]
    <> [windowAbility a 5 (W.VillainWouldTakeDamage (VillainWithId vid) AnyDamage) ForcedInterrupt NoCost (runAbility a 5) | Just (EnemyVillainId vid) <- [attachmentEnemy a], code == "32149"]
   where
    code = toCardCode a
    resources = case code of
      "32067" -> [Energy, Mental, Physical]
      "32068" -> [Energy, Mental, Physical]
      "32094" -> replicate 3 Physical
      "32095" -> replicate 3 Mental
      "32096" -> replicate 3 Energy
      _ -> []
    extraCost = if code == "32150" then ResourceCost (Just Physical) else NoCost

instance RunMessage MutantGenesisAttachment where
  runMessage msg e@(MutantGenesisAttachment attrs) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId attrs -> do
      case toCardCode attrs of
        code | code `elem` ["32067", "32068", "32094", "32095", "32096", "32145b", "32147", "32148", "32149"] -> attachToTheVillain attrs
        "32077" -> attachMinion you AnyMinion True
        "32103" -> attachMinion you (MinionWithTrait Trait.Sentinel) True
        "32115" -> attachMinion you (MinionWithTrait Trait.Sentinel) False
        "32116" -> attachMinion you (MinionWithTrait Trait.Sentinel) False
        "32107" -> do
          marked <- any (== Just you) <$> (selectList (AttachmentIs Cards.mgTargetedForElimination) >>= traverse (getAttachmentIdentity))
          if marked then removeFromPlay attrs >> surge you else attachToIdentity aid you
        "32150" -> do
          marked <- any (== Just you) <$> (selectList (AttachmentIs Cards.mgWrappedInMetal) >>= traverse (getAttachmentIdentity))
          if marked then removeFromPlay attrs else attachToIdentity aid you
        "32170" -> do
          nemeses <- getNemesisMinionsOutOfPlay you
          if null nemeses then removeFromPlay attrs
            else chooseOrNothing you [targeting card do
              putEncounterCardIntoPlay you card
              cardSpecific attrs "attachNemesis" (MinionId $ unCardId $ ecCardId card) | card <- nemeses]
          shuffleEncounterDeck
        _ -> pure ()
      pure e
    CardSpecific (isTarget attrs -> True) "attachNemesis" (fromJSON -> Success mid) -> e <$ attachTo (attachmentId attrs) (mid :: MinionId)
    RanAbility _ (isTarget attrs -> True) 2 _ _ -> do
      for_ (attachmentEnemy attrs) (\enemy -> giveStatus (enemyCharacter enemy) ToughStatus)
      pure e
    RanAbility _ (isTarget attrs -> True) 3 [W.EnemyAttacksAndDamages _ character _] _ -> e <$ giveStatus character StunnedStatus
    RanAbility _ (isTarget attrs -> True) 5 windows _ -> do
      for_ [(vid, damageAmount dmg) | W.DamagedVillain vid dmg <- windows] \(vid,n) -> do
        insteadOf (\case VillainMessage v (VillainDamaged _ _) -> vid == v; _ -> False) (damageAttachment (attachmentId attrs) n)
      pure e
    AttachmentMessage aid (AttachmentDamaged n) | aid == attachmentId attrs, toCardCode attrs == "32149" -> do
      when (attachmentDamage attrs + n >= 8) $ removeFromPlay attrs
      pure $ MutantGenesisAttachment attrs {attachmentDamage = attachmentDamage attrs + n}
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      you <- getBoostedPlayer enemy
      case toCardCode attrs of
        code | code `elem` ["32067", "32068", "32095", "32096"] -> revealBoost attrs you
        "32077" -> boostMinion you AnyMinion True
        "32094" -> exhaust you
        "32115" -> boostMinion you (MinionWithTrait Trait.Sentinel) False
        _ -> pure ()
      pure e
    _ -> MutantGenesisAttachment <$> runMessage msg attrs
   where
    boostMinion you matcher tough = do
      minions <- selectList matcher
      card <- getBoostCard (toTarget attrs)
      for_ card (\c -> chooseOrNothing you [targeting mid do
        consumeBoostCard c
        cardSpecific GameRef "mgAttachTopSentinel" (mid, c)
        when tough $ giveStatus mid ToughStatus
        | mid <- minions])
    enemyCharacter = \case EnemyVillainId v -> VillainCharacter v; EnemyMinionId m -> MinionCharacter m
    attachMinion you matcher tough = do
      minions <- selectList matcher
      valid <- filterM (\mid -> do
        existing <- selectAny (AttachmentIs (getCardDef attrs) <> AttachmentOnEnemy (EnemyMinionId mid))
        pure $ not existing || toCardCode attrs `notElem` ["32103", "32116"]) minions
      if null valid then removeFromPlay attrs >> surge you
        else chooseOrNothing you [targeting m do
          attachTo (attachmentId attrs) m
          when tough (giveStatus m ToughStatus) | m <- valid]

chooseOrNothing :: (ReverseQueue m) => IdentityId -> [m Choice] -> m ()
chooseOrNothing you choices = unless (null choices) $ chooseOrRunOne you choices
