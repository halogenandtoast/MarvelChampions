{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.Attachment.Attachments.AgentsOfShield where

import Marvel.Prelude
import Marvel.Question (runAbility)
import Marvel.Ability
import Marvel.AgentsOfShield.Board
import Marvel.AgentsOfShield.EncounterCards qualified as Cards
import Marvel.AgentsOfShield.Helpers
import Marvel.Attachment.Types
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Hood.Helpers
import Marvel.Id
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Resource
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W

newtype AosAttachment = AosAttachment (Attrs Attachment `With` [PlayerCard])
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsAttachment AosAttachment where
  toAttachmentAttrs (AosAttachment (a `With` _)) = a

aos50068 :: AttachmentCard AosGauntlet
aos50068 = attachment (AosGauntlet . (`With` False)) Cards.aos50068

aos50069 :: AttachmentCard AosAttachment
aos50069 = attachment (AosAttachment . (`With` [])) Cards.aos50069

aos50070 :: AttachmentCard AosAttachment
aos50070 = attachment (AosAttachment . (`With` [])) Cards.aos50070

aos50071 :: AttachmentCard AosAttachment
aos50071 = attachment (AosAttachment . (`With` [])) Cards.aos50071

aos50092 :: AttachmentCard AosAttachment
aos50092 = attachment (AosAttachment . (`With` [])) Cards.aos50092

aos50114 :: AttachmentCard AosAttachment
aos50114 = attachment (AosAttachment . (`With` [])) Cards.aos50114

aos50115 :: AttachmentCard AosAttachment
aos50115 = attachment (AosAttachment . (`With` [])) Cards.aos50115

aos50116 :: AttachmentCard AosAttachment
aos50116 = attachment (AosAttachment . (`With` [])) Cards.aos50116

aos50117 :: AttachmentCard AosAttachment
aos50117 = attachment (AosAttachment . (`With` [])) Cards.aos50117

aos50118 :: AttachmentCard AosAttachment
aos50118 = attachment (AosAttachment . (`With` [])) Cards.aos50118

aos50119 :: AttachmentCard AosAttachment
aos50119 = attachment (AosAttachment . (`With` [])) Cards.aos50119

aos50132 :: AttachmentCard AosWeapon
aos50132 = attachment (AosWeapon . (`With` False)) Cards.aos50132

aos50144 :: AttachmentCard AosAttachment
aos50144 = attachment (AosAttachment . (`With` [])) Cards.aos50144

aos50145 :: AttachmentCard AosAttachment
aos50145 = attachment (AosAttachment . (`With` [])) Cards.aos50145

aos50149 :: AttachmentCard AosAttachment
aos50149 = attachment (AosAttachment . (`With` [])) Cards.aos50149

aos50153 :: AttachmentCard AosAttachment
aos50153 = attachment (AosAttachment . (`With` [])) Cards.aos50153

aos50157 :: AttachmentCard AosWeapon
aos50157 = attachment (AosWeapon . (`With` False)) Cards.aos50157

aos50158 :: AttachmentCard AosAttachment
aos50158 = attachment (AosAttachment . (`With` [])) Cards.aos50158

aos50170 :: AttachmentCard AosAttachment
aos50170 = attachment (AosAttachment . (`With` [])) Cards.aos50170

aos50171 :: AttachmentCard AosAttachment
aos50171 = attachment (AosAttachment . (`With` [])) Cards.aos50171

aos50181b :: AttachmentCard AosAttachment
aos50181b = attachment (AosAttachment . (`With` [])) Cards.aos50181b

aos50182b :: AttachmentCard AosAttachment
aos50182b = attachment (AosAttachment . (`With` [])) Cards.aos50182b

aos50183b :: AttachmentCard AosAttachment
aos50183b = attachment (AosAttachment . (`With` [])) Cards.aos50183b

instance HasModifiersFor AosAttachment where
  getModifiersFor _ target (AosAttachment (a `With` held)) = do
    let enemyTarget = maybe False ((==target) . enemyRef) $ attachmentEnemy a
        characterTarget = maybe False ((==target) . IdentityRef) (attachmentIdentity a) || maybe False ((==target) . AllyRef) (attachmentAlly a)
    if enemyTarget then pure $ case toCardCode a of
      "50068" -> [AttackModifier 1,KeywordModifier $ Retaliate 1]
      "50070" -> [SchemeModifier 1]
      "50114" -> [AttackModifier 1,HitPointModifier 5]
      "50115" -> [AttackModifier 1,SchemeModifier 1]
      "50117" -> [KeywordModifier Stalwart]
      "50118" -> [AttackModifier 1,KeywordModifier Piercing]
      "50119" -> let n = sum [fromIntegral $ max 0 $ fromMaybe 0 $ cdCost $ getCardDef c | c <- held] in [AttackModifier n,SchemeModifier n]
      "50132" -> [AttackModifier 1,SchemeModifier 1]
      "50144" -> [LoseKeyword Stalwart]
      "50157" -> [AttackModifier 1,KeywordModifier Overkill,KeywordModifier Ranged]
      "50170" -> [AttackModifier 2]
      "50181b" -> [AttackModifier 1]
      "50182b" -> [SchemeModifier 1]
      "50183b" -> [AttackModifier 1]
      _ -> []
    else if characterTarget then case toCardCode a of
      "50071" -> pure [CannotAttack]
      "50145" -> pure [ReplaceHeroAttackWithDiscardAndStun $ toRef a]
      "50153" -> do
        gamma <- case target of IdentityRef you -> identityMatches (IdentityWithTrait Trait.Gamma) you; _ -> pure False
        pure $ [ThwartPenalty 1] <> [if gamma then AttackModifier 1 else AttackPenalty 1]
      _ -> pure []
    else pure []

instance HasAbilities AosAttachment where
  getAbilities (AosAttachment (a `With` _)) = identityAbilities <> enemyAbilities <> globalAbilities
   where
    code = toCardCode a
    globalAbilities = [windowAbility a 1 (W.PlaysEvent W.When AnyIdentity AnyCard) ForcedInterrupt NoCost (runAbility a 1) | code == "50069"]
      <> [ability a 2 HeroAction ForAnyPlayer (ExhaustCharacterCost $ CharacterControlledBy You) (runAbility a 2) | code == "50071"]
    identityAbilities = case attachmentIdentity a of
      Nothing -> []
      Just you -> [windowAbility a 3 (W.UsesRecovery $ IdentityWithId you) ForcedResponse (DiscardHandCardsCost 1 $ Just 1) (runAbility a 3) | code == "50153"]
    enemyAbilities = case attachmentEnemy a of
      Nothing -> []
      Just enemy ->
        [windowAbility a 4 (W.EnemyWouldTakeDamage $ EnemyWithId enemy) ForcedInterrupt NoCost (runAbility a 4) | code `elem` ["50092","50117"]]
        <> [windowAbility a 5 (W.EnemyWouldBeDealtDamageBy (EnemyWithId enemy) AnyAttack) ForcedInterrupt NoCost (runAbility a 5) | code == "50149"]
        <> [windowAbility a 6 (W.EnemyActivates W.After (EnemyWithId enemy) AnyIdentity) ForcedResponse NoCost (runAbility a 6) | code == "50116"]
        <> [windowAbility a 7 (W.EnemyAttacked W.After (EnemyWithId enemy) AnyIdentity) ForcedResponse NoCost (runAbility a 7) | code == "50158"]
        <> [windowAbility a 8 (W.CharacterDefeatedByEnemyAttack W.After $ EnemyWithId enemy) ForcedResponse NoCost (runAbility a 8) | code == "50170"]
        <> [windowAbility a 9 (W.IdentityAttacked W.After You $ EnemyWithId enemy) HeroResponse (MultiResourceCost [Just resource,Just resource]) (runAbility a 9) | code `elem` ["50132","50157"], let resource = if code == "50132" then Physical else Energy]
        <> [ability a idx HeroAction NoCriteria (MultiResourceCost $ replicate 3 $ Just resource) (runAbility a idx) | code == "50170", (idx,resource) <- [(10,Physical),(11,Energy)]]

instance RunMessage AosAttachment where
  runMessage msg e@(AosAttachment (a `With` held)) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId a -> do
      attachEffect you
      pure e
    AttachmentMessage aid AttachedToEnemy{} | aid == attachmentId a, toCardCode a `elem` ["50092","50158"], attachmentCounters a == 0 -> do
      a' <- runMessage msg a
      placeCounters a' 4
      pure $ AosAttachment (a' `With` held)
    CardSpecific (isTarget a -> True) "aosSetTransferredCounters" (fromJSON -> Success amount) -> do
      n <- selectListCount (AttachmentWithTrait Trait.BoardMember)
      when (n >= 3) $ gameOver Lost
      pure $ AosAttachment (a {attachmentCounters=amount} `With` held)
    CardSpecific (isTarget a -> True) "mgHoldCard" (fromJSON -> Success card) -> pure $ AosAttachment (a `With` (held <> [card]))
    CardSpecific (isTarget a -> True) "aosModokReset" _ | toCardCode a `elem` ["50114","50115","50116","50118","50119"] -> do
      traverse_ discardCard held
      removeFromPlay a
      pure e
    CardSpecific (isTarget a -> True) "aosRemoveMoreSecrets" (fromJSON -> Success (you,n)) -> e <$ removeSecrets you n
    PlaceCounters target amount | isTarget a target, amount > 0, toCardCode a `elem` ["50181b","50182b","50183b"] -> do
      case toCardCode a of
        "50181b" -> do
          first <- selectJust FirstPlayer
          chars <- friendlyCharactersWithFewestRemainingHitPoints
          chooseEffect first $ [labeled "$label.aosHealVillain" (traverse_ (`heal` 2) $ attachmentEnemy a)]
            <> [targeting (characterRef c) (damageCharacter c a $ toDamage 1 FromAbility) | c <- chars]
        "50182b" -> placeThreatOnEach a 2 MainScheme
        _ -> do
          first <- selectJust FirstPlayer
          chars <- selectList (CharacterControlledBy AnyIdentity)
          chooseEffect first [targeting (characterRef c) (damageCharacter c a $ toDamage 2 FromAbility) | c <- chars]
      AosAttachment . (`With` held) <$> runMessage msg a
    AttachmentMessage aid (AttachmentDamaged amount) | aid == attachmentId a -> pure $ AosAttachment (a {attachmentDamage=attachmentDamage a + amount} `With` held)
    RanAbility _ (isTarget a -> True) 1 windows _ -> do
      for_ [eid | W.EventPlayed _ eid _ <- windows] cancelEventEffects
      removeFromPlay a
      pure e
    RanAbility _ (isTarget a -> True) n _ _ | n `elem` [2,3,9] -> e <$ removeFromPlay a
    RanAbility _ (isTarget a -> True) 4 windows _ -> do
      for_ [(enemy,d) | W.EnemyWouldBeDamaged enemy _ d <- windows] $ \(enemy,d) ->
        if toCardCode a == "50092" then do
          preventEnemyDamage enemy 2
          removeCounters a 1
          when (attachmentCounters a <= 1) $ removeFromPlay a
        else do
          cancelEnemyDamage enemy
          attachmentStep (attachmentId a) $ AttachmentDamaged $ damageAmount d
          when (attachmentDamage a + damageAmount d >= 5) $ removeFromPlay a
      pure e
    RanAbility _ (isTarget a -> True) 5 windows _ -> do
      for_ [(enemy,d) | W.EnemyDamageWouldBeDealt enemy _ d <- windows] $ \(enemy,d) -> do
        cancelEnemyDamage enemy
        case damageSource d of
          FromPlayerAttack you -> damageIdentity you a d {damageSource = FromAbility}
          FromAllyAttack aid -> damageCharacter aid a d {damageSource = FromAbility}
          _ -> pure ()
        removeFromPlay a
      pure e
    RanAbility _ (isTarget a -> True) 6 _ _ -> do
      traverse_ (`heal` 1) (attachmentEnemy a)
      pure e
    RanAbility _ (isTarget a -> True) 7 windows _ -> do
      removeCounters a 1
      let attacked = [you | W.EnemyAttack _ you <- windows] <> [you | W.EnemyAttackAlly _ _ you <- windows]
      traverse_ (\you -> dealIndirectDamage you a 2) attacked
      when (attachmentCounters a <= 1) $ removeFromPlay a
      pure e
    RanAbility _ (isTarget a -> True) 8 _ _ -> e <$ addSecretsToLeast 1
    RanAbility you (isTarget a -> True) idx _ _ | idx `elem` [10,11] -> do
      removeSecrets you 1
      removeFromPlay a
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget a target -> do
      case toCardCode a of
        "50092" -> do
          batrocs <- villainNamed "Batroc"
          traverse_ (\vid -> consumeAndAttach $ EnemyVillainId vid) batrocs
        "50117" -> consumeAndAttach enemy
        "50118" -> whenM (isJust <$> getEnemyAttack enemy) $ attackGains enemy Piercing
        "50145" -> do
          you <- getBoostedPlayer enemy
          already <- identityMatches StunnedIdentity you
          stunIdentity you
          when already $ dealBoost enemy
        _ -> pure ()
      pure e
    _ -> AosAttachment . (`With` held) <$> runMessage msg a
   where
    attachEffect you = case toCardCode a of
      code | code `elem` ["50071","50145","50153"] -> attachToIdentity (attachmentId a) you
      "50149" -> attachNamed you Cards.aos50148
      "50144" -> attachNamed you Cards.aos50143
      "50157" -> do
        mids <- selectList (MinionIs Cards.aos50156)
        if notNull mids then traverse_ (attachTo $ attachmentId a) mids
        else do
          enemies <- selectList EnemyWithHighestAttack
          chooseEffect you [targeting (enemyRef enemy) (attachTo (attachmentId a) enemy) | enemy <- enemies]
      "50158" -> attachNamed you Cards.aos50156
      "50119" -> do
        attachToTheVillain a
        cards <- controlledCards you [UpgradeType]
        if null cards then putTopCardsUnder you a 1
        else chooseEffect you [targeting t do
          card <- getsGame (`cardInPlay` t)
          for_ card $ \case PlayerCard c -> removeFromGame t >> placePlayerCardUnder a c; _ -> pure ()
          | t <- cards]
      "50132" -> attachToTheVillain a >> withTheVillain you (`villainActivatesAgainst` you)
      "50171" -> cardSpecific GameRef "aosReluctantFoe" (you,attachmentId a)
      _ -> attachToTheVillain a
    attachNamed you def = do
      mids <- selectList (MinionIs def)
      if null mids then attachToTheVillain a
      else chooseEffect you [targeting (enemyRef $ EnemyMinionId mid) (attachTo (attachmentId a) mid) | mid <- mids]
    consumeAndAttach enemy = do
      card <- getBoostCard (toTarget a)
      for_ card $ \c -> consumeBoostCard c >> attachEncounterCard c enemy


newtype AosGauntlet = AosGauntlet (Attrs Attachment `With` Bool)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsAttachment AosGauntlet where
  toAttachmentAttrs (AosGauntlet (a `With` _)) = a

instance HasModifiersFor AosGauntlet where
  getModifiersFor _ target (AosGauntlet (a `With` _)) = pure
    [m | Just enemy <- [attachmentEnemy a], target == enemyRef enemy, m <- [AttackModifier 1,KeywordModifier $ Retaliate 1]]

instance HasAbilities AosGauntlet where
  getAbilities (AosGauntlet (a `With` prepared)) =
    [windowAbility a 1 matcher HeroResponse NoCost (runAbility a 1)
    | not prepared, Just enemy <- [attachmentEnemy a]
    , matcher <- [W.IdentityAttacked W.After You (EnemyWithId enemy),W.AllyAttacked W.After (AllyControlledBy You) (EnemyWithId enemy)]]

instance RunMessage AosGauntlet where
  runMessage msg e@(AosGauntlet (a `With` prepared)) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment _) | aid == attachmentId a -> e <$ attachToTheVillain a
    CardSpecific (isTarget a -> True) "aosPreparationStatus" (fromJSON -> Success value) -> pure $ AosGauntlet (a `With` value)
    RanAbility _ (isTarget a -> True) 1 _ _ -> e <$ removeFromPlay a
    _ -> AosGauntlet . (`With` prepared) <$> runMessage msg a


newtype AosWeapon = AosWeapon (Attrs Attachment `With` Bool)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsAttachment AosWeapon where
  toAttachmentAttrs (AosWeapon (a `With` _)) = a

instance HasModifiersFor AosWeapon where
  getModifiersFor _ target (AosWeapon (a `With` _)) = pure $ case attachmentEnemy a of
    Just enemy | target == enemyRef enemy -> [AttackModifier 1] <> if toCardCode a == "50132" then [SchemeModifier 1] else [KeywordModifier Overkill,KeywordModifier Ranged]
    _ -> []

instance HasAbilities AosWeapon where
  getAbilities (AosWeapon (a `With` damaged)) =
    [windowAbility a 1 (W.IdentityAttacked W.After You $ EnemyWithId enemy) HeroResponse (MultiResourceCost $ replicate 2 $ Just resource) (runAbility a 1)
    | damaged, Just enemy <- [attachmentEnemy a], let resource = if toCardCode a == "50132" then Physical else Energy]

instance RunMessage AosWeapon where
  runMessage msg e@(AosWeapon (a `With` damaged)) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId a -> do
      if toCardCode a == "50132" then attachToTheVillain a >> withTheVillain you (`villainActivatesAgainst` you)
      else do
        mids <- selectList (MinionIs Cards.aos50156)
        if notNull mids then traverse_ (attachTo aid) mids else do
          enemies <- selectList EnemyWithHighestAttack
          chooseEffect you [targeting (enemyRef enemy) (attachTo aid enemy) | enemy <- enemies]
      pure e
    CheckWindows windows -> do
      let starts = any (\case W.Window W.When (W.IdentityAttackTarget _ enemy) -> Just enemy == attachmentEnemy a; _ -> False) windows
          hit = any (\case
            W.Window W.After (W.EnemyTookDamageFrom enemy d _) -> Just enemy == attachmentEnemy a && damageAmount d > 0 && playerDamage d
            _ -> False) windows
          playerDamage d = case damageSource d of FromPlayerAttack _ -> True; _ -> False
      pure $ AosWeapon (a `With` (hit || (damaged && not starts)))
    RanAbility _ (isTarget a -> True) 1 _ _ -> e <$ removeFromPlay a
    _ -> AosWeapon . (`With` damaged) <$> runMessage msg a
