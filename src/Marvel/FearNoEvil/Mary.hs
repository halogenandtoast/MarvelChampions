module Marvel.FearNoEvil.Mary where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Attachment.Types
import Marvel.Card
import Marvel.Cost
import Marvel.Damage
import Marvel.Criteria
import Marvel.Difficulty
import Marvel.Entity
import Marvel.Environment.Types
import Marvel.FearNoEvil.Cards qualified as Cards
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hp
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Types
import Marvel.Minion.Runner ()
import Marvel.Modifier
import Marvel.Phase
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.SideScheme.Types
import Marvel.Stats
import Marvel.Treachery.Types
import Marvel.Villain.Types qualified as V
import Marvel.Window qualified as W

newtype Mary = Mary (Attrs V.Villain)
  deriving newtype (Show,Eq,ToJSON,FromJSON,IsRef)
  deriving anyclass (V.IsVillain,HasModifiersFor)
maryFaces :: [V.VillainCard Mary]
maryFaces = [V.villain Mary def (Sch sch) (Atk atk) (HP $ PerPlayer hp) | (def,sch,atk,hp) <-
  [(Cards.street60110a,2,1,10),(Cards.street60110b,1,2,10),(Cards.street60111a,3,1,13),(Cards.street60111b,1,3,13)]]
instance HasAbilities Mary where
  getAbilities (Mary a) = [windowAbility a 1 (W.VillainRevealed W.When (VillainWithId $ V.villainId a) W.RevealedFromVillain) WhenRevealed NoCost (runAbility a 1) | toCardCode (getCardDef a) == "60111b"]
instance RunMessage Mary where
  runMessage msg e@(Mary a) = runQueueT $ case msg of
    VillainMessage vid VillainWouldBeDefeated | vid == V.villainId a -> do
      cancelVillainDefeat vid
      villainStep vid SetVillainHp
      psyche <- selectOne $ EnvironmentWithTitle "Disturbed Psyche"
      for_ psyche $ \eid -> cardSpecific eid "psycheToken" True
      pure e
    VillainMessage vid FlipVillain | vid == V.villainId a -> do
      let code = toCardCode (getCardDef a)
          other = case code of "60110a" -> 1; "60110b" -> 0; "60111a" -> 3; _ -> 2
      pure $ V.flipVillainTo (maryFaces !! other) a
    RanAbility _ (isTarget a -> True) 1 _ _ -> getAllPlayers >>= traverse_ (\you -> damageIdentity you a $ toDamage 1 FromAbility) >> pure e
    _ -> Mary <$> runMessage msg a

newtype DisturbedPsyche = DisturbedPsyche (Attrs Environment)
  deriving newtype (Show,Eq,ToJSON,FromJSON,IsRef,HasCardCode)
  deriving anyclass (IsEnvironment,HasModifiersFor)
disturbedPsyche :: EnvironmentCard DisturbedPsyche
disturbedPsyche = environment DisturbedPsyche Cards.street60112
instance HasAbilities DisturbedPsyche where
  getAbilities (DisturbedPsyche a) = [windowAbility a 1 (W.PhaseEnds W.When VillainPhase) ForcedInterrupt NoCost (runAbility a 1)]
instance RunMessage DisturbedPsyche where
  runMessage msg e@(DisturbedPsyche a) = runQueueT $ case msg of
    RanAbility you (isTarget a -> True) 1 _ _ -> withTheVillain you flipVillain >> pure e
    CardSpecific (isTarget a -> True) "psycheToken" (fromJSON -> Success (isDamage :: Bool)) -> do
      let damage = environmentDamage a + if isDamage then 1 else 0
          threat = environmentCounters a + if isDamage then 0 else 1
      when (damage + threat >= 3) $ gameOver Won
      pure $ DisturbedPsyche a {environmentDamage=damage,environmentCounters=threat}
    _ -> DisturbedPsyche <$> runMessage msg a

newtype EstablishTrust = EstablishTrust (Attrs SideScheme)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)
  deriving anyclass HasModifiersFor
instance IsSideScheme EstablishTrust where sideSchemeAbilities = getAbilities
establishTrust :: SideSchemeCard EstablishTrust
establishTrust = sideScheme EstablishTrust Cards.street60113a (PerPlayer 8)
instance HasAbilities EstablishTrust where
  getAbilities (EstablishTrust a) = [windowAbility a 1 (W.SideSchemeDefeated W.When $ SideSchemeWithId $ sideSchemeId a) WhenRevealed NoCost (runAbility a 1)]
instance RunMessage EstablishTrust where
  runMessage msg e@(EstablishTrust a) = runQueueT $ case msg of
    SideSchemeMessage sid SideSchemePlaceInitialThreat | sid == sideSchemeId a -> do
      expert <- (== Expert) <$> getDifficulty
      base <- runMessage msg a
      extra <- if expert then fromIntegral <$> fromGameValue (PerPlayer 3) else pure 0
      pure $ EstablishTrust base {sideSchemeThreat=sideSchemeThreat base + extra}
    RanAbility you (isTarget a -> True) 1 _ _ -> do
      psyche <- selectOne $ EnvironmentWithTitle "Disturbed Psyche"
      for_ psyche $ \eid -> cardSpecific eid "psycheToken" False
      transformSideSchemeToAttachment (sideSchemeId a) "60113b" you
      pure e
    _ -> EstablishTrust <$> runMessage msg a

newtype MaryAttachment = MaryAttachment (Attrs Attachment)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)
  deriving anyclass IsAttachment
maryAttachments :: [AttachmentCard MaryAttachment]
maryAttachments = map (attachment MaryAttachment) [Cards.street60113b,Cards.street60114,Cards.street60117]
instance HasModifiersFor MaryAttachment where
  getModifiersFor _ target (MaryAttachment a) = pure $ if Just target == (enemyRef <$> attachmentEnemy a) then case toCardCode a of
    "60113b" -> [CannotActivate]
    "60114" -> [SchemeModifier 2]
    _ -> []
    else []
instance HasAbilities MaryAttachment where
  getAbilities (MaryAttachment a) = case attachmentEnemy a of
    Just enemy@(EnemyVillainId vid) -> case toCardCode a of
      "60113b" -> [windowAbility a 1 (W.WindowOneOf [W.VillainDamaged W.After $ VillainWithId vid,W.RoundEnds]) ForcedResponse NoCost (runAbility a 1)]
      "60114" -> [ability a 1 HeroAction NoCriteria (PrintedResourceCost [Mental,Mental]) $ runAbility a 1]
      _ -> [windowAbility a 1 (W.EnemyWouldTakeDamage $ EnemyWithId enemy) ForcedInterrupt NoCost (runAbility a 1)]
    _ -> []
instance RunMessage MaryAttachment where
  runMessage msg e@(MaryAttachment a) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId a -> withTheVillain you (attachTo aid) >> pure e
    RanAbility you (isTarget a -> True) 1 windows _ -> do
      case toCardCode a of
        "60113b" -> getAllPlayers >>= traverse_ dealEncounterCard >> flipMaryWalker (attachmentId a) you False
        "60114" -> removeFromPlay a
        _ -> for_ (attachmentEnemy a) $ \enemy -> for_ [d | W.EnemyWouldBeDamaged target _ d <- windows,target == enemy] $ placeCharacterDamageOnAttachment (toCharacter enemy) (attachmentId a)
      pure e
    AttachmentMessage aid (AttachmentDamaged n) | aid == attachmentId a -> do
      expert <- (== Expert) <$> getDifficulty
      let total = attachmentDamage a + n
      when (total >= if expert then 8 else 5) $ removeFromPlay a
      pure $ MaryAttachment a {attachmentDamage=total}
    Boost (RevealedAsBoost target enemy) | isTarget a target -> do
      you <- getBoostedPlayer enemy
      typhoid <- enemyIsMary enemy False
      bloody <- enemyIsMary enemy True
      when (toCardCode a == "60114" && typhoid) $ dealIndirectDamageAndExhaustDamaged you a 3
      when (toCardCode a == "60117" && bloody) $ confuseIdentity you
      pure e
    _ -> MaryAttachment <$> runMessage msg a

-- Turning over the physical card clears the previous face's tokens and attachments.
flipMaryWalker :: ReverseQueue m => AttachmentId -> IdentityId -> Bool -> m ()
flipMaryWalker aid you reveal = cardSpecific GameRef "flipMaryWalker" (aid,you,reveal)
enemyIsMary :: HasGame m => EnemyId -> Bool -> m Bool
enemyIsMary enemy bloody = do
  def <- getTargetCardDef (enemyRef enemy)
  pure $ maybe False ((`elem` if bloody then ["60110b","60111b"] else ["60110a","60111a"]) . toCardCode) def

newtype MaryTreachery = MaryTreachery (Attrs Treachery)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)
  deriving anyclass (IsTreachery,HasAbilities,HasModifiersFor)
maryTreacheries :: [TreacheryCard MaryTreachery]
maryTreacheries = map (treachery MaryTreachery) [Cards.street60115,Cards.street60116,Cards.street60118,Cards.street60119]
instance RunMessage MaryTreachery where
  runMessage msg e@(MaryTreachery a) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId a -> do
      case toCardCode a of
        "60115" -> breakTrust you >> cardSpecific a "activation" you
        "60118" -> breakTrust you >> cardSpecific a "activation" you
        "60116" -> do
          characters <- selectList $ CharacterControlledBy (IdentityWithId you)
          damageCharacters a (toDamage 1 FromAbility) characters
          stunIdentity you
        _ -> withTheVillain you $ \vid -> giveStatus (VillainCharacter vid) ToughStatus
      pure e
    CardSpecific (isTarget a -> True) "activation" (fromJSON -> Success you) -> do
      withTheVillain you $ \vid -> if toCardCode a == "60115" then villainSchemes vid else villainAttacksCharacter vid you you
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget a target -> do
      you <- getBoostedPlayer enemy
      typhoid <- enemyIsMary enemy False
      bloody <- enemyIsMary enemy True
      case toCardCode a of
        "60115" | typhoid -> selectList (AllyControlledBy $ IdentityWithId you) >>= \allies -> unless (null allies) $ chooseOne you [targeting (AllyRef ally) (damageCharacter ally a $ toDamage 3 FromAbility) | ally <- allies]
        "60116" | typhoid -> stunIdentity you
        "60118" | bloody -> damageIdentity you a $ toDamage 2 FromAbility
        "60119" | bloody -> giveStatus (toCharacter enemy) ToughStatus
        _ -> pure ()
      pure e
    _ -> MaryTreachery <$> runMessage msg a
   where
    breakTrust you = selectList (AttachmentIs Cards.street60113b) >>= traverse_ (\aid -> flipMaryWalker aid you True)

newtype HandLookout = HandLookout (Attrs Minion)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)
  deriving anyclass (IsMinion,HasAbilities,HasModifiersFor)
handLookout :: MinionCard HandLookout
handLookout = minion HandLookout Cards.street60120 (Sch 0) (Atk 2) (HP 4)
instance RunMessage HandLookout where
  runMessage msg e@(HandLookout a) = runQueueT $ case msg of
    Boost (RevealedAsBoost target enemy) | isTarget a target -> dealBoost enemy >> pure e
    _ -> HandLookout <$> runMessage msg a
