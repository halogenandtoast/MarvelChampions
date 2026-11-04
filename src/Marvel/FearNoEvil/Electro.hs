module Marvel.FearNoEvil.Electro where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Attachment.Types
import Marvel.Card
import Marvel.Cost
import Marvel.Damage
import Marvel.Criteria
import Marvel.Difficulty
import Marvel.Entity
import Marvel.FearNoEvil.Cards qualified as Cards
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hand
import Marvel.Hp
import Marvel.Id
import Marvel.Identity.Types (Field(PlayerIdentityHand))
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Types
import Marvel.Minion.Runner ()
import Marvel.Modifier
import Marvel.Payment
import Marvel.Projection
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.SideScheme.Types
import Marvel.Stats
import Marvel.Treachery.Types
import Marvel.Villain.Types qualified as V
import Marvel.Window qualified as W

chargeCard :: HasGame m => m (Maybe AttachmentId)
chargeCard = selectOne $ AttachmentIs Cards.street60079
chargeCount :: HasGame m => m Natural
chargeCount = chargeCard >>= maybe (pure 0) getCountersOn
addCharge :: ReverseQueue m => Natural -> m ()
addCharge n = chargeCard >>= traverse_ (`placeCounters` n)
removeCharge :: ReverseQueue m => Natural -> m ()
removeCharge n = chargeCard >>= traverse_ (`removeCounters` n)
energyOrTwo :: [(Text,Cost)]
energyOrTwo = [("$label.spendAnEnergyResource",ResourceCost $ Just Energy),("$label.fneSpendTwoResources",MultiResourceCost [Nothing,Nothing])]

newtype Electro = Electro (Attrs V.Villain)
  deriving newtype (Show,Eq,ToJSON,FromJSON,IsRef)
  deriving anyclass (V.IsVillain,HasModifiersFor)
electroStages :: [V.VillainCard Electro]
electroStages = [V.villainWith Electro def (Sch sch) (Atk atk) (HP $ PerPlayer hp) (V.stageL .~ stage)
  | (def,sch,atk,hp,stage) <- [(Cards.street60076,1,1,15,1),(Cards.street60077,1,2,17,2),(Cards.street60078,2,2,20,3)]]
instance HasAbilities Electro where
  getAbilities (Electro a) =
    [windowAbility a 1 (W.VillainRevealed W.When (VillainWithId $ V.villainId a) W.RevealedFromVillain) WhenRevealed NoCost (runAbility a 1)
    ,windowAbility a 2 (W.VillainSchemed W.After $ VillainWithId $ V.villainId a) ForcedResponse NoCost (runAbility a 2)]
instance RunMessage Electro where
  runMessage msg e@(Electro a) = runQueueT $ case msg of
    VillainMessage vid VillainDefeated | vid == V.villainId a -> do
      expert <- (== Expert) <$> getDifficulty
      case V.villainStage a of
        1 -> replaceVillain vid "60077" Advanced >> pure e
        2 | expert -> replaceVillain vid "60078" Advanced >> pure e
        _ -> Electro <$> runMessage msg a
    RanAbility _ (isTarget a -> True) 1 _ _ -> do
      when (V.villainStage a < 3) $ do
        cards <- findEncounterCards $ CardIs Cards.street60079
        for_ (headMay cards) $ \card -> attachEncounterToCharacter card (VillainCharacter $ V.villainId a)
      amount <- fromIntegral <$> fromGameValue (PerPlayer $ if V.villainStage a == 3 then 3 else 2)
      cardSpecific a "initialCharges" (amount :: Natural)
      pure e
    CardSpecific (isTarget a -> True) "initialCharges" (fromJSON -> Success n) -> addCharge n >> pure e
    RanAbility _ (isTarget a -> True) 2 _ _ -> addCharge (if V.villainStage a == 3 then 2 else 1) >> pure e
    _ -> Electro <$> runMessage msg a

newtype ElectroAttachment = ElectroAttachment (Attrs Attachment)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)
  deriving anyclass (IsAttachment,HasModifiersFor)
electroAttachments :: [AttachmentCard ElectroAttachment]
electroAttachments = [attachment ElectroAttachment Cards.street60079,attachment (\a -> ElectroAttachment a {attachmentCounters=3}) Cards.street60080]
instance HasAbilities ElectroAttachment where
  getAbilities (ElectroAttachment a)
    | toCardCode a == "60079" =
      [windowAbility a 1 (W.EnemyAttackedAnyTarget W.When $ EnemyWithId enemy) ForcedInterrupt (ExactCountersCost 1) (runAbility a 1)
        | Just enemy <- [attachmentEnemy a],attachmentCounters a > 0]
      <> [label text $ ability a index HeroAction NoCriteria cost (runAbility a index) | attachmentCounters a > 0,(index,(text,cost)) <- zip [2..] energyOrTwo]
    | otherwise = [windowAbility a 4 (W.UpgradeWouldReady $ UpgradeControlledBy $ IdentityWithId you) ForcedInterrupt NoCost (runAbility a 4)
        | Just you <- [attachmentIdentity a],attachmentCounters a > 0]
instance RunMessage ElectroAttachment where
  runMessage msg e@(ElectroAttachment a) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId a -> do
      if toCardCode a == "60079" then villainNamed "Electro" >>= traverse_ (attachTo aid) else do
        attachToIdentity aid you
        upgrades <- selectList $ UpgradeControlledBy $ IdentityWithId you
        traverse_ exhaust upgrades
      pure e
    RanAbility _ (isTarget a -> True) 1 _ _ -> do
      for_ (attachmentEnemy a) $ \enemy -> dealBoost (enemyRef enemy) >> attackGains enemy Overkill
      pure e
    RanAbility _ (isTarget a -> True) index _ _ | index `elem` [2,3] -> removeCounters a 1 >> pure e
    RanAbility _ (isTarget a -> True) 4 windows _ -> do
      for_ (attachmentIdentity a) $ \you -> for_ (headMay [uid | W.UpgradeReadying uid <- windows]) $ \uid -> do
        preventUpgradeReady uid
        costs <- filterM (canAffordCost you (toSource a) . snd) energyOrTwo
        chooseOne you $ [labeled text (payCostThenWithPayment you a cost a "drainedPaid" uid) | (text,cost) <- costs]
          <> [labeled "$label.fneDoNotReadyUpgrade" (pure ())]
      pure e
    CardSpecific (isTarget a -> True) "drainedPaid" (fromJSON -> Success (uid,_ :: Payment)) -> do
      removeCounters a 1
      readyUpgradeAfterInterrupt uid
      pure e
    RemoveCounters target n | isTarget a target, toCardCode a == "60080" -> do
      when (n >= attachmentCounters a) $ removeFromPlay a
      ElectroAttachment <$> runMessage msg a
    _ -> ElectroAttachment <$> runMessage msg a

newtype ChargingUp = ChargingUp (Attrs SideScheme)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)
  deriving anyclass HasModifiersFor
instance IsSideScheme ChargingUp where
  sideSchemeAbilities = getAbilities
chargingUp :: SideSchemeCard ChargingUp
chargingUp = sideScheme ChargingUp Cards.street60082 (Static 6)
instance HasAbilities ChargingUp where
  getAbilities (ChargingUp a) = [windowAbility a 1 (W.SideSchemeDefeated W.When $ SideSchemeWithId $ sideSchemeId a) WhenRevealed NoCost (runAbility a 1)]
instance RunMessage ChargingUp where
  runMessage msg e@(ChargingUp a) = runQueueT $ case msg of
    RanAbility _ (isTarget a -> True) 1 _ _ -> addCharge 3 >> pure e
    _ -> ChargingUp <$> runMessage msg a

newtype Whiplash = Whiplash (Attrs Minion)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)
  deriving anyclass (IsMinion,HasModifiersFor)
whiplash :: MinionCard Whiplash
whiplash = minion Whiplash Cards.street60081 (Sch 2) (Atk 2) (HP 5)
instance HasAbilities Whiplash where
  getAbilities (Whiplash a) = [windowAbility a 1 windows ForcedInterrupt (CountersFromAttachmentCost (AttachmentIs Cards.street60079) 1) (runAbility a 1)]
   where
    target = EnemyWithId $ toEnemyId a
    windows = W.WindowOneOf [W.IdentityAttacked W.When AnyIdentity target,W.AllyAttacked W.When AnyAlly target,W.EnemyWasAttacked W.When target]
instance RunMessage Whiplash where
  runMessage msg e@(Whiplash a) = runQueueT $ case msg of
    RanAbility _ (isTarget a -> True) 1 windows _ -> do
      let attackers = mapMaybe (\case
            W.IdentityAttack you target | target == toEnemyId a -> Just $ IdentityCharacter you
            W.AllyAttack ally target | target == toEnemyId a -> Just $ AllyCharacter ally
            W.EnemyAttackOnEnemy attacker target | target == toEnemyId a -> Just $ toCharacter attacker
            _ -> Nothing) windows
      for_ (headMay attackers) $ \attacker -> do
        cards <- take 1 <$> getEncounterDeck
        discardTopOfEncounterDeck 1
        countBoostIcons a attacker cards
      pure e
    WithBoostIcons (isTarget a -> True) (fromJSON -> Success attacker) counts -> do
      damageCharacter (attacker :: CharacterId) a (toDamage (sum $ map snd counts) FromAbility)
      pure e
    _ -> Whiplash <$> runMessage msg a

newtype ElectroTreachery = ElectroTreachery (Attrs Treachery)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)
  deriving anyclass IsTreachery
electroTreacheries :: [TreacheryCard ElectroTreachery]
electroTreacheries = map (treachery ElectroTreachery) [Cards.street60083,Cards.street60084,Cards.street60085]
instance RunMessage ElectroTreachery where
  runMessage msg e@(ElectroTreachery a) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId a -> do
      case toCardCode a of
        "60083" -> selectList (CharacterControlledBy $ IdentityWithId you) >>= cardSpecific a "chainNext" . (you,)
        "60084" -> do
          hand <- unHand <$> project PlayerIdentityHand you
          let cards = filter (elem Energy . printedResources . getCardDef) hand
          if null cards then gainSurge a else cardSpecific a "overloadNext" (you,cards)
        _ -> do
          allies <- selectList $ AllyControlledBy $ IdentityWithId you
          supports <- selectList $ SupportControlledBy $ IdentityWithId you
          upgrades <- selectList $ UpgradeControlledBy $ IdentityWithId you
          schemes <- getControlledPlayerSideSchemes you False
          discardFromDeck you (fromIntegral $ 1 + length allies + length supports + length upgrades + length schemes) a
      pure $ ElectroTreachery a {treacheryResolver=Just you}
    CardSpecific (isTarget a -> True) "chainNext" (fromJSON -> Success (you,characters :: [CharacterId])) -> do
      remaining <- filterM (characterMatches AnyCharacter) characters
      chooseOne you [targeting (characterRef character) (cardSpecific a "chainOne" (you,character) >> cardSpecific a "chainNext" (you,filter (/= character) remaining)) | character <- remaining]
      pure e
    CardSpecific (isTarget a -> True) "chainOne" (fromJSON -> Success (you,character :: CharacterId)) -> do
      ready <- characterMatches (CharacterOneOf [IdentityCharacters UnexhaustedIdentity,AllyCharacters UnexhaustedAlly]) character
      charged <- (> 0) <$> chargeCount
      modifiers <- getModifiers (characterRef character)
      chooseOne you $
        [labeled "$label.fneExhaustCharacter" (case character of IdentityCharacter identity -> exhaust identity; AllyCharacter ally -> exhaust ally; _ -> pure ()) | ready]
        <> [labeled "$label.fneLightningDamage" (damageCharacter character a (toDamage (if charged then 3 else 1) FromAbility) >> removeCharge 1) | charged || CannotTakeDamage `notElem` modifiers]
      pure e
    CardSpecific (isTarget a -> True) "overloadNext" (fromJSON -> Success (you,cards :: [PlayerCard])) -> do
      chooseOne you [cardLabeled (toCardCode card) (cardSpecific a "overloadOne" (you,card) >> cardSpecific a "overloadNext" (you,filter ((/= pcCardId card) . pcCardId) cards)) | card <- cards]
      pure e
    CardSpecific (isTarget a -> True) "overloadOne" (fromJSON -> Success (you,card :: PlayerCard)) -> do
      hand <- unHand <$> project PlayerIdentityHand you
      characters <- filterM (`characterCanGainStatus` StunnedStatus) =<< selectList (CharacterControlledBy $ IdentityWithId you)
      chooseOne you $ [cardLabeled (toCardCode card) (discardCard card) | card `elem` hand]
        <> [targeting (characterRef character) (giveStatus character StunnedStatus) | character <- characters]
      pure e
    WithDiscarded (isTarget a -> True) FromDeck cards -> do
      let count = length [card | card <- cards,Energy `elem` printedResources (getCardDef card)]
      for_ (treacheryResolver a) $ \you -> cardSpecific a "shockingNext" (you,count)
      pure e
    CardSpecific (isTarget a -> True) "shockingNext" (fromJSON -> Success (you,n :: Int)) -> do
      when (n > 0) $ do
        charge <- chargeCard
        chooseOne you $ [labeled "$label.fneTakeIndirectDamage" (dealIndirectDamage you a 1 >> cardSpecific a "shockingNext" (you,n-1))]
          <> [targeting aid (placeCounters aid 1 >> cardSpecific a "shockingNext" (you,n-1)) | Just aid <- [charge]]
      pure e
    _ -> ElectroTreachery <$> runMessage msg a
