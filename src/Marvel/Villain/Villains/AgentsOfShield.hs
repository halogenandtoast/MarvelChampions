{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.Villain.Villains.AgentsOfShield where

import Marvel.Prelude
import Marvel.Question (runAbility)
import Marvel.Ability
import Marvel.AgentsOfShield.Board
import Marvel.AgentsOfShield.EncounterCards qualified as Cards
import Marvel.AgentsOfShield.Helpers
import Marvel.Card
import Marvel.Count
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Difficulty
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hood.Helpers (controlledCharacters)
import Marvel.Hp
import Marvel.Hand (unHand)
import Marvel.Identity.Types (Field (PlayerIdentityHand))
import Marvel.Projection
import Marvel.Id
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Phase
import Marvel.Query
import Marvel.Ref
import Marvel.Stats
import Marvel.Trait qualified as Trait
import Marvel.Villain.Types
import Marvel.Window qualified as W

-- A preparation belongs to the attack that exposed it, including an ally's attack.
data AosVillainState = AosVillainState
  { aosAttackSource :: Maybe Source
  , aosAttacker :: Maybe CharacterId
  , aosPreparationResolved :: Bool
  , aosAfterAttack :: [(Text,EncounterCard)]
  , aosPhaseTwo :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

emptyState :: AosVillainState
emptyState = AosVillainState Nothing Nothing False [] False

newtype AosVillain = AosVillain (Attrs Villain `With` AosVillainState)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance IsVillain AosVillain where
  toVillainAttrs (AosVillain (a `With` _)) = a
  overVillainAttrs f (AosVillain (a `With` state)) = AosVillain (f a `With` state)

aosVillain :: Attrs Villain -> AosVillain
aosVillain = AosVillain . (`With` emptyState)

aos50064 :: VillainCard AosVillain
aos50064 = villainWith aosVillain Cards.aos50064 (Sch 2) (Atk 1) (HP $ PerPlayer 13) (stageL .~ 1)

aos50065 :: VillainCard AosVillain
aos50065 = villainWith aosVillain Cards.aos50065 (Sch 2) (Atk 2) (HP $ PerPlayer 16) (stageL .~ 2)

aos50066 :: VillainCard AosVillain
aos50066 = villainWith aosVillain Cards.aos50066 (Sch 3) (Atk 2) (HP $ PerPlayer 13) (stageL .~ 3)

aos50086a :: VillainCard AosVillain
aos50086a = villainWith aosVillain Cards.aos50086a (Sch 1) (Atk 2) (HP $ Static 8) (stageL .~ 1)

aos50086b :: VillainCard AosVillain
aos50086b = villainWith aosVillain Cards.aos50086b (Sch 2) (Atk 2) (HP $ Static 12) (stageL .~ 1)

aos50103a :: VillainCard AosVillain
aos50103a = villainWith aosVillain Cards.aos50103a (Sch 2) (Atk 1) (HP $ Static 10) (stageL .~ 1)

aos50103b :: VillainCard AosVillain
aos50103b = villainWith aosVillain Cards.aos50103b (Sch 3) (Atk 1) (HP $ Static 14) (stageL .~ 1)

aos50129a :: VillainCard AosVillain
aos50129a = villainWith aosVillain Cards.aos50129a (Sch 2) (Atk 2) (HP $ PerPlayer 12) (stageL .~ 1)

aos50129b :: VillainCard AosVillain
aos50129b = villainWith aosVillain Cards.aos50129b (Sch 3) (Atk 2) (HP $ PerPlayer 16) (stageL .~ 1)

aos50165a :: VillainCard AosVillain
aos50165a = villainWith aosVillain Cards.aos50165a (Sch 3) (Atk 2) (HP $ Static 12) (stageL .~ 1)

aos50165b :: VillainCard AosVillain
aos50165b = villainWith aosVillain Cards.aos50165b (Sch 3) (Atk 2) (HP $ PerPlayer 18) (stageL .~ 1)

aos50166a :: VillainCard AosVillain
aos50166a = villainWith aosVillain Cards.aos50166a (Sch 4) (Atk 2) (HP $ Static 16) (stageL .~ 1)

aos50166b :: VillainCard AosVillain
aos50166b = villainWith aosVillain Cards.aos50166b (Sch 4) (Atk 2) (HP $ PerPlayer 18) (stageL .~ 1)

instance HasModifiersFor AosVillain where
  getModifiersFor _ target (AosVillain (a `With` _)) | target == toRef a, toCardCode (getCardDef a) `elem` ["50129a","50129b"] = do
    required <- fromGameValue (PerPlayer 1)
    n <- length . filter (cardMatch $ CardWithType MinionType <> CardWithTrait Trait.Thunderbolt) <$> getVictoryDisplay
    pure [CannotBeDefeated | n < required]
  getModifiersFor _ _ _ = pure []

instance HasAbilities AosVillain where
  getAbilities (AosVillain (a `With` state)) = case toCardCode (getCardDef a) of
    code | code `elem` ["50064","50065","50066"] ->
      [windowAbility a 1 matcher ForcedInterrupt NoCost (runAbility a 1)
      | matcher <- [W.IdentityTargetsEnemy W.When AnyIdentity enemy,W.AllyAttacked W.When AnyAlly enemy]]
      <> [windowAbility a 2 (W.VillainRevealed W.When (VillainWithId $ villainId a) W.RevealedFromVillain) WhenRevealed NoCost (runAbility a 2) | code /= "50064"]
    code | code `elem` ["50086a","50086b"] -> [windowAbility a 3 (W.EnemyAttackedAnyTarget W.After enemy) ForcedResponse NoCost (runAbility a 3)]
    code | code `elem` ["50129a","50129b"], aosPhaseTwo state -> [windowAbility a 4 (W.VillainWouldActivate AnyIdentity) ForcedInterrupt NoCost (runAbility a 4)]
    code | code `elem` ["50165b","50166b"] -> [windowAbility a 5 (W.VillainWouldActivate AnyIdentity) ForcedInterrupt NoCost (runAbility a 5)]
    _ -> []
   where enemy = EnemyWithId $ EnemyVillainId $ villainId a

instance RunMessage AosVillain where
  runMessage msg e@(AosVillain (a `With` state)) = runQueueT $ case msg of
    VillainMessage vid (VillainResolveDefeat _ _) | vid == villainId a, toCardCode (getCardDef a) `elem` ["50086a","50086b","50103a","50103b","50165a","50166a"] -> replaceDefeat
    VillainMessage vid VillainWouldBeDefeated | vid == villainId a, toCardCode (getCardDef a) `elem` ["50086a","50086b","50103a","50103b","50165a","50166a"] -> replaceDefeat
    VillainMessage vid VillainDefeated | vid == villainId a, toCardCode (getCardDef a) `elem` ["50064","50065"] -> do
      difficulty <- getDifficulty
      case (difficulty,toCardCode (getCardDef a)) of
        (Normal,"50064") -> advanceTo aos50065
        (Expert,"50065") -> advanceTo aos50066
        _ -> AosVillain . (`With` state) <$> runMessage msg a
    VillainMessage vid FlipVillain | vid == villainId a, toCardCode (getCardDef a) `elem` ["50165a","50166a"] -> do
      let AosVillain (updated `With` _) = flipVillainTo (if toCardCode (getCardDef a) == "50165a" then aos50165b else aos50166b) a
      villainStep vid SetVillainHp
      pure $ AosVillain (updated `With` state)
    RanAbility you (isTarget a -> True) 1 windows _ -> do
      let ally = headMay [aid | W.AllyAttack aid _ <- windows]
      who <- maybe (pure you) (fmap (fromMaybe you) . getAllyControllerId) ally
      source <- case ally of Just aid -> pure $ Just $ AllyRef aid; Nothing -> currentPlayerAttackSource who
      let attacker = maybe (IdentityCharacter who) AllyCharacter ally
          next = state {aosAttackSource = source <|> Just (IdentityRef who),aosAttacker = Just attacker,aosPreparationResolved = False,aosAfterAttack = []}
      gauntlets <- selectList (AttachmentIs Cards.aos50068)
      traverse_ (\aid -> cardSpecific aid "aosPreparationStatus" False) gauntlets
      schemes <- selectList MainScheme
      for_ (headMay schemes) $ \scheme -> do
        threat <- selectCount SchemeThreat (SchemeWithId scheme)
        when (threat > 0) do
          removeThreat who a 1 (SchemeWithId scheme)
          discardTopOfEncounterDeckFor 1 a
      pure $ AosVillain (a `With` next)
    RanAbility _ (isTarget a -> True) 2 _ _ -> do
      amount <- fromIntegral <$> fromGameValue (PerPlayer $ if toCardCode (getCardDef a) == "50065" then 2 else 3)
      placeThreatOnEach a amount MainScheme
      pure e
    RanAbility _ (isTarget a -> True) 3 _ _ -> do
      selectOne (EnvironmentWithTitle "Alert Level") >>= traverse_ (`placeCounters` 1)
      pure e
    RanAbility _ (isTarget a -> True) 4 windows _ -> do
      for_ [(vid,you) | W.VillainActivationWouldBegin vid you <- windows, vid == villainId a] $ \(_,you) -> do
        hero <- identityMatches HeroIdentity you
        status <- characterHasCondition (if hero then StunnedStatus else ConfusedStatus) (toCharacter $ EnemyVillainId $ villainId a)
        engaged <- selectAny (MinionWithTrait Trait.Thunderbolt <> MinionEngagedWith (IdentityWithId you))
        when (engaged && not status) do
          cancelMatchingMessage $ \case
            CardSpecific GameRef "resolveVillainActivation" (fromJSON -> Success (vid,who :: IdentityId)) -> vid == villainId a && who == you
            CardSpecific target "activationAfterWould" _ -> target == toRef a
            _ -> False
          heal (villainId a) (if toCardCode (getCardDef a) == "50129a" then 4 else 6)
      pure e
    RanAbility _ (isTarget a -> True) 5 windows _ -> do
      for_ [(vid,you) | W.VillainActivationWouldBegin vid you <- windows, vid == villainId a] $ \(_,you) -> do
        members <- boardMembers
        chooseEffect you $ [targeting t (placeCounters t $ if toCardCode (getCardDef a) == "50165b" then 1 else 2) | t <- members]
          <> [labeled "$label.aosAdditionalBoost" (additionalActivationBoost (villainId a) you)]
      pure e
    WithDiscarded (isTarget a -> True) FromEncounterDeck cards -> do
      case headMay [c | EncounterCard c <- cards] of
        Nothing -> pure e
        Just card -> resolvePreparation card
    CardSpecific (isTarget a -> True) "aosPreparationGrunt" (fromJSON -> Success (you,card :: EncounterCard)) -> do
      for_ (aosAttackSource state) $ \source -> do
        let mid = MinionId $ unCardId $ ecCardId card
        redirectPlayerAttackToEnemy you source (EnemyVillainId $ villainId a) (EnemyMinionId mid)
      pure e
    CheckWindows windows | toCardCode (getCardDef a) `elem` ["50064","50065","50066"] -> do
      let completed = any (\case
            W.Window W.After (W.PlayerAttackResolved source _ _) -> Just source == aosAttackSource state
            W.Window W.After (W.AllyAttackCompletedAgainst aid _) -> Just (AllyRef aid) == aosAttackSource state
            _ -> False) windows
      if not completed then AosVillain . (`With` state) <$> runMessage msg a else do
        for_ (aosAfterAttack state) $ \(key,card) -> case key of
          "commando" -> for_ (aosAttacker state) $ \attacker -> do
            controller <- getSourceController $ toSource $ characterRef attacker
            for_ controller $ \you -> putEncounterCardIntoPlay you card
          "net" -> for_ (aosAttacker state) $ \attacker -> cardSpecific GameRef "aosAttachEncounterToCharacter" (card,attacker)
          "stun" -> traverse_ (`giveStatus` StunnedStatus) $ aosAttacker state
          _ -> pure ()
        pure $ AosVillain (a `With` state {aosAttackSource=Nothing,aosAttacker=Nothing,aosAfterAttack=[]})
    VillainPhaseStep step -> pure $ AosVillain (a `With` state {aosPhaseTwo = step == StepEnemiesActivate})
    BeginPhase _ -> pure $ AosVillain (a `With` state {aosPhaseTwo=False})
    _ -> AosVillain . (`With` state) <$> runMessage msg a
   where
    advanceTo card = do
      advanceVillain (villainId a)
      let AosVillain (updated `With` _) = advanceVillainTo card a
      pure $ AosVillain (updated `With` state)
    replaceDefeat = do
      cancelVillainDefeat (villainId a)
      first <- selectJust FirstPlayer
      case toCardCode (getCardDef a) of
        code | code `elem` ["50086a","50086b"] -> removeThreat first a 6 MainScheme
        code | code `elem` ["50103a","50103b"] -> do
          cells <- selectList (EnvironmentWithTitle "Holding Cell")
          if null cells then gameOver Won
          else chooseEffect first [targeting eid (removeCounters eid 2) | eid <- cells]
          attachments <- selectList (AttachmentOnEnemy $ EnemyVillainId $ villainId a)
          traverse_ (\aid -> cardSpecific aid "aosModokReset" ()) attachments
        _ -> removeSecrets first 3
      hp <- fromGameValue $ unHp $ villainStartingHp a
      pure $ AosVillain (a {villainHp=HP hp} `With` state)
    resolvePreparation card = do
      let code = toCardCode card
          printed = code `elem` ["50068","50069","50070","50071","50072","50073","50074","50075","50076","50077","50078","50079"] && code `notElem` ["50074","50075"]
      first <- selectJust FirstPlayer
      you <- maybe (pure first) (fmap (fromMaybe first) . getSourceController . toSource . characterRef) (aosAttacker state)
      let prevent = for_ (aosAttackSource state) $ \source -> villainStep (villainId a) $ VillainPreventAttackDamage source
          deferred key = do
            signalPreparation
            pure $ AosVillain (a `With` state {aosPreparationResolved=True,aosAfterAttack=aosAfterAttack state <> [(key,card)]})
      case code of
        "50068" -> do
          attachEncounterCard card (villainId a)
          cardSpecific (AttachmentId $ unCardId $ ecCardId card) "aosPreparationStatus" True
          mark
        "50069" -> do
          hand <- projectMap PlayerIdentityHand unHand you
          let events = filter (cardMatch $ CardWithType EventType) hand
          chooseEffect you [targeting c (discardCard c) | c <- events]
          mark
        "50070" -> attachEncounterCard card (villainId a) >> mark
        "50071" -> deferred "net"
        "50072" -> deferred "commando"
        "50073" -> do
          putEncounterCardIntoPlay you card
          cardSpecific a "aosPreparationGrunt" (you,card)
          mark
        "50076" -> do
          prevent
          difficulty <- getDifficulty
          when (difficulty == Expert) $ cardSpecific GameRef "aosReflectPendingAttack" (you,villainId a,aosAttacker state,aosAttackSource state)
          mark
        "50077" -> placeThreatOnEach a 1 AnyScheme >> mark
        "50078" -> do
          controlledCharacters you >>= traverse_ (\c -> damageCharacter c a $ toDamage 1 FromAbility)
          mark
        "50079" -> deferred "stun"
        _ -> do
          goggles <- selectList (AttachmentIs Cards.aos50070)
          unless (null goggles) $ prevent >> traverse_ removeFromPlay goggles
          automated <- selectAny (SideSchemeIs Cards.aos50074)
          when automated $ for_ (aosAttacker state) $ \c -> damageCharacter c a $ toDamage 1 FromAbility
          when (not (null goggles) || automated || printed) signalPreparation
          pure $ AosVillain (a `With` state {aosPreparationResolved = not (null goggles) || automated || printed})
    signalPreparation = selectList (AttachmentIs Cards.aos50068) >>= traverse_ (\aid -> cardSpecific aid "aosPreparationStatus" True)
    mark = do
      signalPreparation
      pure $ AosVillain (a `With` state {aosPreparationResolved=True})
