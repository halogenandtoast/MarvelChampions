module Marvel.Encounter.DeadpoolDreadpoolSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Data.List (nub)
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Attachment.Types qualified as Attachment
import Marvel.Entity (isTarget)
import Marvel.Attack
import Marvel.Effect.Types (Attrs (..), Effect (..), IsEffect (..))
import Marvel.EncounterSet (EncounterSet (DreadpoolSet))
import Marvel.Matchers
import Marvel.Message.Lifted qualified as Lifted
import Marvel.Minion.Cards qualified as Minions
import Marvel.Minion.Types qualified as Minion
import Marvel.Modifier
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Trait qualified as Trait
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Window qualified as W
import TestImport

settle :: GameAppT ()
settle = go (150 :: Int)
 where
  go 0 = error "Dreadpool fixture did not settle"
  go n = openQuestion >>= \case
    Just (_, QuestionLabel "$label.yourTurn" _ _) -> pure ()
    Just (you, q) -> do
      let passing c = isNoResponse c || isNoDefender c || isLabel "$label.declineToDefend" c
      answerQuestion you (fromMaybe 0 $ headMay [i | (i,c) <- zip [0..] (choicesOf q), passing c])
      go (n - 1)
    Nothing -> pure ()

toDefense :: GameAppT ()
toDefense = go (30 :: Int)
 where
  go 0 = error "attack did not reach defender selection"
  go n = asked >>= \case
    Just (_, choices) | any isNoDefender choices -> pure ()
    Just (you, _) -> answerQuestion you 0 >> go (n - 1)
    Nothing -> error "attack has no defender question"

attachmentOf :: EncounterCard -> AttachmentId
attachmentOf = AttachmentId . unCardId . ecCardId

minionAttrsOf :: MinionId -> GameAppT (Attrs Minion)
minionAttrsOf mid = getsGame $ toAttrs . fromMaybe (error "missing minion") . HashMap.lookup mid . gameMinions

attachRayToHero :: IdentityId -> GameAppT EncounterCard
attachRayToHero you = do
  card <- revealNow you Attachments.antiRegenerationRay
  settle
  clearHand you
  e <- putInHand you "01088"
  m <- putInHand you "01089"
  p <- putInHand you "01090"
  choose "take the Ray" (isAbilityOf $ attachmentOf card)
  payWith [e,m,p]
  settle
  pure card

data ProbeState = ProbeState
  { nestedTarget :: Maybe IdentityId
  , nestedStarted :: Bool
  , completions :: [Maybe Attack]
  , observedBlank :: [CharacterId]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype AttackProbe = AttackProbe (Attrs Effect `With` ProbeState)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance IsEffect AttackProbe where
  toEffectAttrs (AttackProbe (attrs `With` _)) = attrs

instance RunMessage AttackProbe where
  runMessage msg (AttackProbe (attrs `With` state)) = Lifted.runQueueT $ case msg of
    CheckWindows windows
      | not (nestedStarted state), Just other <- nestedTarget state
      , enemy : _ <- [enemy | W.Window W.When (W.EnemyAttack enemy _) <- windows] -> do
          Lifted.enemyAttacksWithCompletion enemy other attrs
          pure $ AttackProbe (attrs `With` state {nestedStarted = True})
    CardSpecific (isTarget attrs -> True) "attackCompleted" (fromJSON -> Success result) ->
      pure $ AttackProbe (attrs `With` state {completions = completions state <> [result]})
    IdentityMessage you (IdentityDamaged _ _) -> observe (IdentityCharacter you)
    AllyMessage aid (AllyDamaged _ _) -> observe (AllyCharacter aid)
    _ -> do
      updated <- runMessage msg attrs
      pure $ AttackProbe (updated `With` state)
   where
    observe character = do
      blank <- elem Blank <$> getModifiers (characterRef character)
      pure $ AttackProbe (attrs `With` state {observedBlank = observedBlank state <> [character | blank]})

addProbe :: Maybe IdentityId -> GameAppT EffectId
addProbe other = do
  eid <- getRandom
  let attrs = EffectAttrs eid "01039" GameRef (IdentityEntity AnyIdentity) [] Nothing
  withGame_ $ entitiesL . effectsL . at eid ?~ Effect (AttackProbe (attrs `With` ProbeState other False [] []))
  pure eid

probeState :: EffectId -> GameAppT ProbeState
probeState eid = do
  effect <- getsGame $ fromMaybe (error "missing probe") . HashMap.lookup eid . gameEffects
  case fromJSON (toJSON effect) of
    Success (AttackProbe (_ `With` state)) -> pure state
    Error err -> error err

spec :: Spec
spec = describe "Deadpool Dreadpool encounter set" do
  describe "Crisis of Infinite Deadpools" do
    it "reveals both set-aside cards, shuffles the rest, and removes itself" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        before <- filter ((== Just DreadpoolSet) . cdEncounterSet . getCardDef) <$> getSetAsideEncounterCards
        length before `shouldBe` 6
        card <- revealNow you Treacheries.crisisOfInfiniteDeadpools
        settle
        getSetAsideEncounterCards >>= (`shouldSatisfy` all ((/= Just DreadpoolSet) . cdEncounterSet . getCardDef))
        minions <- getsGame $ filter ((== "44038") . toCardCode . getCardDef) . HashMap.elems . gameMinions
        length minions `shouldBe` 1
        scheme <- getsGame $ fromMaybe (error "missing Dreadful Deeds") . headMay . filter ((== "44039") . toCardCode . getCardDef) . HashMap.elems . gameSideSchemes
        sideSchemeThreat (toId scheme) `shouldReturn` 2
        deck <- map toCardCode <$> getEncounterDeck
        for_ ["44040", "44041", "44042"] $ \code -> deck `shouldSatisfy` elem code
        encounterDiscardCodes >>= (`shouldSatisfy` notElem "44037")
        getsGame (HashMap.member (TreacheryId $ unCardId $ ecCardId card) . gameTreacheries) `shouldReturn` False

  describe "Dreadpool" do
    it "engages the first player and follows the first-player token" do
      tg <- testGame ["deadpool", "spider-man"]
      inGame tg do
        mid <- addMinion (player2 tg) Minions.dreadpool
        minionEngagedIdentity <$> minionAttrsOf mid `shouldReturn` player1 tg
        resolve [PassFirstPlayer]
        minionEngagedIdentity <$> minionAttrsOf mid `shouldReturn` player2 tg

    it "deals its exact physical card to the defeating player as a facedown encounter" do
      tg <- testGame ["deadpool", "spider-man"]
      inGame tg do
        let you = player1 tg
            other = player2 tg
        mid <- addMinion you Minions.dreadpool
        runNow [MinionMessage mid $ MinionDamaged (IdentityRef other) (toDamage 3 $ FromPlayerAttack other)]
        settle
        map ecCardId <$> dealtTo other >>= (`shouldSatisfy` elem (CardId $ unMinionId mid))
        map ecCardId <$> dealtTo you >>= (`shouldSatisfy` notElem (CardId $ unMinionId mid))
        minionDamageOf mid `shouldReturn` Nothing
        encounterDiscardCodes >>= (`shouldSatisfy` notElem "44038")

    it "is discarded normally when removed without being defeated" do
      tg <- testGame ["deadpool"]
      inGame tg do
        mid <- addMinion (player1 tg) Minions.dreadpool
        resolve [RemoveFromPlay $ MinionRef mid]
        map ecCardId <$> dealtTo (player1 tg) >>= (`shouldSatisfy` notElem (CardId $ unMinionId mid))
        encounterDiscardCodes >>= (`shouldSatisfy` elem "44038")

  describe "Dreadful Deeds" do
    it "adds two per player controlling pink cards, independent of how many each controls" do
      tg <- testGame ["deadpool", "spider-man"]
      inGame tg do
        let you = player1 tg
            other = player2 tg
        _ <- putIntoPlay you "44013"
        _ <- putIntoPlay you "44015"
        _ <- putIntoPlay other "44014"
        card <- revealNow you Schemes.dreadfulDeeds
        settle
        sideSchemeThreat (sideSchemeOf card) `shouldReturn` 6
        getAmplifyCount `shouldReturn` 2

    it "does not count pink cards merely in hand or in the deck" do
      tg <- testGame ["deadpool"]
      inGame tg do
        _ <- putInHand (player1 tg) "44013"
        card <- revealNow (player1 tg) Schemes.dreadfulDeeds
        settle
        sideSchemeThreat (sideSchemeOf card) `shouldReturn` 2

  describe "Anti-Regeneration Ray" do
    it "attaches to Dreadpool when available and otherwise to the villain" do
      for_ [False, True] $ \present -> do
        tg <- testGame ["deadpool"]
        inGame tg do
          let you = player1 tg
          host <- if present then Just <$> addMinion you Minions.dreadpool else pure Nothing
          ray <- revealNow you Attachments.antiRegenerationRay
          settle
          villain <- villainIdNow
          let expected = maybe (EnemyVillainId villain) EnemyMinionId host
          getsGame (fmap (Attachment.attachmentEnemy . toAttrs) . HashMap.lookup (attachmentOf ray) . gameAttachments) `shouldReturn` Just (Just expected)
          getModifiers (enemyRef expected) >>= (`shouldSatisfy` elem (AttackModifier 1))

    it "can be bought with energy, mental and physical resources and moved onto the hero" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        clearHand you
        changeForm you
        ray <- attachRayToHero you
        getsGame (fmap (Attachment.attachmentIdentity . toAttrs) . HashMap.lookup (attachmentOf ray) . gameAttachments) `shouldReturn` Just (Just you)
        getModifiers (IdentityRef you) >>= (`shouldSatisfy` elem AttacksBlankDefender)

    it "blanks a minion during the hero's basic attack, suppressing retaliate" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        clearHand you
        changeForm you
        _ <- attachRayToHero you
        mid <- addMinion you Minions.whiplash
        choose "basic attack" (isHeroAttack you)
        choose "attack Whiplash" (isTargetChoice mid)
        settle
        minionDamageOf mid `shouldReturn` Just 3
        damageOf you `shouldReturn` 0
        getModifiers (MinionRef mid) >>= (`shouldSatisfy` notElem Blank)

    it "never blanks a villain attacked by its hero bearer" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        clearHand you
        changeForm you
        _ <- attachRayToHero you
        choose "basic attack" (isHeroAttack you)
        chooseVillain
        settle
        villain <- villainIdNow
        getModifiers (VillainRef villain) >>= (`shouldSatisfy` notElem Blank)
        villainDamageNow `shouldReturn` 3

    it "blanks the actual defending ally rather than the original hero when damage lands" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        clearHand you
        changeForm you
        groot <- putIntoPlay you "16047"
        mid <- addMinion you Minions.dreadpool
        _ <- revealNow you Attachments.antiRegenerationRay
        settle
        probe <- addProbe Nothing
        runNow [MinionMessage mid $ MinionAttacks you]
        toDefense
        choose "Groot defends" (\case AllyDefend aid _ -> aid == allyIdOf groot; _ -> False)
        observedBlank <$> probeState probe >>= (`shouldSatisfy` elem (AllyCharacter $ allyIdOf groot))
        observedBlank <$> probeState probe >>= (`shouldSatisfy` notElem (IdentityCharacter you))
        settle
        map allyDamage <$> alliesWith "16047" `shouldReturn` [3]

    it "keeps the attack's blanking after the Ray leaves and prevents Deadpool's revival" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        clearHand you
        changeForm you
        mid <- addMinion you Minions.dreadpool
        ray <- revealNow you Attachments.antiRegenerationRay
        settle
        setDamage you 8
        runNow [MinionMessage mid $ MinionAttacks you]
        toDefense
        runNow [RemoveFromPlay $ AttachmentRef $ attachmentOf ray]
        getModifiers (IdentityRef you) >>= (`shouldSatisfy` elem Blank)
        choose "take the attack" isNoDefender
        settle
        getsGame gameState `shouldReturn` Finished Lost

  describe "'Pool-ized" do
    it "converts the highest-cost ally, preserves its damage/status, and uses printed THW as SCH" do
      tg <- testGame ["deadpool", "spider-man"]
      inGame tg do
        let you = player1 tg
            other = player2 tg
        _ <- putIntoPlay you "44013"
        fury <- putIntoPlay other "01084"
        resolve [AllyMessage (allyIdOf fury) $ AllyDamaged GameRef (toDamage 1 FromAbility)]
        resolve [GiveStatus (AllyRef $ allyIdOf fury) ConfusedStatus]
        card <- revealNow you Attachments.poolIzed
        settle
        let mid = MinionId $ unAllyId $ allyIdOf fury
        attrs <- minionAttrsOf mid
        minionEngagedIdentity attrs `shouldBe` other
        minionDamage attrs `shouldBe` 1
        minionConfused attrs `shouldBe` True
        Minion.getModifiedScheme attrs `shouldReturn` 2
        minionMatches (MinionWithTrait Trait.Pool) mid `shouldReturn` True
        getModifiers (MinionRef mid) >>= (`shouldSatisfy` elem Blank)
        resolve [RemoveFromPlay $ AttachmentRef $ attachmentOf card]
        minionDamageOf mid `shouldReturn` Nothing
        map allyDamage <$> alliesWith "01084" `shouldReturn` [1]

    it "surges and discards itself when no ally can be attached" do
      tg <- testGame ["deadpool"]
      inGame tg do
        before <- length <$> dealtTo (player1 tg)
        _ <- revealNow (player1 tg) Attachments.poolIzed
        settle
        length <$> dealtTo (player1 tg) `shouldReturn` before + 1
        encounterDiscardCodes >>= (`shouldSatisfy` elem "44041")

  describe "Metacidal Tendencies" do
    it "deals two to each Deadpool Corps character, or three while Dreadpool is in play" do
      for_ [False, True] $ \present -> do
        tg <- testGame ["deadpool"]
        inGame tg do
          let you = player1 tg
          clearHand you
          changeForm you
          when present $ void $ addMinion you Minions.dreadpool
          _ <- revealNow you Treacheries.metacidalTendencies
          settle
          damageOf you `shouldReturn` (if present then 3 else 2)
          getMainSchemeTokenCount `shouldReturn` 0

    it "adds an acceleration token when no Corps character is in play" do
      tg <- testGame ["spider-man"]
      inGame tg do
        _ <- revealNow (player1 tg) Treacheries.metacidalTendencies
        settle
        getMainSchemeTokenCount `shouldReturn` 1

    it "does not add a token when tough prevents the damage taken" do
      tg <- testGame ["deadpool"]
      inGame tg do
        _ <- putIntoPlay (player1 tg) "44013"
        _ <- revealNow (player1 tg) Treacheries.metacidalTendencies
        settle
        map allyDamage <$> alliesWith "44013" `shouldReturn` [0]
        map allyTough <$> alliesWith "44013" `shouldReturn` [False]
        getMainSchemeTokenCount `shouldReturn` 0

  describe "attack completion scope" do
    it "records separate final defenders for nested attacks by the same enemy" do
      tg <- testGame ["spider-man", "captain-marvel"]
      inGame tg do
        let you = player1 tg
            other = player2 tg
        clearHand you
        clearHand other
        mid <- addMinion you Minions.hydraMercenary
        probe <- addProbe (Just other)
        messages <- Lifted.runQueueT $ Lifted.capture $ Lifted.enemyAttacksWithCompletion mid you (EffectRef probe)
        runNow messages
        settle
        state <- probeState probe
        map (fmap attackCharacter) (completions state) `shouldBe` [Just $ IdentityCharacter other, Just $ IdentityCharacter you]
        let nonces = mapMaybe (>>= attackCompletionId) (completions state)
        length nonces `shouldBe` 2
        length (nub nonces) `shouldBe` 2
