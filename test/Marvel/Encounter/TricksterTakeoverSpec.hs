module Marvel.Encounter.TricksterTakeoverSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Ability qualified as Ability
import Marvel.Cost qualified as Cost
import Marvel.Attachment.Types qualified as Attachment
import Marvel.EncounterSet qualified as Set
import Marvel.Villain.Types qualified as Villain
import Marvel.Hp (HP(..))
import Marvel.Entity
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Minion.Types qualified as Minion
import Marvel.Modifier
import Marvel.Phase
import Marvel.Query
import Marvel.Ref
import Marvel.Scenario.Types (Attrs (scenarioSetAsideCards, scenarioTrickster), TricksterState(..))
import Marvel.TricksterTakeover.Cards qualified as Cards
import Marvel.Trait qualified as Trait
import TestImport

takeover :: CardCode -> Difficulty -> IO TestGame
takeover code difficulty = do
  let setup = ScenarioSetup code [Set.TricksterMagic] difficulty
  (app, ran, logged) <- dealtGameLogged setup [starter "spider-man"] 1
  seats <- runGameApp app (getsGame gameSeats)
  let tg = TestGame app ran logged seats
  inGame tg toFirstTurn
  pure tg

neutral :: GameAppT TricksterState
neutral = getsGame $ fromMaybe (error "missing God of Lies state") . scenarioTrickster . toAttrs . gameScenario

enchantment :: GameAppT AttachmentId
enchantment = selectJust (AttachmentWithTrait Trait.Enchantment)

spec :: Spec
spec = describe "Trickster Takeover" do
  for_ [(Normal, "55001"), (Expert, "55002")] $ \(difficulty,code) ->
    it ("sets up Enchantress in " <> show difficulty) do
      tg <- takeover "55001" difficulty
      inGame tg do
        toCardCode . getCardDef <$> getVillain `shouldReturn` code
        mainSchemeThreatNow `shouldReturn` 1
        selectAny (IdentityWithTrait Trait.Defiant) `shouldReturn` True
        cards <- getsGame $ scenarioSetAsideCards . toAttrs . gameScenario
        length (filter ((`elem` ["55063", "55064", "55065", "55066"]) . toCardCode) cards) `shouldBe` 4
        length (filter ((== "55006") . toCardCode) cards) `shouldBe` 1

  it "flips the hidden enchantment on the fifth charm counter and substitutes threat afterward" do
    tg <- takeover "55001" Normal
    inGame tg do
      let p = player1 tg
      aid <- enchantment
      resolve $ replicate 5 $ CardSpecific GameRef "ttCharm" (toJSON p)
      selectAny (IdentityWithId p <> IdentityWithTrait Trait.Enthralled) `shouldReturn` True
      before <- mainSchemeThreatNow
      resolve [CardSpecific GameRef "ttCharm" $ toJSON p]
      mainSchemeThreatNow `shouldReturn` (before + 1)
      counters <- getsGame $ Attachment.attachmentCounters . toAttrs . (HashMap.! aid) . gameAttachments
      counters `shouldBe` 0

  it "advances Enchantress and puts Future of Despair into play with five threat" do
    tg <- takeover "55001" Normal
    inGame tg do
      vid <- villainIdNow
      resolve [VillainMessage vid VillainDefeated]
      toCardCode . getCardDef <$> getVillain `shouldReturn` "55002"
      sid <- selectJust (SideSchemeIs Cards.tt55006)
      getSchemeThreatAmount (SchemeSideSchemeId sid) `shouldReturn` 5
      mods <- getModifiers (VillainRef vid)
      mods `shouldSatisfy` elem CannotTakeDamage
      mods `shouldSatisfy` elem (KeywordModifier Stalwart)

  it "keeps Loki and Worlds Collide outside ordinary targets and starts all synergy environments" do
    tg <- takeover "55027a" Normal
    inGame tg do
      state <- neutral
      lokiHitPoints state `shouldBe` 20
      worldsCollideThreat state `shouldBe` 0
      selectListCount AnyVillain `shouldReturn` 1
      selectListCount EncounterMainScheme `shouldReturn` 1
      selectListCount (EnvironmentWithTrait Trait.Synergy) `shouldReturn` 4
      selectAny (AttachmentIs Cards.tt55034a) `shouldReturn` False

  it "swaps avatars preserving their dial, counters and status cards" do
    tg <- takeover "55027a" Normal
    inGame tg do
      vid <- villainIdNow
      before <- toCardCode . getCardDef <$> getVillain
      resolve [VillainMessage vid (VillainDamaged (toSource $ player1 tg) $ toDamage 3 FromAbility), PlaceCounters (VillainRef vid) 2, VillainMessage vid (VillainStunned $ toSource $ player1 tg)]
      hp <- (unHp . Villain.villainHp . toAttrs <$> getVillain)
      resolve [CardSpecific GameRef "ttSwapAvatar" $ toJSON False]
      after <- toCardCode . getCardDef <$> getVillain
      liftIO $ after `shouldNotBe` before
      (unHp . Villain.villainHp . toAttrs <$> getVillain) `shouldReturn` hp
      getCountersOn vid `shouldReturn` 2
      (Villain.villainStunned . toAttrs <$> getVillain) `shouldReturn` True

  it "shatters an avatar, damages true Loki, resets the dial and clears shatter counters" do
    tg <- takeover "55027a" Normal
    inGame tg do
      vid <- villainIdNow
      resolve [PlaceCounters (VillainRef vid) 2, VillainMessage vid VillainWouldBeDefeated]
      lokiHitPoints <$> neutral `shouldReturn` 13
      getCountersOn vid `shouldReturn` 0
      (unHp . Villain.villainHp . toAttrs <$> getVillain) `shouldReturn` 15

  it "resets Mischief and Mayhem when it fills and advances Worlds Collide" do
    tg <- takeover "55027a" Normal
    inGame tg do
      mid <- mainSchemeIdNow
      resolve [AdvanceMainScheme mid]
      mainSchemeThreatNow `shouldReturn` 0
      worldsCollideThreat <$> neutral `shouldReturn` 1

  it "caps synergy counters at the number of players" do
    tg <- takeover "55027a" Normal
    inGame tg do
      eid <- selectJust (EnvironmentIs Cards.tt55052)
      resolve [CardSpecific (toRef eid) "ttAddSynergy" $ toJSON (4 :: Natural)]
      getCountersOn eid `shouldReturn` 1

  for_ [(Cards.tt55038,3), (Cards.tt55039,2), (Cards.tt55040,2), (Cards.tt55041,3), (Cards.tt55042,3), (Cards.tt55043,3), (Cards.tt55044,3)] $ \(def,n) ->
    it ("earns shatter counters by defeating " <> show (toCardCode def)) do
      tg <- takeover "55027a" Normal
      inGame tg do
        mid <- addMinion (player1 tg) def
        when (toCardCode def == "55040") $ resolve [MinionMessage mid (MinionDamaged (toSource $ player1 tg) $ toDamage 1 FromAbility)]
        resolve [MinionMessage mid (MinionDamaged (toSource $ player1 tg) $ toDamage 20 FromAbility)]
        vid <- villainIdNow
        getCountersOn vid `shouldReturn` n

  for_ [(Cards.tt55056,"55063"), (Cards.tt55057,"55064"), (Cards.tt55058,"55065"), (Cards.tt55059,"55066")] $ \(def,code) ->
    it ("recruits the linked ally after defeating " <> show (toCardCode def)) do
      tg <- takeover "55001" Normal
      inGame tg do
        mid <- addMinion (player1 tg) def
        resolve [MinionMessage mid (MinionDamaged (toSource $ player1 tg) $ toDamage 20 FromAbility)]
        allies <- getsGame $ HashMap.elems . gameAllies
        liftIO $ any ((== code) . toCardCode . getCardDef) allies `shouldBe` True
        for_ (filter ((== code) . toCardCode . getCardDef) allies) $ \ally ->
          getModifiers ally >>= (`shouldSatisfy` elem NotCountedAgainstAllyLimit)

  it "Seduced prevents basic attacks and Attack events" do
    tg <- takeover "55001" Normal
    inGame tg do
      card <- encounterCard Cards.tt55015
      resolve [RevealEncounterCard (player1 tg) card]
      mods <- getModifiers $ player1 tg
      mods `shouldSatisfy` elem CannotBasicAttack
      mods `shouldSatisfy` elem (CannotPlayCards $ CardWithType EventType <> CardWithTrait Trait.Attack)

  for_ [(Cards.tt55021,2), (Cards.tt55022,4), (Cards.tt55045,4), (Cards.tt55046,7), (Cards.tt55047,5), (Cards.tt55048,6), (Cards.tt55060,4), (Cards.tt55061,2)] $ \(def,n) ->
    it ("places the printed threat on " <> show (toCardCode def)) do
      tg <- takeover "55027a" Normal
      inGame tg do
        card <- encounterCard def
        resolve [PutEncounterCardIntoPlay (player1 tg) card]
        let sid = sideSchemeOf card
        getSchemeThreatAmount (SchemeSideSchemeId sid) `shouldReturn` n

  it "Puppet Master gains hinder in expert mode and prevents ally thwarting and villain defense" do
    tg <- takeover "55027a" Expert
    inGame tg do
      card <- encounterCard Cards.tt55061
      resolve [PutEncounterCardIntoPlay (player1 tg) card]
      getSchemeThreatAmount (SchemeSideSchemeId $ sideSchemeOf card) `shouldReturn` 3

  it "Wrapped in Chains exhausts its identity and prevents readying" do
    tg <- takeover "55027a" Normal
    inGame tg do
      card <- encounterCard Cards.tt55035
      resolve [RevealEncounterCard (player1 tg) card]
      isExhaustedNow (player1 tg) `shouldReturn` True
      getModifiers (player1 tg) >>= (`shouldSatisfy` elem CannotReady)

  it "The Trickster Tango grants stalwart to the active villain" do
    tg <- takeover "55001" Normal
    inGame tg do
      card <- encounterCard Cards.tt55060
      resolve [PutEncounterCardIntoPlay (player1 tg) card]
      v <- villainIdNow
      getModifiers v >>= (`shouldSatisfy` elem (KeywordModifier Stalwart))

  it "Dirty Trick removes one shatter counter and deals normal-mode damage" do
    tg <- takeover "55027a" Normal
    inGame tg do
      v <- villainIdNow
      resolve [ReplaceVillain v "55029a" Swapped]
      card <- encounterCard Cards.tt55050
      before <- damageOf (player1 tg)
      resolve [PlaceCounters (VillainRef v) 3, RevealEncounterCard (player1 tg) card]
      getCountersOn v `shouldReturn` 2
      damageOf (player1 tg) `shouldReturn` (before + 2)

  it "flips true Loki at half health and reveals Intense Focus" do
    tg <- takeover "55027a" Normal
    inGame tg do
      resolve [CardSpecific GameRef "ttLokiDamage" $ toJSON (10 :: Natural)]
      lokiFlipped <$> neutral `shouldReturn` True
      selectAny (AttachmentIs Cards.tt55034a) `shouldReturn` True

  it "starts expert mode with Intense Focus and flips it to Total Focus at half health" do
    tg <- takeover "55027a" Expert
    inGame tg do
      selectAny (AttachmentIs Cards.tt55034a) `shouldReturn` True
      resolve [CardSpecific GameRef "ttLokiDamage" $ toJSON (10 :: Natural)]
      codes <- getsGame $ map (toCardCode . getCardDef) . HashMap.elems . gameAttachments
      codes `shouldSatisfy` elem "55034b"

  for_ [Cards.tt55012, Cards.tt55013, Cards.tt55014] $ \def ->
    it ("attaches " <> show (toCardCode def) <> " to its revealing identity and tempts them") do
      tg <- takeover "55001" Normal
      inGame tg do
        card <- encounterCard def
        resolve [RevealEncounterCard (player1 tg) card]
        let aid = AttachmentId $ unCardId $ ecCardId card
        attached <- getsGame $ Attachment.attachmentIdentity . toAttrs . (HashMap.! aid) . gameAttachments
        attached `shouldBe` Just (player1 tg)
        gaze <- enchantment
        resolve [RanAbility (player1 tg) (AttachmentRef aid) 1 [] NoPayment]
        getCountersOn gaze `shouldReturn` 1

  it "Magical Restraints stuns and places a charm counter while defiant" do
    tg <- takeover "55001" Normal
    inGame tg do
      card <- encounterCard Cards.tt55024
      resolve [RevealEncounterCard (player1 tg) card]
      isStunned (player1 tg) `shouldReturn` True
      aid <- enchantment
      getCountersOn aid `shouldReturn` 1

  it "Dark Arts reveals a discarded minion" do
    tg <- takeover "55027a" Normal
    inGame tg do
      void $ setEncounterDeck [Cards.tt55049] [Cards.tt55037]
      card <- encounterCard Cards.tt55049
      resolve [RevealEncounterCard (player1 tg) card]
      selectAny (MinionIs Cards.tt55037) `shouldReturn` True

  it "Do My Bidding searches for a minion" do
    tg <- takeover "55001" Normal
    inGame tg do
      void $ setEncounterDeck [Cards.tt55017, Cards.tt55022] []
      card <- encounterCard Cards.tt55023
      resolve [RevealEncounterCard (player1 tg) card]
      selectAny (MinionIs Cards.tt55017) `shouldReturn` True

  it "Law of Attraction reveals an enthralled minion, including its reveal effect" do
    tg <- takeover "55001" Normal
    inGame tg do
      void $ setEncounterDeck [Cards.tt55059, Cards.tt55021, Cards.tt55022] []
      card <- encounterCard Cards.tt55021
      resolve [RevealEncounterCard (player1 tg) card]
      selectAny (MinionIs Cards.tt55059) `shouldReturn` True

  it "Crown of the Enchantress attaches to Enchantress and grants expert stalwart" do
    tg <- takeover "55001" Expert
    inGame tg do
      card <- encounterCard Cards.tt55016
      resolve [RevealEncounterCard (player1 tg) card]
      v <- villainIdNow
      getModifiers v >>= (`shouldSatisfy` elem (KeywordModifier Stalwart))

  it "Dark Scepter grants stalwart and prevents treachery cancellation" do
    tg <- takeover "55027a" Normal
    inGame tg do
      card <- encounterCard Cards.tt55036
      resolve [RevealEncounterCard (player1 tg) card]
      v <- villainIdNow
      getModifiers v >>= (`shouldSatisfy` elem (KeywordModifier Stalwart))

  it "Ulik is tough when revealed in expert mode" do
    tg <- takeover "55001" Expert
    inGame tg do
      card <- encounterCard Cards.tt55020
      resolve [RevealEncounterCard (player1 tg) card]
      minions <- getsGame $ HashMap.elems . gameMinions
      let ulik = filter ((== "55020") . toCardCode . getCardDef) minions
      liftIO $ notNull ulik `shouldBe` True
      for_ ulik $ \m -> Minion.minionTough (toAttrs m) `shouldBe` True

  it "Draugr Buddy becomes an encounter card after its defeat without also remaining in the discard" do
    tg <- takeover "55027a" Normal
    inGame tg do
      mid <- addMinion (player1 tg) Cards.tt55037
      void $ setEncounterDeck [Cards.tt55050, Cards.tt55021] []
      resolve [MinionMessage mid (MinionDamaged (toSource $ player1 tg) $ toDamage 5 FromAbility)]
      selectAny (MinionWithId mid) `shouldReturn` False
      discard <- getEncounterDiscard
      liftIO $ all ((/= CardId (unMinionId mid)) . ecCardId) discard `shouldBe` True

  it "replaces identity defeat with alter-ego form, one hit point and Worlds Collide threat" do
    tg <- takeover "55027a" Normal
    inGame tg do
      resolve $ damageIdentity (player1 tg) GameRef (toDamage 30 FromAbility)
      selectAny (IdentityWithId (player1 tg) <> AlterEgoIdentity) `shouldReturn` True
      worldsCollideThreat <$> neutral `shouldReturn` 1
      (remaining,_,_) <- getRemainingHitPoints (player1 tg)
      remaining `shouldBe` 1

  it "Spell Shards finishes safely when its indirect damage eliminates its player" do
    tg <- testGameAgainst "55001" ["spider-man", "captain-marvel"]
    inGame tg do
      resolve [IdentityMessage (player1 tg) $ IdentitySetHitPoints 1]
      card <- encounterCard Cards.tt55026
      resolve [RevealEncounterCard (player1 tg) card]
      selectAny (IdentityWithId $ player1 tg) `shouldReturn` False

  it "performs an available Forced Action before ending the player phase and does not repeat an exhausted attachment" do
    tg <- takeover "55001" Normal
    inGame tg do
      card <- encounterCard Cards.tt55013
      resolve [RevealEncounterCard (player1 tg) card]
      let aid = AttachmentId $ unCardId $ ecCardId card
      gaze <- enchantment
      getsGame gamePhase `shouldReturn` PlayerPhase
      abilities <- getAbilitiesFor (player1 tg)
      let forced = filter ((== Ability.ForcedAction) . abilityType) abilities
      length forced `shouldBe` 1
      for_ forced $ \ab -> Cost.canAffordCost (player1 tg) (abilitySource ab) (abilityCost ab) `shouldReturn` True
      runNow [EndPhase PlayerPhase]
      getCountersOn gaze `shouldReturn` 0
      choose "the forced action" (isAbilityOf aid)
      getCountersOn gaze `shouldReturn` 1
      exhausted <- getsGame $ Attachment.attachmentExhausted . toAttrs . (HashMap.! aid) . gameAttachments
      exhausted `shouldBe` True
      Cost.canAffordCost (player1 tg) (toSource aid) Cost.ExhaustAttachmentCost `shouldReturn` False

  it "Trance of Greed takes damage instead of trying to play a drawn resource card" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      resource <- putInHand p "01090"
      next <- putInHand p "01089"
      withGame_ $ entitiesL . playersL . ix p %~ \(PlayerIdentity a) -> PlayerIdentity a
        {playerIdentityHand = Hand [], playerIdentityDeck = Deck [resource,next]}
      card <- encounterCard Cards.tt55008b
      resolve [RevealEncounterCard p card]
      before <- damageOf p
      resolve [RanAbility p (AttachmentRef $ AttachmentId $ unCardId $ ecCardId card) 1 [] NoPayment]
      damageOf p `shouldReturn` (before + 1)
      hand <- handOf p
      liftIO $ resource `elem` hand `shouldBe` True
