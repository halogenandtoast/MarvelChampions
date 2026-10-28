-- | Ebony Maw (The Mad Titan's Shadow, scenario 1) and the Black Order and Armies of
-- Titan modular sets, each card played as a table would.
module Marvel.Encounter.EbonyMawSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Attachment.Cards qualified as AttachmentCards
import Marvel.Attachment.Types (Attachment, Attrs (..))
import Marvel.EncounterSet (EncounterSet (ArmiesOfTitan, BlackOrderSet))
import Marvel.Environment.Cards qualified as EnvironmentCards
import Marvel.Environment.Types qualified as Env
import Marvel.Hp (HP (..))
import Marvel.Keyword (Keyword (Guard, Quickstrike))
import Marvel.Keyword qualified as Keyword
import Marvel.Matchers (EnvironmentMatcher (EnvironmentInPlayAreaOf))
import Marvel.MainScheme.Cards qualified as MainSchemeCards
import Marvel.Minion.Cards qualified as MinionCards
import Marvel.Query (selectList)
import Marvel.SideScheme.Cards qualified as SideSchemeCards
import Marvel.SideScheme.Types qualified as SideScheme
import Marvel.Status (StatusCard (ToughStatus))
import Marvel.Trait (Trait (Spell))
import Marvel.Treachery.Cards qualified as TreacheryCards
import Marvel.Villain.Cards qualified as VillainCards
import Marvel.Villain.Types qualified as Villain
import TestImport

ebonyMaw :: [Text] -> IO TestGame
ebonyMaw = testGameAgainst "21071"

ebonyMawExpert :: [Text] -> IO TestGame
ebonyMawExpert heroes = do
  (app, ran, logged) <- dealtGameLogged (ScenarioSetup "21071" [BlackOrderSet, ArmiesOfTitan] Expert) (map starter heroes) 1
  seats <- runGameApp app (getsGame gameSeats)
  let tg = TestGame app ran logged seats
  inGame tg toFirstTurn
  pure tg

finished :: GameAppT (Maybe FinishedStatus)
finished = getsGame \g -> case gameState g of
  Finished status -> Just status
  _ -> Nothing

asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass

-- | The Spells in the player's play area, with their invocation counters.
spellsOf :: IdentityId -> GameAppT [(CardCode, Natural)]
spellsOf p = getsGame \g ->
  [ (toCardCode (getCardDef e), field Env.EnvironmentCounters e)
  | e <- HashMap.elems (gameEnvironments g)
  , field Env.EnvironmentOwner e == Just p
  ]

-- | Take away the Spells setup put into play, for a test to start from none.
clearSpells :: GameAppT ()
clearSpells = withGame_ $ entitiesL . environmentsL .~ mempty

-- | A Spell put into play in the player's area, with this many of its counters left.
spellWith :: IdentityId -> CardDef -> Natural -> GameAppT EnvironmentId
spellWith p def n = do
  card <- encounterCard def
  resolve [PutEncounterCardIntoPlay p card]
  let eid = EnvironmentId (unCardId (ecCardId card))
  full <- getsGame (maybe 0 (field Env.EnvironmentCounters) . HashMap.lookup eid . gameEnvironments)
  when (full > n) $ resolve [RemoveCounters (EnvironmentRef eid) (full - n)]
  pure eid

inDiscard :: CardDef -> GameAppT Bool
inDiscard def = elem (toCardCode def) <$> encounterDiscardCodes

attachmentsOf :: CardDef -> GameAppT [Attrs Attachment]
attachmentsOf def = getsGame (filter ((== toCardCode def) . toCardCode . attachmentCardDef) . map toAttrs . HashMap.elems . gameAttachments)

villainCode :: GameAppT CardCode
villainCode = toCardCode . getCardDef <$> getVillain

mainSchemeCode :: GameAppT CardCode
mainSchemeCode = getsGame (maybe (error "no main scheme") (toCardCode . getCardDef) . headMay . HashMap.elems . gameMainSchemes)

-- | An attack of the hero's on the villain, of this much damage.
heroAttackFor :: IdentityId -> Natural -> GameAppT ()
heroAttackFor you n = do
  vid <- villainIdNow
  runNow [Ask you (ChooseOne [DamageEnemy (EnemyRef (EnemyVillainId vid)) (toSource you) (toDamage n (FromPlayerAttack you))])]
  choose "the attack" (const True)
  resolve []

-- | Answer until the turn, declining responses and defenders.
settle :: GameAppT ()
settle = go (100 :: Int)
 where
  go 0 = pure ()
  go n =
    openQuestion >>= \case
      Just (_, QuestionLabel "$label.yourTurn" _ _) -> pure ()
      Just (ident, q) -> do
        let cs = choicesOf q
            passing = \case
              Label l _ -> l `elem` ["$label.noResponse", "$label.noDefender", "$label.declineToDefend"]
              _ -> False
        answerQuestion ident (fromMaybe 0 $ headMay [i | (i, c) <- zip [0 ..] cs, passing c])
        go (n - 1)
      Nothing -> pure ()

spec :: Spec
spec = do
  setupSpec
  villainSpec
  mainSchemeSpec
  spellSpec
  encounterSpec
  blackOrderSpec
  armiesOfTitanSpec

setupSpec :: Spec
setupSpec = describe "Ebony Maw: setup" do
  it "Ebony Maw (I) and Attack on Knowhere: each player puts a Spell into play in their area, without surge; the discard pile is shuffled back" do
    tg <- ebonyMaw ["spider-man", "captain-marvel"]
    let (p1, p2) = (player1 tg, player2 tg)
    inGame tg do
      code <- villainCode
      scheme <- mainSchemeCode
      s1 <- spellsOf p1
      s2 <- spellsOf p2
      discard <- encounterDiscard
      threat <- mainSchemeThreatNow
      liftIO do
        code `shouldBe` toCardCode VillainCards.ebonyMaw1
        scheme `shouldBe` toCardCode MainSchemeCards.attackOnKnowhere
        (length s1, length s2) `shouldBe` (1, 1)
        discard `shouldBe` []
        -- 1 per player
        threat `shouldBe` 2

  it "expert: Ebony Maw (II), whose When Revealed gives each player a second Spell" do
    tg <- ebonyMawExpert ["spider-man"]
    let p = player1 tg
    inGame tg do
      code <- villainCode
      spells <- spellsOf p
      liftIO do
        code `shouldBe` toCardCode VillainCards.ebonyMaw2
        length spells `shouldBe` 2

  it "the encounter deck is the Ebony Maw, Black Order, Armies of Titan and standard sets" do
    tg <- ebonyMaw ["spider-man"]
    inGame tg do
      deck <- map toCardCode <$> scenarioEncounterDeckNow
      inPlay <- getsGame (map (toCardCode . getCardDef) . HashMap.elems . gameEnvironments)
      let codes = deck <> inPlay
      liftIO do
        codes `shouldSatisfy` elem (toCardCode TreacheryCards.agentOfThanos)
        codes `shouldSatisfy` elem (toCardCode MinionCards.blackDwarf)
        codes `shouldSatisfy` elem (toCardCode MinionCards.outrider)
        length (filter (`elem` map toCardCode [EnvironmentCards.fireball, EnvironmentCards.manipulation, EnvironmentCards.pacification, EnvironmentCards.rubblestorm]) codes) `shouldBe` 8

villainSpec :: Spec
villainSpec = describe "Ebony Maw (villain)" do
  it "Ebony Maw (I) defeated: Ebony Maw (II), 18 hit points per player, gives each player a Spell; (II) defeated wins" do
    tg <- ebonyMaw ["spider-man"]
    let p = player1 tg
    inGame tg do
      clearSpells
      _ <- stackEncounterDeck [MinionCards.hydraMercenary, EnvironmentCards.manipulation]
      vid <- villainIdNow
      resolve [VillainMessage vid VillainDefeated]
      pass
      v <- getVillain
      spells <- spellsOf p
      merc <- inDiscard MinionCards.hydraMercenary
      liftIO do
        toCardCode (getCardDef v) `shouldBe` toCardCode VillainCards.ebonyMaw2
        Villain.villainHp (toAttrs v) `shouldBe` HP 18
        spells `shouldBe` [(toCardCode EnvironmentCards.manipulation, 2)]
        merc `shouldBe` True
      resolve [VillainMessage vid VillainDefeated]
      finished `shouldReturn` Just Won

  it "expert: Ebony Maw (II) defeated: Ebony Maw (III), 23 hit points per player, and another Spell" do
    tg <- ebonyMawExpert ["spider-man"]
    let p = player1 tg
    inGame tg do
      clearSpells
      _ <- stackEncounterDeck [EnvironmentCards.fireball]
      vid <- villainIdNow
      resolve [VillainMessage vid VillainDefeated]
      pass
      v <- getVillain
      spells <- spellsOf p
      liftIO do
        toCardCode (getCardDef v) `shouldBe` toCardCode VillainCards.ebonyMaw3
        Villain.villainHp (toAttrs v) `shouldBe` HP 23
        spells `shouldBe` [(toCardCode EnvironmentCards.fireball, 4)]

  it "attacking you, he first removes an invocation counter from each Spell in your play area" do
    tg <- ebonyMaw ["spider-man"]
    let p = player1 tg
    inGame tg do
      asHero p
      clearSpells
      _ <- spellWith p EnvironmentCards.fireball 1
      _ <- spellWith p EnvironmentCards.rubblestorm 3
      _ <- stackEncounterDeck [MinionCards.hydraMercenary]
      vid <- villainIdNow
      resolve [VillainMessage vid (VillainAttacks p)]
      pass
      spells <- spellsOf p
      fire <- inDiscard EnvironmentCards.fireball
      dmg <- damageOf p
      liftIO do
        spells `shouldBe` [(toCardCode EnvironmentCards.rubblestorm, 2)]
        fire `shouldBe` True
        -- Fireball's 4, and his 1 ATK with the Mercenary's boost icon
        dmg `shouldBe` 6

  it "scheming in your alter-ego's villain phase, he removes them too" do
    tg <- ebonyMaw ["spider-man"]
    let p = player1 tg
    inGame tg do
      clearSpells
      _ <- spellWith p EnvironmentCards.pacification 3
      _ <- stackEncounterDeck [MinionCards.hydraMercenary]
      vid <- villainIdNow
      resolve [VillainMessage vid VillainSchemes]
      spellsOf p `shouldReturn` [(toCardCode EnvironmentCards.pacification, 2)]

  it "stunned, he removes his stun instead: no counter is removed; confused, the same" do
    tg <- ebonyMaw ["spider-man"]
    let p = player1 tg
    inGame tg do
      asHero p
      clearSpells
      _ <- spellWith p EnvironmentCards.fireball 4
      vid <- villainIdNow
      resolve [VillainMessage vid (VillainStunned (toSource p)), VillainMessage vid (VillainAttacks p)]
      pass
      afterStun <- spellsOf p
      resolve [VillainMessage vid (VillainConfused (toSource p)), VillainMessage vid VillainSchemes]
      afterConfuse <- spellsOf p
      v <- toAttrs <$> getVillain
      liftIO do
        afterStun `shouldBe` [(toCardCode EnvironmentCards.fireball, 4)]
        afterConfuse `shouldBe` [(toCardCode EnvironmentCards.fireball, 4)]
        (Villain.villainStunned v, Villain.villainConfused v) `shouldBe` (False, False)

  it "only your Spells: another player's keep their counters" do
    tg <- ebonyMaw ["spider-man", "captain-marvel"]
    let (p1, p2) = (player1 tg, player2 tg)
    inGame tg do
      clearSpells
      _ <- spellWith p1 EnvironmentCards.fireball 4
      _ <- spellWith p2 EnvironmentCards.fireball 4
      _ <- stackEncounterDeck [MinionCards.hydraMercenary]
      vid <- villainIdNow
      resolve [VillainMessage vid VillainSchemes]
      s1 <- spellsOf p1
      s2 <- spellsOf p2
      liftIO $ (s1, s2) `shouldBe` ([(toCardCode EnvironmentCards.fireball, 3)], [(toCardCode EnvironmentCards.fireball, 4)])

  it "scheming in player 2's activation, he removes counters from player 2's Spells only" do
    tg <- ebonyMaw ["spider-man", "captain-marvel"]
    let (p1, p2) = (player1 tg, player2 tg)
    inGame tg do
      clearSpells
      _ <- spellWith p1 EnvironmentCards.fireball 4
      _ <- spellWith p2 EnvironmentCards.fireball 4
      _ <- stackEncounterDeck [MinionCards.hydraMercenary]
      withGame_ $ activePlayerL .~ p2
      vid <- villainIdNow
      resolve [VillainMessage vid VillainSchemes]
      s1 <- spellsOf p1
      s2 <- spellsOf p2
      liftIO $ (s1, s2) `shouldBe` ([(toCardCode EnvironmentCards.fireball, 4)], [(toCardCode EnvironmentCards.fireball, 3)])

  it "two Spells losing their last counter at once: the first player chooses the order, whoever's Spells they are" do
    tg <- ebonyMaw ["spider-man", "captain-marvel"]
    let (first, p) = (player1 tg, player2 tg)
    inGame tg do
      clearSpells
      fire <- spellWith p EnvironmentCards.fireball 1
      manip <- spellWith p EnvironmentCards.manipulation 1
      _ <- stackEncounterDeck [MinionCards.hydraMercenary]
      hand <- length <$> handOf p
      withGame_ $ activePlayerL .~ p
      vid <- villainIdNow
      runNow [VillainMessage vid VillainSchemes]
      Just (who, _) <- asked
      ordered <- offers (isTargetChoice (EnvironmentRef fire))
      choose "Fireball first" (isTargetChoice (EnvironmentRef fire))
      -- then the one left, still the first player's to pick
      chooseIf (isTargetChoice (EnvironmentRef manip))
      resolve []
      spells <- spellsOf p
      dmg <- damageOf p
      hand' <- length <$> handOf p
      confused <- isConfused p
      liftIO do
        (who, ordered) `shouldBe` (first, True)
        spells `shouldBe` []
        (dmg, hand - hand', confused) `shouldBe` (4, 1, True)

mainSchemeSpec :: Spec
mainSchemeSpec = describe "Ebony Maw (main schemes)" do
  it "Attack on Knowhere 1B: each player in turn discards to a Spell and puts it into play; then the discard pile is shuffled back" do
    tg <- ebonyMaw ["spider-man", "captain-marvel"]
    let (p1, p2) = (player1 tg, player2 tg)
    inGame tg do
      clearSpells
      _ <-
        setEncounterDeck
          [MinionCards.hydraMercenary, EnvironmentCards.fireball, MinionCards.hydraMercenary, EnvironmentCards.rubblestorm, MinionCards.hydraMercenary]
          []
      m <- mainSchemeIdNow
      resolve [MainSchemeMessage m RevealMainScheme]
      s1 <- spellsOf p1
      s2 <- spellsOf p2
      discard <- encounterDiscard
      deck <- map toCardCode <$> scenarioEncounterDeckNow
      liftIO do
        s1 `shouldBe` [(toCardCode EnvironmentCards.fireball, 4)]
        s2 `shouldBe` [(toCardCode EnvironmentCards.rubblestorm, 3)]
        discard `shouldBe` []
        deck `shouldBe` replicate 3 (toCardCode MinionCards.hydraMercenary)

  it "a Spell put into play is not revealed: no surge" do
    tg <- ebonyMaw ["spider-man"]
    let p = player1 tg
    inGame tg do
      clearSpells
      _ <- setEncounterDeck [EnvironmentCards.fireball, MinionCards.hydraMercenary] []
      dealt <- length <$> dealtTo p
      m <- mainSchemeIdNow
      resolve [MainSchemeMessage m RevealMainScheme]
      dealt' <- length <$> dealtTo p
      minions <- getsGame (length . gameMinions)
      liftIO $ (dealt' - dealt, minions) `shouldBe` (0, 0)

  it "with no Spell left in the encounter deck, the deck is discarded and the player gets none" do
    tg <- ebonyMaw ["spider-man"]
    let p = player1 tg
    inGame tg do
      clearSpells
      _ <- setEncounterDeck [MinionCards.hydraMercenary, MinionCards.hydraMercenary] []
      resolve [DiscardUntilIntoPlay p (CardWithType EnvironmentType <> CardWithTrait Spell)]
      spellsOf p `shouldReturn` []

  it "1B completed: The Power Stone 2A shuffles the discard pile back, then each player puts a Spell into play" do
    tg <- ebonyMaw ["spider-man"]
    let p = player1 tg
    inGame tg do
      clearSpells
      _ <- setEncounterDeck [MinionCards.hydraMercenary] [EnvironmentCards.pacification, MinionCards.hydraMercenary]
      m <- mainSchemeIdNow
      resolve [AdvanceMainScheme m]
      pass
      scheme <- mainSchemeCode
      spells <- spellsOf p
      left <- (+) <$> (length <$> encounterDiscard) <*> (length <$> scenarioEncounterDeckNow)
      liftIO do
        scheme `shouldBe` toCardCode MainSchemeCards.thePowerStoneScheme
        spells `shouldBe` [(toCardCode EnvironmentCards.pacification, 3)]
        left `shouldBe` 2

  it "The Power Stone completed: the players lose" do
    tg <- ebonyMaw ["spider-man"]
    inGame tg do
      m <- mainSchemeIdNow
      resolve [AdvanceMainScheme m]
      pass
      m2 <- mainSchemeIdNow
      resolve [AdvanceMainScheme m2]
      finished `shouldReturn` Just Lost

spellSpec :: Spec
spellSpec = describe "Ebony Maw's Spells" do
  it "Fireball revealed goes into the revealing player's area with 4 invocation counters, and surges" do
    tg <- ebonyMaw ["spider-man", "captain-marvel"]
    let (p1, p2) = (player1 tg, player2 tg)
    inGame tg do
      clearSpells
      _ <- stackEncounterDeck [MinionCards.hydraMercenary]
      dealt <- length <$> dealtTo p2
      _ <- revealNow p2 EnvironmentCards.fireball
      pass
      dealt' <- length <$> dealtTo p2
      s1 <- spellsOf p1
      s2 <- spellsOf p2
      mine <- selectList (EnvironmentInPlayAreaOf p2)
      liftIO do
        (s1, s2) `shouldBe` ([], [(toCardCode EnvironmentCards.fireball, 4)])
        length mine `shouldBe` 1
        dealt' - dealt `shouldBe` 1
        Keyword.Surge `member` cdKeywords EnvironmentCards.fireball `shouldBe` True

  it "Fireball: after its last counter is removed, it is discarded and deals 4 damage to your identity; not before" do
    tg <- ebonyMaw ["spider-man"]
    let p = player1 tg
    inGame tg do
      clearSpells
      fire <- spellWith p EnvironmentCards.fireball 4
      resolve [RemoveCounters (EnvironmentRef fire) 3]
      before <- damageOf p
      resolve [RemoveCounters (EnvironmentRef fire) 1]
      after <- damageOf p
      gone <- inDiscard EnvironmentCards.fireball
      liftIO $ (before, after, gone) `shouldBe` (0, 4, True)

  it "Manipulation (2 counters): discard 1 card at random from your hand, and you are confused" do
    tg <- ebonyMaw ["spider-man"]
    let p = player1 tg
    inGame tg do
      clearSpells
      e <- spellWith p EnvironmentCards.manipulation 2
      hand <- length <$> handOf p
      resolve [RemoveCounters (EnvironmentRef e) 2]
      hand' <- length <$> handOf p
      confused <- isConfused p
      liftIO $ (hand - hand', confused) `shouldBe` (1, True)

  it "Pacification (3 counters): exhaust each upgrade you control, and you are stunned" do
    tg <- ebonyMaw ["spider-man"]
    let p = player1 tg
    inGame tg do
      clearSpells
      vest <- putIntoPlay p "01081"
      e <- spellWith p EnvironmentCards.pacification 3
      resolve [RemoveCounters (EnvironmentRef e) 3]
      exhausted <- upgradeIsExhausted (upgradeOf vest)
      stunned <- isStunned p
      liftIO $ (exhausted, stunned) `shouldBe` (True, True)

  it "Rubblestorm (3 counters): 2 damage to each character you control" do
    tg <- ebonyMaw ["spider-man"]
    let p = player1 tg
    inGame tg do
      clearSpells
      _ <- putIntoPlay p "01020"
      e <- spellWith p EnvironmentCards.rubblestorm 3
      resolve [RemoveCounters (EnvironmentRef e) 3]
      dmg <- damageOf p
      hellcat <- allyIn "01020"
      liftIO $ (dmg, hellcat.allyDamage) `shouldBe` (2, 2)

  it "a player eliminated has their Spells discarded" do
    tg <- ebonyMaw ["spider-man", "captain-marvel"]
    let p2 = player2 tg
    inGame tg do
      clearSpells
      _ <- spellWith p2 EnvironmentCards.fireball 4
      resolve [IdentityMessage p2 IdentityDefeated]
      left <- getsGame (length . gameEnvironments)
      gone <- inDiscard EnvironmentCards.fireball
      liftIO $ (left, gone) `shouldBe` (0, True)

encounterSpec :: Spec
encounterSpec = describe "Ebony Maw (encounter set)" do
  it "Agent of Thanos (hero): 1 damage for each Spell in your play area, no surge" do
    tg <- ebonyMaw ["spider-man"]
    let p = player1 tg
    inGame tg do
      asHero p
      clearSpells
      _ <- spellWith p EnvironmentCards.fireball 4
      _ <- spellWith p EnvironmentCards.rubblestorm 3
      dealt <- length <$> dealtTo p
      _ <- revealNow p TreacheryCards.agentOfThanos
      pass
      dmg <- damageOf p
      dealt' <- length <$> dealtTo p
      liftIO $ (dmg, dealt' - dealt) `shouldBe` (2, 0)

  it "Agent of Thanos (alter-ego): 1 threat on the main scheme for each" do
    tg <- ebonyMaw ["spider-man"]
    let p = player1 tg
    inGame tg do
      clearSpells
      _ <- spellWith p EnvironmentCards.fireball 4
      _ <- spellWith p EnvironmentCards.rubblestorm 3
      before <- mainSchemeThreatNow
      _ <- revealNow p TreacheryCards.agentOfThanos
      pass
      after <- mainSchemeThreatNow
      liftIO $ after - before `shouldBe` 2

  it "Agent of Thanos with no Spell in your play area gains surge" do
    tg <- ebonyMaw ["spider-man"]
    let p = player1 tg
    inGame tg do
      clearSpells
      _ <- stackEncounterDeck [MinionCards.hydraMercenary]
      dealt <- length <$> dealtTo p
      _ <- revealNow p TreacheryCards.agentOfThanos
      pass
      dealt' <- length <$> dealtTo p
      liftIO $ dealt' - dealt `shouldBe` 1

  it "Channeling Trance: removes 1 invocation counter from each of your Spells" do
    tg <- ebonyMaw ["spider-man"]
    let p = player1 tg
    inGame tg do
      clearSpells
      _ <- spellWith p EnvironmentCards.fireball 4
      _ <- spellWith p EnvironmentCards.pacification 3
      _ <- revealNow p TreacheryCards.channelingTrance
      pass
      spellsOf p >>= (`shouldMatchList` [(toCardCode EnvironmentCards.fireball, 3), (toCardCode EnvironmentCards.pacification, 2)])

  it "Channeling Trance with no Spell: discard to a Spell environment and put it into play in your area" do
    tg <- ebonyMaw ["spider-man"]
    let p = player1 tg
    inGame tg do
      clearSpells
      _ <- stackEncounterDeck [MinionCards.hydraMercenary, EnvironmentCards.rubblestorm, MinionCards.hydraMercenary]
      dealt <- length <$> dealtTo p
      _ <- revealNow p TreacheryCards.channelingTrance
      pass
      spells <- spellsOf p
      dealt' <- length <$> dealtTo p
      merc <- inDiscard MinionCards.hydraMercenary
      liftIO $ (spells, dealt' - dealt, merc) `shouldBe` ([(toCardCode EnvironmentCards.rubblestorm, 3)], 0, True)

  it "Abjuration attaches to Ebony Maw and prevents all damage to him; 2 or more from a single attack discards it" do
    tg <- ebonyMaw ["spider-man"]
    let p = player1 tg
    inGame tg do
      _ <- revealNow p AttachmentCards.abjuration
      pass
      vid <- villainIdNow
      [a] <- attachmentsOf AttachmentCards.abjuration
      heroAttackFor p 1
      small <- villainDamageNow
      afterSmall <- length <$> attachmentsOf AttachmentCards.abjuration
      resolve [VillainMessage vid (VillainDamaged (toSource p) (toDamage 3 FromAbility))]
      notAttack <- villainDamageNow
      afterAbility <- length <$> attachmentsOf AttachmentCards.abjuration
      heroAttackFor p 3
      big <- villainDamageNow
      afterBig <- length <$> attachmentsOf AttachmentCards.abjuration
      gone <- inDiscard AttachmentCards.abjuration
      liftIO do
        attachmentEnemy a `shouldBe` Just (EnemyVillainId vid)
        (small, afterSmall) `shouldBe` (0, 1)
        (notAttack, afterAbility) `shouldBe` (0, 1)
        (big, afterBig, gone) `shouldBe` (0, 0, True)

  it "Abjuration prevents the damage before a tough status card would: he stays tough" do
    tg <- ebonyMaw ["spider-man"]
    let p = player1 tg
    inGame tg do
      vid <- villainIdNow
      resolve [VillainMessage vid VillainBecomeTough]
      _ <- revealNow p AttachmentCards.abjuration
      pass
      heroAttackFor p 1
      tough <- Villain.villainIsTough <$> getVillain
      liftIO $ tough `shouldBe` True

  it "Restrained attaches to the friendly character with the highest ATK and exhausts it: an ally, which cannot ready" do
    tg <- ebonyMaw ["spider-man"]
    let p = player1 tg
    inGame tg do
      -- an alter-ego has no ATK: Hellcat is the only character with one
      _ <- putIntoPlay p "01020"
      _ <- revealNow p AttachmentCards.restrained
      pass
      hellcat <- allyIn "01020"
      [r] <- attachmentsOf AttachmentCards.restrained
      resolve [IdentityMessage p ReadyCards]
      stillExhausted <- (.allyExhausted) <$> allyIn "01020"
      liftIO do
        r.attachmentAlly `shouldBe` Just hellcat.allyId
        hellcat.allyExhausted `shouldBe` True
        stillExhausted `shouldBe` True

  it "Restrained on an ally: its controller's hero spends [energy] [physical] to discard it" do
    tg <- ebonyMaw ["spider-man"]
    let p = player1 tg
    inGame tg do
      _ <- putIntoPlay p "01020"
      _ <- revealNow p AttachmentCards.restrained
      pass
      [r] <- attachmentsOf AttachmentCards.restrained
      inAlterEgo <- offers (isAbilityOf (AttachmentRef r.attachmentId))
      asHero p
      clearHand p
      energy <- putInHand p "01088"
      strength <- putInHand p "01090"
      choose "discard Restrained" (isAbilityOf (AttachmentRef r.attachmentId))
      payWith [energy, strength]
      pass
      left <- attachmentsOf AttachmentCards.restrained
      gone <- inDiscard AttachmentCards.restrained
      liftIO $ (inAlterEgo, length left, gone) `shouldBe` (False, 0, True)

  it "Restrained leaves play with its ally" do
    tg <- ebonyMaw ["spider-man"]
    let p = player1 tg
    inGame tg do
      _ <- putIntoPlay p "01020"
      _ <- revealNow p AttachmentCards.restrained
      pass
      hellcat <- allyIn "01020"
      resolve [AllyMessage hellcat.allyId AllyDefeated]
      left <- attachmentsOf AttachmentCards.restrained
      gone <- inDiscard AttachmentCards.restrained
      liftIO $ (length left, gone) `shouldBe` (0, True)

  it "Restrained on the hero with the highest ATK: exhausted, and does not ready" do
    tg <- ebonyMaw ["spider-man"]
    let p = player1 tg
    inGame tg do
      asHero p
      _ <- revealNow p AttachmentCards.restrained
      pass
      [r] <- attachmentsOf AttachmentCards.restrained
      exhausted <- exhaustedNow p
      resolve [IdentityMessage p ReadyCards]
      still <- exhaustedNow p
      liftIO $ (r.attachmentIdentity, exhausted, still) `shouldBe` (Just p, True, True)

  it "Restrained: a tie for the highest ATK is the revealing player's choice" do
    tg <- ebonyMaw ["spider-man"]
    let p = player1 tg
    inGame tg do
      asHero p
      -- Spider-Woman's 2 ATK, as Spider-Man's
      _ <- putIntoPlay p "01011"
      woman <- allyIn "01011"
      _ <- revealNow p AttachmentCards.restrained
      heroOffered <- offers (isTargetChoice (IdentityRef p))
      choose "Spider-Woman" (isTargetChoice (AllyRef woman.allyId))
      pass
      [r] <- attachmentsOf AttachmentCards.restrained
      liftIO $ (heroOffered, r.attachmentAlly) `shouldBe` (True, Just woman.allyId)

  it "Restrained with no hero or ally in play is discarded" do
    tg <- ebonyMaw ["spider-man"]
    let p = player1 tg
    inGame tg do
      _ <- revealNow p AttachmentCards.restrained
      pass
      left <- attachmentsOf AttachmentCards.restrained
      gone <- inDiscard AttachmentCards.restrained
      liftIO $ (length left, gone) `shouldBe` (0, True)

  it "Reactor Overload: 3 threat, a crisis icon; each player takes 2 damage or places 2 threat here" do
    tg <- ebonyMaw ["spider-man", "captain-marvel"]
    let (p1, p2) = (player1 tg, player2 tg)
    inGame tg do
      card <- revealNow p1 SideSchemeCards.reactorOverload
      choose "take 2 damage" (isLabel (countLabel "takeDamage" 2))
      choose "place 2 threat" (isLabel (countLabel "placeThreatOnReactorOverload" 2))
      pass
      threat <- sideSchemeThreat (sideSchemeOf card)
      d1 <- damageOf p1
      d2 <- damageOf p2
      crisis <- getsGame (fmap (field SideScheme.SideSchemeCrisis) . HashMap.lookup (sideSchemeOf card) . gameSideSchemes)
      liftIO $ (threat, d1, d2, crisis) `shouldBe` (5, 2, 0, Just True)

blackOrderSpec :: Spec
blackOrderSpec = describe "Black Order (modular set)" do
  it "Black Dwarf's attack gains overkill: past a defending ally to the hero" do
    tg <- ebonyMaw ["spider-man"]
    let p = player1 tg
    inGame tg do
      m <- addMinion p MinionCards.blackDwarf
      asHero p
      _ <- putIntoPlay p "01002"
      pass
      cat <- allyIn "01002"
      start [MinionMessage m (MinionAttacks p)]
      chooseIf isNoResponse
      choose "Black Cat defends" \case
        AllyDefend a _ -> a == allyId cat
        _ -> False
      settle
      -- 3 ATK on Black Cat's 2 hit points
      damageOf p `shouldReturn` 1

  it "Supergiant: quickstrike; after it attacks and damages a character, that character is stunned" do
    tg <- ebonyMaw ["spider-man"]
    let p = player1 tg
    inGame tg do
      asHero p
      _ <- revealNow p MinionCards.supergiant
      settle
      dmg <- damageOf p
      stunned <- isStunned p
      liftIO do
        (dmg, stunned) `shouldBe` (2, True)
        Quickstrike `member` cdKeywords MinionCards.supergiant `shouldBe` True

  it "Supergiant's attack that deals no damage stuns no one" do
    tg <- ebonyMaw ["spider-man"]
    let p = player1 tg
    inGame tg do
      asHero p
      resolve [GiveStatus (IdentityRef p) ToughStatus]
      _ <- revealNow p MinionCards.supergiant
      settle
      dmg <- damageOf p
      stunned <- isStunned p
      liftIO $ (dmg, stunned) `shouldBe` (0, False)

  it "The Black Order: 2 threat per player, a hazard icon; no threat is removed while a Black Order minion is in play" do
    tg <- ebonyMaw ["spider-man"]
    let p = player1 tg
    inGame tg do
      card <- revealNow p SideSchemeCards.theBlackOrder
      pass
      let s = sideSchemeOf card
      m <- addMinion p MinionCards.outrider
      resolve [SideSchemeMessage s (SideSchemeThwarted (IdentityRef p) 1)]
      locked <- sideSchemeThreat s
      resolve [RemoveFromPlay (MinionRef m)]
      resolve [SideSchemeMessage s (SideSchemeThwarted (IdentityRef p) 1)]
      freed <- sideSchemeThreat s
      liftIO do
        (locked, freed) `shouldBe` (2, 1)
        cdHazards SideSchemeCards.theBlackOrder `shouldBe` 1

  it "Blood to Spare: each minion engaged with a player activates against them; those not engaged search for a Black Order minion" do
    tg <- ebonyMaw ["spider-man", "captain-marvel"]
    let (p1, p2) = (player1 tg, player2 tg)
    inGame tg do
      asHero p1
      m <- addMinion p1 MinionCards.outrider
      _ <- stackEncounterDeck [MinionCards.blackDwarf]
      before <- damageOf p1
      _ <- revealNow p1 TreacheryCards.bloodToSpare
      settle
      after <- damageOf p1
      engaged2 <- getsGame (map (toCardCode . getCardDef) . filter ((== p2) . minionEngagedIdentity . toAttrs) . HashMap.elems . gameMinions)
      engaged1 <- getsGame (map toId . filter ((== p1) . minionEngagedIdentity . toAttrs) . HashMap.elems . gameMinions)
      liftIO do
        -- the Outrider's 1 ATK
        after - before `shouldBe` 1
        engaged1 `shouldBe` [m]
        engaged2 `shouldSatisfy` elem (toCardCode MinionCards.blackDwarf)

  it "Blood to Spare: an alter-ego's minion schemes against them" do
    tg <- ebonyMaw ["spider-man"]
    let p = player1 tg
    inGame tg do
      _ <- addMinion p MinionCards.outrider
      before <- mainSchemeThreatNow
      _ <- revealNow p TreacheryCards.bloodToSpare
      settle
      after <- mainSchemeThreatNow
      liftIO $ after - before `shouldBe` 1

armiesOfTitanSpec :: Spec
armiesOfTitanSpec = describe "Armies of Titan (modular set)" do
  it "Black Order Infantry: guard; when defeated the villain gets a tough status card" do
    tg <- ebonyMaw ["spider-man"]
    let p = player1 tg
    inGame tg do
      m <- addMinion p MinionCards.blackOrderInfantry
      before <- Villain.villainIsTough <$> getVillain
      resolve [MinionMessage m (MinionDamaged (toSource p) (toDamage 4 FromAbility))]
      after <- Villain.villainIsTough <$> getVillain
      liftIO do
        (before, after) `shouldBe` (False, True)
        Guard `member` cdKeywords MinionCards.blackOrderInfantry `shouldBe` True

  it "Black Order Infantry's boost gives the villain a tough status card" do
    tg <- ebonyMaw ["spider-man"]
    inGame tg do
      clearSpells
      _ <- stackEncounterDeck [MinionCards.blackOrderInfantry]
      vid <- villainIdNow
      resolve [VillainMessage vid VillainSchemes]
      tough <- Villain.villainIsTough <$> getVillain
      liftIO $ tough `shouldBe` True

  it "Outrider: when revealed, and as a boost, discard 1 card at random from your hand" do
    tg <- ebonyMaw ["spider-man"]
    let p = player1 tg
    inGame tg do
      clearSpells
      h0 <- length <$> handOf p
      _ <- addMinion p MinionCards.outrider
      h1 <- length <$> handOf p
      _ <- stackEncounterDeck [MinionCards.outrider]
      vid <- villainIdNow
      resolve [VillainMessage vid VillainSchemes]
      h2 <- length <$> handOf p
      liftIO $ (h0 - h1, h1 - h2) `shouldBe` (1, 1)

  it "Landing Craft: 2 threat per player, an acceleration icon; defeated, discard to a minion and it engages the player who defeated it" do
    tg <- ebonyMaw ["spider-man", "captain-marvel"]
    let (p1, p2) = (player1 tg, player2 tg)
    inGame tg do
      card <- revealNow p1 SideSchemeCards.landingCraft
      pass
      let s = sideSchemeOf card
      threat <- sideSchemeThreat s
      _ <- stackEncounterDeck [TreacheryCards.agentOfThanos, MinionCards.outrider]
      resolve [SideSchemeMessage s (SideSchemeThwarted (IdentityRef p2) threat)]
      engaged <- getsGame (map (toCardCode . getCardDef) . filter ((== p2) . minionEngagedIdentity . toAttrs) . HashMap.elems . gameMinions)
      agent <- inDiscard TreacheryCards.agentOfThanos
      liftIO do
        threat `shouldBe` 4
        cdAcceleration SideSchemeCards.landingCraft `shouldBe` 1
        engaged `shouldBe` [toCardCode MinionCards.outrider]
        agent `shouldBe` True
