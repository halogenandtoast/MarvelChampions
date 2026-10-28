-- | Tower Defense (The Mad Titan's Shadow, scenario 2), each card played as a table
-- would: two villains, two main schemes, Focused Defense and Avengers Tower.
module Marvel.Encounter.TowerDefenseSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Attack (Attack (..))
import Marvel.Attachment.Cards qualified as AttachmentCards
import Marvel.Attachment.Types (Attachment, Attrs (..))
import Marvel.Environment.Cards qualified as EnvironmentCards
import Marvel.Environment.Types (Environment, Attrs (..))
import Marvel.Hp (unHp)
import Marvel.Identity (isPlayable)
import Marvel.Keyword (Keyword (Overkill, Piercing, Retaliate))
import Marvel.MainScheme.Cards qualified as MainSchemeCards
import Marvel.MainScheme.Types (getMainSchemeThreat)
import Marvel.Matchers (VillainMatcher (ActiveVillain))
import Marvel.Message.Lifted (villainNamed)
import Marvel.Minion.Cards qualified as MinionCards
import Marvel.Minion.Types (Minion, Attrs (..))
import Marvel.Modifier (Modifier (KeywordModifier))
import Marvel.Phase (Phase (PlayerPhase))
import Marvel.Query (selectOne)
import Marvel.SideScheme.Cards qualified as SideSchemeCards
import Marvel.SideScheme.Types (isCrisis)
import Marvel.Treachery.Cards qualified as TreacheryCards
import Marvel.Villain.Types (villainConfused, villainHp, villainIsTough)
import TestImport

-- * Tables

-- | Against Tower Defense, standard, with no modular set (its cards are the scenario's
-- own), answering setup with first choices: no modular difficulty, and each Black Order
-- Besieger's 1 damage to Avengers Tower.
td :: [Text] -> IO TestGame
td = tdWith Normal (const False)

tdExpert :: [Text] -> IO TestGame
tdExpert = tdWith Expert (const False)

-- | The same, answering setup with the first choice matching where one does.
tdWith :: Difficulty -> (Choice -> Bool) -> [Text] -> IO TestGame
tdWith difficulty p heroes = do
  (app, ran, logged) <- dealtGameLogged (ScenarioSetup "21098" [] difficulty) (map starter heroes) 1
  seats <- runGameApp app (getsGame gameSeats)
  let tg = TestGame app ran logged seats
  inGame tg (go (200 :: Int))
  pure tg
 where
  go 0 = error "setup did not reach a turn"
  go n =
    openQuestion >>= \case
      Just (_, q) | EndTurn `elem` choicesOf q -> pure ()
      Just (ident, q) -> answerQuestion ident (fromMaybe 0 $ headMay [i | (i, c) <- zip [0 ..] (choicesOf q), p c]) >> go (n - 1)
      Nothing -> error "no question during setup"

-- * The board

proxima, corvus :: GameAppT VillainId
proxima = maybe (error "no Proxima Midnight") pure =<< villainNamed "Proxima Midnight"
corvus = maybe (error "no Corvus Glaive") pure =<< villainNamed "Corvus Glaive"

villainCode :: VillainId -> GameAppT CardCode
villainCode v = getsGame (maybe (error "no such villain") (toCardCode . getCardDef) . HashMap.lookup v . gameVillains)

hpOf :: VillainId -> GameAppT Int
hpOf v = getsGame (maybe (error "no such villain") (unHp . villainHp . toAttrs) . HashMap.lookup v . gameVillains)

toughNow :: VillainId -> GameAppT Bool
toughNow v = getsGame (maybe False villainIsTough . HashMap.lookup v . gameVillains)

confusedNow :: VillainId -> GameAppT Bool
confusedNow v = getsGame (maybe False (villainConfused . toAttrs) . HashMap.lookup v . gameVillains)

schemeOf :: CardDef -> GameAppT MainSchemeId
schemeOf def =
  getsGame (\g -> [k | (k, m) <- HashMap.toList (gameMainSchemes g), toCardCode (getCardDef m) == toCardCode def]) >>= \case
    [m] -> pure m
    ms -> error $ "expected one " <> show (cdName def) <> ", found " <> show (length ms)

underSiege, armies :: GameAppT MainSchemeId
underSiege = schemeOf MainSchemeCards.underSiege
armies = schemeOf MainSchemeCards.theArmiesOfThanos

threatOn :: MainSchemeId -> GameAppT Natural
threatOn m = getsGame (maybe 0 getMainSchemeThreat . HashMap.lookup m . gameMainSchemes)

-- | Threat on Under Siege and on The Armies of Thanos.
threats :: GameAppT (Natural, Natural)
threats = (,) <$> (threatOn =<< underSiege) <*> (threatOn =<< armies)

tower :: GameAppT (Attrs Environment)
tower = getsGame (fromMaybe (error "no Avengers Tower") . headMay . map toAttrs . HashMap.elems . gameEnvironments)

towerDamage :: GameAppT Natural
towerDamage = environmentDamage <$> tower

damageTower :: Natural -> GameAppT ()
damageTower n = tower >>= \t -> resolve [EnvironmentMessage (environmentId t) (EnvironmentDamaged n)]

attachmentsOf :: CardDef -> GameAppT [Attrs Attachment]
attachmentsOf def = getsGame (filter ((== toCardCode def) . toCardCode . attachmentCardDef) . map toAttrs . HashMap.elems . gameAttachments)

attachmentOf :: CardDef -> GameAppT (Attrs Attachment)
attachmentOf def = attachmentsOf def >>= \case
  [a] -> pure a
  as -> error $ "expected one " <> show (cdName def) <> ", found " <> show (length as)

-- | The main scheme Focused Defense is on.
focusedOn :: GameAppT (Maybe MainSchemeId)
focusedOn = attachmentMainScheme <$> attachmentOf AttachmentCards.focusedDefense

besiegers :: GameAppT [Attrs Minion]
besiegers = getsGame (filter ((== toCardCode MinionCards.blackOrderBesieger) . toCardCode . minionCardDef) . map toAttrs . HashMap.elems . gameMinions)

finished :: GameAppT (Maybe FinishedStatus)
finished = getsGame \g -> case gameState g of
  Finished status -> Just status
  _ -> Nothing

-- | Enough damage to take the villain to 0 hit points.
kill :: VillainId -> GameAppT ()
kill v = resolve [VillainMessage v (VillainDamaged GameRef (toDamage 99 FromAbility))]

asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass

-- | Answer until the turn comes back: no defending and no responses, anything else (a
-- forced ability, a When Revealed) with its first choice.
settle :: GameAppT ()
settle = go (100 :: Int)
 where
  go 0 = pure ()
  go n =
    openQuestion >>= \case
      Just (_, QuestionLabel "$label.yourTurn" _ _) -> pure ()
      Just (ident, q) -> do
        answerQuestion ident (fromMaybe 0 $ headMay [i | (i, c) <- zip [0 ..] (choicesOf q), passing c])
        go (n - 1)
      Nothing -> pure ()
  passing = \case
    Label l _ -> l `elem` ["$label.noResponse", "$label.noDefender", "$label.declineToDefend"]
    _ -> False

-- | Answer as 'settle' does until a choice matching is offered.
answerUntil :: (HasCallStack) => (Choice -> Bool) -> GameAppT ()
answerUntil p = go (50 :: Int)
 where
  go 0 = error "the choice was never offered"
  go n = do
    here <- offers p
    unless here do
      openQuestion >>= \case
        Just (ident, q) -> do
          let passing = \case
                Label l _ -> l `elem` ["$label.noResponse", "$label.noDefender", "$label.declineToDefend"]
                _ -> False
          answerQuestion ident (fromMaybe 0 $ headMay [i | (i, c) <- zip [0 ..] (choicesOf q), passing c])
          go (n - 1)
        Nothing -> error "nothing was asked"

towerLabel :: Natural -> Choice -> Bool
towerLabel = isLabel . countLabel "damageAvengersTower"

spec :: Spec
spec = do
  scenarioSpec
  engineSpec
  villainSpec
  mainSchemeSpec
  towerSpec
  encounterSpec

-- * Setup

scenarioSpec :: Spec
scenarioSpec = describe "Tower Defense (setup)" do
  it "two villains, both main schemes, Avengers Tower (Stronghold), Focused Defense on The Armies of Thanos, a Besieger engaged with each player" do
    tg <- td ["spider-man", "captain-marvel"]
    inGame tg do
      p <- proxima
      c <- corvus
      codes <- traverse villainCode [p, c]
      ts <- threats
      t <- tower
      fd <- focusedOn
      a <- armies
      active <- selectOne ActiveVillain
      engaged <- map minionEngagedIdentity <$> besiegers
      everyone <- players
      liftIO do
        codes `shouldBe` ["21092", "21095"]
        -- 1 per player each
        ts `shouldBe` (2, 2)
        toCardCode t.environmentCardDef `shouldBe` "21100a"
        -- each Besieger's player chose 1 damage to the tower
        t.environmentDamage `shouldBe` 2
        fd `shouldBe` Just a
        active `shouldBe` Just c
        engaged `shouldMatchList` everyone

  it "expert: Proxima Midnight (II) and Corvus Glaive (II)" do
    tg <- tdExpert ["spider-man"]
    inGame tg do
      p <- proxima
      c <- corvus
      codes <- traverse villainCode [p, c]
      hp <- hpOf p
      liftIO $ (codes, hp) `shouldBe` (["21093", "21096"], 12)

  it "modular difficulty: the first player may place damage per player on Avengers Tower" do
    tg <- tdWith Normal (isLabel (countLabel "towerDamagePerPlayer" 2)) ["spider-man", "captain-marvel"]
    -- 2 per player, and each Besieger's 1
    inGame tg $ towerDamage `shouldReturn` 6

  it "a Black Order Besieger put into play at setup has engaged its player: they may take 2 damage instead" do
    tg <- tdWith Normal (isLabel (countLabel "takeDamage" 2)) ["spider-man"]
    inGame tg do
      towerDamage `shouldReturn` 0
      damageOf (player1 tg) `shouldReturn` 2

-- * Two villains, two main schemes

engineSpec :: Spec
engineSpec = describe "Tower Defense (two villains, two main schemes)" do
  it "Focused Defense moves to the other main scheme after the player phase, its villain becoming the active one" do
    tg <- td ["spider-man"]
    inGame tg do
      runNow [EndPhase PlayerPhase]
      fd <- focusedOn
      s <- underSiege
      p <- proxima
      active <- selectOne ActiveVillain
      liftIO $ (fd, active) `shouldBe` (Just s, Just p)

  it "only the active villain activates; a villain schemes onto his own scheme, a minion onto Focused Defense's" do
    tg <- td ["spider-man"]
    let you = player1 tg
    inGame tg do
      s <- underSiege
      fd <- attachmentOf AttachmentCards.focusedDefense
      resolve [AttachmentMessage fd.attachmentId (AttachedToMainScheme s)]
      _ <- stackEncounterDeck [TreacheryCards.advance]
      before <- threats
      resolve [IdentityMessage you VillainAndMinionsActivate]
      after <- threats
      -- Proxima's 1 SCH and the Besieger's 1, on Under Siege only
      liftIO $ (fst after - fst before, snd after - snd before) `shouldBe` (2, 0)

  it "Proxima Midnight schemes onto Under Siege, Corvus Glaive onto The Armies of Thanos" do
    tg <- td ["spider-man"]
    inGame tg do
      p <- proxima
      c <- corvus
      _ <- stackEncounterDeck [TreacheryCards.advance]
      t0 <- threats
      resolve [VillainMessage p VillainSchemes]
      t1 <- threats
      _ <- stackEncounterDeck [TreacheryCards.advance]
      resolve [VillainMessage c VillainSchemes]
      t2 <- threats
      liftIO do
        (fst t1 - fst t0, snd t1 - snd t0) `shouldBe` (1, 0)
        (fst t2 - fst t1, snd t2 - snd t1) `shouldBe` (0, 2)

  it "a minion schemes onto the main scheme Focused Defense is on" do
    tg <- td ["spider-man"]
    inGame tg do
      m <- maybe (error "no Besieger") minionId . headMay <$> besiegers
      before <- threats
      resolve [MinionMessage m MinionSchemes]
      after <- threats
      liftIO $ (fst after - fst before, snd after - snd before) `shouldBe` (0, 1)

  it "an encounter card's \"the main scheme\" is both: Sniper Shot to an alter-ego" do
    tg <- td ["spider-man"]
    let you = player1 tg
    inGame tg do
      before <- threats
      _ <- revealNow you TreacheryCards.sniperShot
      settle
      after <- threats
      liftIO $ (fst after - fst before, snd after - snd before) `shouldBe` (3, 3)

  it "each main scheme gains its acceleration and each acceleration token" do
    tg <- td ["spider-man"]
    inGame tg do
      s <- underSiege
      resolve [PlaceAccelerationToken s]
      before <- threats
      resolve [AccelerateMainScheme]
      after <- threats
      liftIO $ (fst after - fst before, snd after - snd before) `shouldBe` (2, 2)

  it "a player's \"the villain\" is the one they choose: Think Fast! confuses the one chosen" do
    tg <- td ["drax"]
    let you = player1 tg
    inGame tg do
      asHero you
      p <- proxima
      c <- corvus
      thinkFast <- putInHand you "19031"
      resource <- putInHand you "01089"
      playWith thinkFast [resource]
      answerUntil (isTargetChoice (VillainRef p))
      both <- (&&) <$> offers (isTargetChoice (VillainRef p)) <*> offers (isTargetChoice (VillainRef c))
      choose "Proxima Midnight" (isTargetChoice (VillainRef p))
      settle
      confused <- traverse confusedNow [p, c]
      liftIO $ (both, confused) `shouldBe` (True, [True, False])

  it "a player's \"the main scheme\" is the one they choose: By Any Means" do
    tg <- td ["spider-man"]
    let you = player1 tg
    inGame tg do
      asHero you
      s <- underSiege
      a <- armies
      card <- putInHand you "16151"
      before <- threats
      playWith card []
      answerUntil (isTargetChoice (MainSchemeRef s))
      both <- offers (isTargetChoice (MainSchemeRef a))
      choose "Under Siege" (isTargetChoice (MainSchemeRef s))
      settle
      after <- threats
      liftIO $ (both, fst after - fst before, snd after - snd before) `shouldBe` (True, 2, 0)

-- * The villains

villainSpec :: Spec
villainSpec = describe "Tower Defense (villains)" do
  it "neither is defeated while the other has hit points; both at 0, each advances; both final stages defeated, the players win" do
    tg <- td ["spider-man"]
    inGame tg do
      p <- proxima
      c <- corvus
      kill p
      stillHere <- (,) <$> villainCode p <*> hpOf p
      kill c
      advanced <- traverse villainCode [p, c]
      hps <- traverse hpOf [p, c]
      kill p
      notYet <- finished
      kill c
      done <- finished
      liftIO do
        stillHere `shouldBe` ("21092", 0)
        advanced `shouldBe` ["21093", "21096"]
        hps `shouldBe` [12, 11]
        notYet `shouldBe` Nothing
        done `shouldBe` Just Won

  it "a villain at 0 hit points healed has hit points again, and holds the other up" do
    tg <- td ["spider-man"]
    inGame tg do
      p <- proxima
      c <- corvus
      kill p
      resolve [VillainMessage p (VillainHealed 2)]
      kill c
      codes <- traverse villainCode [p, c]
      liftIO $ codes `shouldBe` ["21092", "21095"]

  it "expert: (II) defeated advances to (III)" do
    tg <- tdExpert ["spider-man"]
    inGame tg do
      p <- proxima
      c <- corvus
      kill p >> kill c
      codes <- traverse villainCode [p, c]
      kill p >> kill c
      done <- finished
      liftIO $ (codes, done) `shouldBe` (["21094", "21097"], Just Won)

  it "Proxima Midnight attacking you: deal 1 damage to Avengers Tower, or she gets +2 ATK" do
    tg <- td ["spider-man"]
    let you = player1 tg
    inGame tg do
      asHero you
      p <- proxima
      t0 <- towerDamage
      _ <- stackEncounterDeck [TreacheryCards.advance]
      start [VillainMessage p (VillainAttacks you)]
      answerUntil (towerLabel 1)
      both <- offers (isLabel (countLabel "proximaGetsAttack" 2))
      choose "the tower" (towerLabel 1)
      settle
      t1 <- towerDamage
      d1 <- damageOf you
      _ <- stackEncounterDeck [TreacheryCards.advance]
      start [VillainMessage p (VillainAttacks you)]
      answerUntil (isLabel (countLabel "proximaGetsAttack" 2))
      choose "+2 ATK" (isLabel (countLabel "proximaGetsAttack" 2))
      settle
      t2 <- towerDamage
      d2 <- damageOf you
      liftIO $ (both, t1 - t0, d1, t2 - t1, d2 - d1) `shouldBe` (True, 1, 2, 0, 4)

  it "Corvus Glaive's undefended attack discards the top encounter card: 1 damage to the tower per boost icon" do
    tg <- td ["spider-man"]
    let you = player1 tg
    inGame tg do
      asHero you
      c <- corvus
      t0 <- towerDamage
      -- the attack's boost card, then the card discarded (2 boost icons)
      _ <- stackEncounterDeck [TreacheryCards.advance, SideSchemeCards.cityUnderAttack]
      start [VillainMessage c (VillainAttacks you)]
      settle
      t1 <- towerDamage
      liftIO $ t1 - t0 `shouldBe` 2

  it "Corvus Glaive's defended attack discards nothing" do
    tg <- td ["spider-man"]
    let you = player1 tg
    inGame tg do
      asHero you
      c <- corvus
      t0 <- towerDamage
      _ <- stackEncounterDeck [TreacheryCards.advance, SideSchemeCards.cityUnderAttack]
      start [VillainMessage c (VillainAttacks you)]
      answerUntil isDefend
      choose "defend" isDefend
      settle
      t1 <- towerDamage
      liftIO $ t1 - t0 `shouldBe` 0

  it "Corvus Glaive's attack whose defending ally is defeated by a boost before the damage is undefended" do
    tg <- td ["spider-man"]
    let you = player1 tg
    inGame tg do
      asHero you
      c <- corvus
      _ <- putIntoPlay you "01002"
      cat <- allyIn "01002"
      -- 1 hit point left: Starshark's boost, 1 damage to each character you control,
      -- defeats her before the attack's damage
      resolve [AllyMessage (allyId cat) (AllyDamaged GameRef (toDamage 1 FromAbility))]
      t0 <- towerDamage
      d0 <- damageOf you
      _ <- stackEncounterDeck [MinionCards.starshark, SideSchemeCards.cityUnderAttack]
      start [VillainMessage c (VillainAttacks you)]
      let catDefends = \case
            AllyDefend a _ -> a == allyId cat
            _ -> False
      answerUntil catDefends
      choose "Black Cat defends" catDefends
      settle
      t1 <- towerDamage
      d1 <- damageOf you
      cats <- alliesWith "01002"
      liftIO do
        length cats `shouldBe` 0
        -- undefended: Corvus Glaive's response, and the attack's damage on the hero
        t1 - t0 `shouldBe` 2
        d1 - d0 `shouldSatisfy` (> 1)

-- * The main schemes

mainSchemeSpec :: Spec
mainSchemeSpec = describe "Tower Defense (main schemes)" do
  it "Under Siege completed: its threat is removed instead, and 6 per player damage dealt to Avengers Tower" do
    tg <- td ["spider-man"]
    inGame tg do
      s <- underSiege
      t0 <- towerDamage
      resolve [MainSchemeMessage s (MainSchemePlaceThreat 5)]
      threat <- threatOn s
      t1 <- towerDamage
      done <- finished
      liftIO $ (threat, t1 - t0, done) `shouldBe` (0, 6, Nothing)

  it "The Armies of Thanos completed: its threat is removed instead, and each player dealt an encounter card" do
    tg <- td ["spider-man", "captain-marvel"]
    inGame tg do
      a <- armies
      dealt0 <- traverse (fmap length . dealtTo) =<< players
      resolve [MainSchemeMessage a (MainSchemePlaceThreat 10)]
      threat <- threatOn a
      dealt1 <- traverse (fmap length . dealtTo) =<< players
      liftIO $ (threat, zipWith (-) dealt1 dealt0) `shouldBe` (0, [1, 1])

-- * Avengers Tower

towerSpec :: Spec
towerSpec = describe "Tower Defense (Avengers Tower)" do
  it "9 per player damage on Stronghold: flipped to Damaged with none, each Avengers Tower support discarded; 9 per player on Damaged, the players lose" do
    tg <- td ["spider-man", "captain-marvel"]
    let (you, other) = (player1 tg, player2 tg)
    inGame tg do
      _ <- putIntoPlay you "03024"
      _ <- putIntoPlay other "03024"
      supports0 <- length <$> supportsWith "03024"
      -- 2 from the Besiegers already
      damageTower 15
      t1 <- tower
      damageTower 1
      t2 <- tower
      supports1 <- length <$> supportsWith "03024"
      damageTower 17
      notYet <- finished
      damageTower 1
      done <- finished
      liftIO do
        supports0 `shouldBe` 2
        (toCardCode t1.environmentCardDef, t1.environmentDamage) `shouldBe` ("21100a", 17)
        (toCardCode t2.environmentCardDef, t2.environmentDamage) `shouldBe` ("21100b", 0)
        supports1 `shouldBe` 0
        notYet `shouldBe` Nothing
        done `shouldBe` Just Lost

  it "Stronghold: the unique rule does not apply to Avengers Tower; Damaged, it does again" do
    tg <- td ["spider-man", "captain-marvel"]
    let (you, other) = (player1 tg, player2 tg)
    inGame tg do
      _ <- putIntoPlay other "03024"
      card <- putInHand you "03024"
      _ <- putInHand you "01088"
      exempt <- getPlayer you >>= (`isPlayable` card)
      -- flipped: the other player's copy is discarded, the environment's title unique again
      damageTower 16
      gone <- supportsWith "03024"
      unique <- getPlayer you >>= (`isPlayable` card)
      liftIO $ length gone `shouldBe` 0
      liftIO $ (exempt, unique) `shouldBe` (True, False)

-- * The encounter cards

encounterSpec :: Spec
encounterSpec = describe "Tower Defense (encounter set)" do
  it "Black Order Besieger engaging you: 1 damage to Avengers Tower, or 2 to your identity" do
    tg <- td ["spider-man"]
    let you = player1 tg
    inGame tg do
      t0 <- towerDamage
      _ <- revealNow you MinionCards.blackOrderBesieger
      answerUntil (isLabel (countLabel "takeDamage" 2))
      both <- offers (towerLabel 1)
      d0 <- damageOf you
      choose "2 damage" (isLabel (countLabel "takeDamage" 2))
      settle
      d1 <- damageOf you
      t1 <- towerDamage
      liftIO $ (both, d1 - d0, t1 - t0) `shouldBe` (True, 2, 0)

  it "Proxima's Spear: on Proxima Midnight, +1 ATK, her attacks gain overkill and piercing; a hero may take 1 damage and spend [energy][mental] to discard it" do
    tg <- td ["spider-man"]
    let you = player1 tg
    inGame tg do
      p <- proxima
      _ <- revealNow you AttachmentCards.proximasSpear
      settle
      spear <- attachmentOf AttachmentCards.proximasSpear
      atk <- getVillainAttackValue p
      mods <- getModifiers (VillainRef p)
      liftIO do
        attachmentEnemy spear `shouldBe` Just (EnemyVillainId p)
        atk `shouldBe` 3
        mods `shouldSatisfy` elem (KeywordModifier Overkill)
        mods `shouldSatisfy` elem (KeywordModifier Piercing)
      asHero you
      clearHand you
      energy <- putInHand you "01088"
      genius <- putInHand you "01089"
      d0 <- damageOf you
      choose "discard the Spear" (isAbilityOf (AttachmentRef spear.attachmentId))
      payWith [energy, genius]
      settle
      d1 <- damageOf you
      left <- attachmentsOf AttachmentCards.proximasSpear
      liftIO $ (d1 - d0, length left) `shouldBe` (1, 0)

  it "Corvus's Glaive: on Corvus Glaive, who gains retaliate 1" do
    tg <- td ["spider-man"]
    let you = player1 tg
    inGame tg do
      c <- corvus
      _ <- revealNow you AttachmentCards.corvussGlaive
      settle
      glaive <- attachmentOf AttachmentCards.corvussGlaive
      mods <- getModifiers (VillainRef c)
      liftIO do
        attachmentEnemy glaive `shouldBe` Just (EnemyVillainId c)
        mods `shouldSatisfy` elem (KeywordModifier (Retaliate 1))

  it "Direct Assault: on the villain who is not active, +2 ATK; his attack gains ranged, an ally it defeats deals 3 to the tower, and it is discarded after" do
    tg <- td ["spider-man"]
    let you = player1 tg
    inGame tg do
      p <- proxima
      _ <- revealNow you AttachmentCards.directAssault
      settle
      assault <- attachmentOf AttachmentCards.directAssault
      atk <- getVillainAttackValue p
      asHero you
      _ <- putIntoPlay you "01002"
      cat <- allyIn "01002"
      t0 <- towerDamage
      _ <- stackEncounterDeck [TreacheryCards.advance]
      start [VillainMessage p (VillainAttacks you)]
      let catDefends = \case
            AllyDefend a _ -> a == allyId cat
            _ -> False
      answerUntil catDefends
      ranged <- maybe False attackRanged <$> getVillainAttack p
      choose "Black Cat defends" catDefends
      settle
      t1 <- towerDamage
      left <- attachmentsOf AttachmentCards.directAssault
      cats <- alliesWith "01002"
      liftIO do
        attachmentEnemy assault `shouldBe` Just (EnemyVillainId p)
        atk `shouldBe` 4
        ranged `shouldBe` True
        -- Proxima's interrupt answered with 1 to the tower, then Direct Assault's 3
        t1 - t0 `shouldBe` 4
        length cats `shouldBe` 0
        length left `shouldBe` 0

  it "Proxima's Power: to an alter-ego she schemes (onto Under Siege); to a hero she attacks" do
    tg <- td ["spider-man"]
    let you = player1 tg
    inGame tg do
      _ <- stackEncounterDeck [TreacheryCards.advance]
      before <- threats
      _ <- revealNow you TreacheryCards.proximasPower
      settle
      after <- threats
      asHero you
      d0 <- damageOf you
      _ <- stackEncounterDeck [TreacheryCards.advance]
      _ <- revealNow you TreacheryCards.proximasPower
      settle
      d1 <- damageOf you
      liftIO $ (fst after - fst before, snd after - snd before, d1 - d0) `shouldBe` (1, 0, 2)

  it "Corvus's Cunning: to an alter-ego Corvus Glaive schemes (onto The Armies of Thanos)" do
    tg <- td ["spider-man"]
    let you = player1 tg
    inGame tg do
      _ <- stackEncounterDeck [TreacheryCards.advance]
      before <- threats
      _ <- revealNow you TreacheryCards.corvussCunning
      settle
      after <- threats
      liftIO $ (fst after - fst before, snd after - snd before) `shouldBe` (0, 2)

  it "Proxima's Power as a boost: the other villain's ATK added to an attack, his SCH to a scheme" do
    tg <- td ["spider-man"]
    let you = player1 tg
    inGame tg do
      p <- proxima
      c <- corvus
      _ <- stackEncounterDeck [TreacheryCards.corvussCunning]
      before <- threats
      resolve [VillainMessage p VillainSchemes]
      after <- threats
      asHero you
      -- the boost card, then the card Corvus's undefended attack discards
      _ <- stackEncounterDeck [TreacheryCards.proximasPower, TreacheryCards.advance]
      d0 <- damageOf you
      start [VillainMessage c (VillainAttacks you)]
      settle
      d1 <- damageOf you
      -- Proxima's 1 SCH and Corvus's 2; Corvus's 1 ATK and Proxima's 2
      liftIO $ (fst after - fst before, d1 - d0) `shouldBe` (3, 3)

  it "Bound by Blood: heals 2 from each villain and makes each tough; as a boost, the active villain's" do
    tg <- td ["spider-man"]
    let you = player1 tg
    inGame tg do
      p <- proxima
      c <- corvus
      resolve [VillainMessage v (VillainDamaged GameRef (toDamage 3 FromAbility)) | v <- [p, c]]
      _ <- revealNow you TreacheryCards.boundByBlood
      settle
      hps <- traverse hpOf [p, c]
      toughs <- traverse toughNow [p, c]
      liftIO $ (hps, toughs) `shouldBe` ([8, 7], [True, True])
    tg' <- td ["spider-man"]
    let you' = player1 tg'
    inGame tg' do
      p <- proxima
      c <- corvus
      resolve [VillainMessage v (VillainDamaged GameRef (toDamage 3 FromAbility)) | v <- [p, c]]
      asHero you'
      _ <- stackEncounterDeck [TreacheryCards.boundByBlood, TreacheryCards.advance]
      start [VillainMessage c (VillainAttacks you')]
      settle
      hps <- traverse hpOf [p, c]
      toughs <- traverse toughNow [p, c]
      -- Corvus Glaive is the active villain
      liftIO $ (hps, toughs) `shouldBe` ([6, 7], [False, True])

  it "Rain Fire: 3 damage to Avengers Tower; as a boost, 3 more if the attack defeats an ally" do
    tg <- td ["spider-man"]
    let you = player1 tg
    inGame tg do
      c <- corvus
      t0 <- towerDamage
      _ <- revealNow you TreacheryCards.rainFire
      settle
      t1 <- towerDamage
      asHero you
      _ <- putIntoPlay you "01002"
      cat <- allyIn "01002"
      _ <- stackEncounterDeck [TreacheryCards.rainFire]
      start [VillainMessage c (VillainAttacks you)]
      let catDefends = \case
            AllyDefend a _ -> a == allyId cat
            _ -> False
      answerUntil catDefends
      choose "Black Cat defends" catDefends
      settle
      t2 <- towerDamage
      cats <- alliesWith "01002"
      -- an attack that defeats no ally: the hero's, undefended (Corvus's own discard a
      -- card with no boost icons)
      _ <- stackEncounterDeck [TreacheryCards.rainFire, TreacheryCards.advance]
      start [VillainMessage c (VillainAttacks you)]
      settle
      t3 <- towerDamage
      liftIO $ (t1 - t0, length cats, t2 - t1, t3 - t2) `shouldBe` (3, 0, 3, 0)

  it "City Under Attack: 2 threat and Hinder 1 per player, a crisis icon; the player who defeats it draws 1" do
    tg <- td ["spider-man"]
    let you = player1 tg
    inGame tg do
      card <- encounterCard SideSchemeCards.cityUnderAttack
      runNow [RevealEncounterCard you card]
      settle
      threat <- sideSchemeThreat (sideSchemeOf card)
      crisis <- getsGame (maybe False isCrisis . HashMap.lookup (sideSchemeOf card) . gameSideSchemes)
      hand0 <- length <$> handOf you
      runNow [SideSchemeMessage (sideSchemeOf card) (SideSchemeThwarted (IdentityRef you) 3)]
      settle
      hand1 <- length <$> handOf you
      liftIO $ (threat, crisis, hand1 - hand0) `shouldBe` (3, True, 1)
