-- | Loki (The Mad Titan's Shadow, scenario 5) and the Loki set, each card played as a
-- table would.
module Marvel.Encounter.LokiSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Data.Aeson (toJSON)
import Marvel.Area (DeckKey (..), Pile (..))
import Marvel.Attachment.Cards qualified as AttachmentCards
import Marvel.Attachment.Types (Attachment, Attrs (..))
import Marvel.Environment.Cards qualified as EnvironmentCards
import Marvel.Environment.Types (Environment)
import Marvel.Hp (HP (..))
import Marvel.MainScheme.Cards qualified as MainSchemeCards
import Marvel.Minion.Cards qualified as MinionCards
import Marvel.Scenario.Types (Attrs (scenarioVictoryCondition), villainAsCard)
import Marvel.SideScheme.Cards qualified as SideSchemeCards
import Marvel.Trait (Trait (InfinityStone))
import Marvel.Treachery.Cards qualified as TreacheryCards
import Marvel.Villain.Cards qualified as VillainCards
import Marvel.Villain.Types (getModifiedAttack, getModifiedScheme, villainAttachments, villainBoostCards, villainHp, villainId, villainStunned, villainTough)
import Marvel.Window qualified as W
import Data.List (sort)
import TestImport

lokiGame :: IO TestGame
lokiGame = testGameAgainst "21160" ["spider-man"]

lokis :: [CardDef]
lokis = [VillainCards.loki1, VillainCards.loki2, VillainCards.loki3, VillainCards.loki4, VillainCards.loki5]

villainAttrs :: GameAppT (Attrs Villain)
villainAttrs = toAttrs <$> getVillain

villainCode :: GameAppT CardCode
villainCode = toCardCode . getCardDef <$> getVillain

-- | The Lokis set aside (the nemesis sets are set aside too).
asideCodes :: GameAppT [CardCode]
asideCodes = filter (`elem` map toCardCode lokis) . map toCardCode <$> getSetAsideEncounterCards

victoryCodes :: GameAppT [CardCode]
victoryCodes = map toCardCode <$> getVictoryDisplay

finished :: GameAppT (Maybe FinishedStatus)
finished = getsGame \g -> case gameState g of
  Finished status -> Just status
  _ -> Nothing

asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass

-- | The Loki in play made the version given, by a swap whose set-aside versions are
-- kept as they would be.
lokiIs :: CardDef -> GameAppT ()
lokiIs def = do
  vid <- villainIdNow
  old <- getCardDef <$> getVillain
  unless (toCardCode old == toCardCode def) do
    aside <- getSetAsideEncounterCards
    resolve [RemoveFromGame (CardIdRef (ecCardId c)) | c <- aside, toCardCode c == toCardCode def]
    resolve [ReplaceVillain vid (toCardCode def) Swapped]
    resolve [SetAside [EncounterCard (villainAsCard vid old)]]

-- | Only these versions left set aside, so a swap must pick among them.
onlyAside :: [CardDef] -> GameAppT ()
onlyAside keep = do
  aside <- getSetAsideEncounterCards
  resolve [RemoveFromGame (CardIdRef (ecCardId c)) | c <- aside, toCardCode c `elem` map toCardCode lokis, toCardCode c `notElem` map toCardCode keep]

setVictoryCondition :: Natural -> GameAppT ()
setVictoryCondition n = resolve [CardSpecific GameRef "lokiVictoryCondition" (toJSON n)]

attachmentsOf :: CardDef -> GameAppT [Attrs Attachment]
attachmentsOf def = getsGame (filter ((== toCardCode def) . toCardCode . attachmentCardDef) . map toAttrs . HashMap.elems . gameAttachments)

-- | An attachment revealed to the player, as from the encounter deck.
attachNow :: IdentityId -> CardDef -> GameAppT AttachmentId
attachNow you def = do
  card <- revealNow you def
  resolve []
  pure $ AttachmentId (unCardId (ecCardId card))

removeGauntlet :: GameAppT ()
removeGauntlet = attachmentsOf AttachmentCards.infinityGauntlet >>= traverse_ \g -> resolve [RemoveFromGame (AttachmentRef $ attachmentId g)]

sideSchemeIds :: GameAppT [SideSchemeId]
sideSchemeIds = getsGame (HashMap.keys . gameSideSchemes)

sideSchemeCodes :: GameAppT [CardCode]
sideSchemeCodes = getsGame (map (toCardCode . getCardDef) . HashMap.elems . gameSideSchemes)

removeSideSchemes :: GameAppT ()
removeSideSchemes = sideSchemeIds >>= \ss -> resolve [RemoveFromGame (SideSchemeRef s) | s <- ss]

stonePile :: GameAppT Pile
stonePile = getPile InfinityStoneDeckKey

stonesInPlay :: GameAppT [Environment]
stonesInPlay = getsGame (filter ((InfinityStone `member`) . cdTraits . getCardDef) . HashMap.elems . gameEnvironments)

clearStones :: GameAppT ()
clearStones = stonesInPlay >>= \es -> resolve [RemoveFromPlay (toRef e) | e <- es]

stoneOnTop :: CardDef -> GameAppT ()
stoneOnTop def = do
  resolve [UsingDeck InfinityStoneDeckKey ShuffleEncounterDiscardIntoDeck]
  Pile deck _ <- stonePile
  case filter ((== toCardCode def) . toCardCode) deck of
    [c] -> resolve [UsingDeck InfinityStoneDeckKey (PutOnTopOfEncounterDeck [c])]
    _ -> error $ "not in the stone deck: " <> show (cdName def)

-- | Damage of an attack of the hero's on the villain, its target already chosen.
heroAttackFor :: IdentityId -> Natural -> GameAppT ()
heroAttackFor you n = do
  vid <- villainIdNow
  runNow [Ask you (ChooseOne [DamageEnemy (EnemyRef (EnemyVillainId vid)) (toSource you) (toDamage n (FromPlayerAttack you))])]
  choose "the attack" (const True)
  -- a forced interrupt to it (Master of Illusions) asked first
  chooseIf isAnyAbility
  resolve []

-- | Damage to the villain from no player card.
damageFromGame :: Natural -> GameAppT ()
damageFromGame n = do
  vid <- villainIdNow
  you <- headMay <$> players >>= maybe (error "no players") pure
  runNow [Ask you (ChooseOne [DamageEnemy (EnemyRef (EnemyVillainId vid)) GameRef (toDamage n FromAbility)])]
  choose "the damage" (const True)
  resolve []

defeatLoki :: GameAppT ()
defeatLoki = villainIdNow >>= \vid -> resolve [VillainMessage vid VillainDefeated]

-- | Decline each response offered until one matching is offered, or nothing more is.
answerUntil :: (Choice -> Bool) -> GameAppT Bool
answerUntil p = go (20 :: Int)
 where
  go 0 = pure False
  go n =
    offers p >>= \case
      True -> pure True
      False ->
        offers isNoResponse >>= \case
          True -> choose "no response" isNoResponse >> go (n - 1)
          False -> pure False

spec :: Spec
spec = do
  setupSpec
  victorySpec
  swapSpec
  versionsSpec
  attachmentsSpec
  treacheriesSpec

setupSpec :: Spec
setupSpec = describe "Loki (setup)" do
  it "one Loki at random in play, the other four set aside; the Gauntlet on him, War in Asgard in play, a stone revealed" do
    tg <- lokiGame
    inGame tg do
      code <- villainCode
      aside <- asideCodes
      vid <- villainIdNow
      gauntlets <- map attachmentEnemy <$> attachmentsOf AttachmentCards.infinityGauntlet
      schemes <- sideSchemeCodes
      threat <- maybe (pure 0) sideSchemeThreat . headMay =<< sideSchemeIds
      stones <- length <$> stonesInPlay
      main <- getsGame (map (toCardCode . getCardDef) . HashMap.elems . gameMainSchemes)
      mainThreat <- mainSchemeThreatNow
      hp <- villainHp <$> villainAttrs
      liftIO do
        sort (code : aside) `shouldBe` map toCardCode lokis
        gauntlets `shouldBe` [Just (EnemyVillainId vid)]
        schemes `shouldBe` [toCardCode SideSchemeCards.warInAsgard]
        -- 6, and hinder 1 per player
        threat `shouldBe` 7
        stones `shouldBe` 1
        main `shouldBe` [toCardCode MainSchemeCards.allHailKingLoki]
        mainThreat `shouldBe` 1
        hp `shouldBe` HP 20

  it "the first player chooses the victory condition, 1 to 4, as the scenario starts" do
    let setup = ScenarioSetup "21160" [] Normal
    (app, ran) <- dealtGameBeforeSetup id setup [starter "spider-man"] 1 (const (pure ()))
    seats <- runGameApp app (getsGame gameSeats)
    logged <- newIORef []
    let tg = TestGame app ran logged seats
    inGame tg do
      let go (0 :: Int) = error "the victory condition was not asked"
          go n =
            openQuestion >>= \case
              Just (ident, QuestionLabel "$label.chooseLokiVictoryCondition" _ q) -> do
                liftIO $ length (choicesOf q) `shouldBe` 4
                answerQuestion ident 2
              Just (ident, _) -> answerQuestion ident 0 >> go (n - 1)
              Nothing -> error "nothing asked"
      go 20
      toFirstTurn
      condition <- getsGame (scenarioVictoryCondition . toAttrs . gameScenario)
      inPlay <- length <$> getsGame (HashMap.keys . gameVillains)
      liftIO $ (condition, inPlay) `shouldBe` (Just 3, 1)

  it "expert: one Loki, the expert set in the encounter deck" do
    let setup = ScenarioSetup "21160" [] Expert
    (app, ran, logged) <- dealtGameLogged setup [starter "spider-man"] 1
    seats <- runGameApp app (getsGame gameSeats)
    let tg = TestGame app ran logged seats
    inGame tg do
      toFirstTurn
      code <- villainCode
      aside <- asideCodes
      liftIO $ sort (code : aside) `shouldBe` map toCardCode lokis

victorySpec :: Spec
victorySpec = describe "Loki (winning and losing)" do
  it "victory condition 1: Loki defeated goes to the victory display, and the players win" do
    tg <- lokiGame
    inGame tg do
      code <- villainCode
      defeatLoki
      shown <- victoryCodes
      liftIO $ shown `shouldBe` [code]
      finished `shouldReturn` Just Won

  it "victory condition 2: the first Loki defeated, a set-aside Loki takes his place with what was on him, at full hit points, and a side scheme is revealed" do
    tg <- lokiGame
    let you = player1 tg
    inGame tg do
      setVictoryCondition 2
      lokiIs VillainCards.loki2
      damageFromGame 5
      staff <- attachNow you AttachmentCards.lokisStaff
      vid <- villainIdNow
      resolve [VillainMessage vid (VillainStunned GameRef)]
      removeSideSchemes
      _ <- stackEncounterDeck [MinionCards.hydraMercenary, SideSchemeCards.openTheBifrost]
      defeatLoki
      code <- villainCode
      shown <- victoryCodes
      aside <- asideCodes
      a <- villainAttrs
      schemes <- sideSchemeCodes
      done <- finished
      liftIO do
        done `shouldBe` Nothing
        shown `shouldBe` [toCardCode VillainCards.loki2]
        code `shouldSatisfy` (/= toCardCode VillainCards.loki2)
        length aside `shouldBe` 3
        villainId a `shouldBe` vid
        villainHp a `shouldBe` HP 20
        villainAttachments a `shouldSatisfy` member staff
        schemes `shouldBe` [toCardCode SideSchemeCards.openTheBifrost]
      -- stunned, unless the new version is stalwart
      stalwart <- (`elem` map toCardCode [VillainCards.loki3, VillainCards.loki4]) <$> villainCode
      stunned <- villainStunned <$> villainAttrs
      liftIO $ stunned `shouldBe` not stalwart
      defeatLoki
      finished `shouldReturn` Just Won
      length <$> victoryCodes `shouldReturn` 2

  it "All Hail King Loki completed: the players lose" do
    tg <- lokiGame
    inGame tg do
      mid <- mainSchemeIdNow
      resolve [AdvanceMainScheme mid]
      finished `shouldReturn` Just Lost

swapSpec :: Spec
swapSpec = describe "Swapping Loki" do
  it "The Trickster: Loki is swapped with a set-aside Loki, keeping his damage, attachments and status cards; the old one set aside; then he activates against you" do
    tg <- lokiGame
    let you = player1 tg
    inGame tg do
      lokiIs VillainCards.loki2
      onlyAside [VillainCards.loki1, VillainCards.loki5]
      removeGauntlet
      damageFromGame 4
      crown <- attachNow you AttachmentCards.lokisCrown
      vid <- villainIdNow
      resolve [VillainMessage vid (VillainConfused GameRef)]
      before <- mainSchemeThreatNow
      _ <- stackEncounterDeck [MinionCards.hydraMercenary]
      _ <- revealNow you TreacheryCards.theTrickster
      resolve []
      code <- villainCode
      aside <- asideCodes
      a <- villainAttrs
      dmg <- villainDamageNow
      after <- mainSchemeThreatNow
      liftIO do
        code `shouldSatisfy` (`elem` map toCardCode [VillainCards.loki1, VillainCards.loki5])
        sort aside `shouldBe` sort (toCardCode VillainCards.loki2 : filter (/= code) (map toCardCode [VillainCards.loki1, VillainCards.loki5]))
        dmg `shouldBe` 4
        villainAttachments a `shouldSatisfy` member crown
        -- his scheme against the alter-ego was the confusion's to cancel
        after `shouldBe` before

  it "swapped in alter-ego, Loki schemes against you; in hero form he attacks" do
    tg <- lokiGame
    let you = player1 tg
    inGame tg do
      lokiIs VillainCards.loki2
      onlyAside [VillainCards.loki4]
      removeGauntlet
      before <- mainSchemeThreatNow
      _ <- stackEncounterDeck [MinionCards.hydraMercenary]
      _ <- revealNow you TreacheryCards.theTrickster
      resolve []
      after <- mainSchemeThreatNow
      -- Loki 21163: SCH 3, and the boost card's icons
      liftIO $ after - before `shouldSatisfy` (>= 3)
      -- the version swapped out (21161) is the only one set aside now
      asHero you
      clearHand you
      _ <- stackEncounterDeck [MinionCards.hydraMercenary]
      _ <- revealNow you TreacheryCards.theTrickster
      pass
      resolve []
      code <- villainCode
      dmg <- damageOf you
      liftIO $ (code, dmg > 0) `shouldBe` (toCardCode VillainCards.loki2, True)

  it "swapped for a stalwart Loki, his stunned and confused status cards are discarded" do
    tg <- lokiGame
    inGame tg do
      lokiIs VillainCards.loki1
      onlyAside [VillainCards.loki3]
      vid <- villainIdNow
      resolve [VillainMessage vid (VillainStunned GameRef), VillainMessage vid (VillainConfused GameRef)]
      resolve [SwapVillain vid]
      code <- villainCode
      a <- villainAttrs
      liftIO $ (code, villainStunned a) `shouldBe` (toCardCode VillainCards.loki3, False)

  it "the Infinity Gauntlet stays attached to Loki through a swap and an advance" do
    tg <- lokiGame
    inGame tg do
      setVictoryCondition 3
      vid <- villainIdNow
      resolve [SwapVillain vid]
      swapped <- map attachmentEnemy <$> attachmentsOf AttachmentCards.infinityGauntlet
      defeatLoki
      vid' <- villainIdNow
      advanced <- map attachmentEnemy <$> attachmentsOf AttachmentCards.infinityGauntlet
      onLoki <- villainAttachments <$> villainAttrs
      liftIO do
        vid' `shouldBe` vid
        swapped `shouldBe` [Just (EnemyVillainId vid)]
        advanced `shouldBe` [Just (EnemyVillainId vid)]
        length onLoki `shouldSatisfy` (>= 1)

  it "with no set-aside Loki, a swap does nothing" do
    tg <- lokiGame
    inGame tg do
      code <- villainCode
      onlyAside []
      vid <- villainIdNow
      resolve [SwapVillain vid]
      villainCode `shouldReturn` code

  it "Loki's Cape: after Loki is swapped, he is given a tough status card" do
    tg <- lokiGame
    let you = player1 tg
    inGame tg do
      _ <- attachNow you AttachmentCards.lokisCape
      tough <- villainTough <$> villainAttrs
      vid <- villainIdNow
      resolve [SwapVillain vid]
      tough' <- villainTough <$> villainAttrs
      liftIO $ (tough, tough') `shouldBe` (False, True)

  it "a Loki side scheme defeated: the top stone is revealed and Loki is swapped" do
    tg <- lokiGame
    let you = player1 tg
    inGame tg do
      code <- villainCode
      clearStones
      removeSideSchemes
      card <- revealNow you SideSchemeCards.casketOfAncientWinters
      resolve []
      let sid = sideSchemeOf card
      threat <- sideSchemeThreat sid
      resolve [SideSchemeMessage sid DefeatSideScheme]
      code' <- villainCode
      stones <- length <$> stonesInPlay
      liftIO do
        -- 4, and hinder 1 per player
        threat `shouldBe` 5
        code' `shouldSatisfy` (/= code)
        stones `shouldBe` 1

versionsSpec :: Spec
versionsSpec = describe "The five versions of Loki" do
  it "Loki (21160) cannot take damage while a side scheme is in play" do
    tg <- lokiGame
    let you = player1 tg
    inGame tg do
      lokiIs VillainCards.loki1
      heroAttackFor you 3
      held <- villainDamageNow
      removeSideSchemes
      heroAttackFor you 3
      dealt <- villainDamageNow
      liftIO $ (held, dealt) `shouldBe` (0, 3)

  it "Loki (21161) retaliates 1" do
    tg <- lokiGame
    let you = player1 tg
    inGame tg do
      lokiIs VillainCards.loki2
      asHero you
      choose "attack" (isHeroAttack you)
      chooseVillain
      resolve []
      dmg <- damageOf you
      liftIO $ dmg `shouldBe` 1

  it "Loki (21162) and (21163) are stalwart" do
    tg <- lokiGame
    inGame tg do
      for_ [VillainCards.loki3, VillainCards.loki4] \def -> do
        lokiIs def
        vid <- villainIdNow
        resolve [VillainMessage vid (VillainStunned GameRef)]
        stunned <- villainStunned <$> villainAttrs
        liftIO $ stunned `shouldBe` False

  it "Loki (21164)'s attacks gain piercing: your tough status card is discarded and you take damage" do
    tg <- lokiGame
    let you = player1 tg
    inGame tg do
      lokiIs VillainCards.loki5
      removeGauntlet
      asHero you
      resolve [IdentityMessage you IdentityBecomeTough]
      _ <- stackEncounterDeck [MinionCards.hydraMercenary]
      vid <- villainIdNow
      runNow [VillainMessage vid (VillainAttacks you)]
      pass
      resolve []
      dmg <- damageOf you
      tough <- (.playerIdentityTough) <$> playerAttrs you
      liftIO $ (dmg > 0, tough) `shouldBe` (True, False)

  it "each version's printed SCH and ATK, the Gauntlet's +1 each" do
    tg <- lokiGame
    inGame tg do
      stats <- for lokis \def -> do
        lokiIs def
        a <- villainAttrs
        (,) <$> getModifiedScheme a <*> getModifiedAttack a
      liftIO $ stats `shouldBe` [(3, 3), (3, 2), (2, 4), (4, 2), (2, 3)]

attachmentsSpec :: Spec
attachmentsSpec = describe "Loki's attachments" do
  it "Loki's Staff: +1 ATK; after your basic attack against Loki, spend [energy][physical] to discard it" do
    tg <- lokiGame
    let you = player1 tg
    inGame tg do
      lokiIs VillainCards.loki3
      asHero you
      clearHand you
      energy <- putInHand you "01088"
      strength <- putInHand you "01090"
      atk <- getModifiedAttack =<< villainAttrs
      staff <- attachNow you AttachmentCards.lokisStaff
      atk' <- getModifiedAttack =<< villainAttrs
      choose "attack" (isHeroAttack you)
      chooseVillain
      offered <- answerUntil (isAbilityOf (AttachmentRef staff))
      liftIO $ (atk' - atk, offered) `shouldBe` (1, True)
      choose "discard the Staff" (isAbilityOf (AttachmentRef staff))
      payWith [energy, strength]
      resolve []
      attachmentsOf AttachmentCards.lokisStaff `shouldReturn` []

  it "Loki's Crown: +1 SCH; after your basic attack, spend [mental][physical] to discard it" do
    tg <- lokiGame
    let you = player1 tg
    inGame tg do
      lokiIs VillainCards.loki3
      asHero you
      clearHand you
      genius <- putInHand you "01089"
      strength <- putInHand you "01090"
      sch <- getModifiedScheme =<< villainAttrs
      crown <- attachNow you AttachmentCards.lokisCrown
      sch' <- getModifiedScheme =<< villainAttrs
      choose "attack" (isHeroAttack you)
      chooseVillain
      offered <- answerUntil (isAbilityOf (AttachmentRef crown))
      liftIO $ (sch' - sch, offered) `shouldBe` (1, True)
      choose "discard the Crown" (isAbilityOf (AttachmentRef crown))
      payWith [genius, strength]
      resolve []
      attachmentsOf AttachmentCards.lokisCrown `shouldReturn` []

  it "Loki's Cape is discarded with [energy][mental] after your basic attack" do
    tg <- lokiGame
    let you = player1 tg
    inGame tg do
      lokiIs VillainCards.loki3
      asHero you
      clearHand you
      energy <- putInHand you "01088"
      genius <- putInHand you "01089"
      cape <- attachNow you AttachmentCards.lokisCape
      choose "attack" (isHeroAttack you)
      chooseVillain
      offered <- answerUntil (isAbilityOf (AttachmentRef cape))
      liftIO $ offered `shouldBe` True
      choose "discard the Cape" (isAbilityOf (AttachmentRef cape))
      payWith [energy, genius]
      resolve []
      attachmentsOf AttachmentCards.lokisCape `shouldReturn` []

  it "Loki's Staff, as a boost card, attaches to Loki" do
    tg <- lokiGame
    inGame tg do
      lokiIs VillainCards.loki3
      removeGauntlet
      _ <- stackEncounterDeck [AttachmentCards.lokisStaff]
      vid <- villainIdNow
      resolve [VillainMessage vid VillainSchemes]
      staffs <- map attachmentEnemy <$> attachmentsOf AttachmentCards.lokisStaff
      liftIO $ staffs `shouldBe` [Just (EnemyVillainId vid)]

  it "Master of Illusions: no discard while Loki (21160) cannot take damage" do
    tg <- lokiGame
    let you = player1 tg
    inGame tg do
      lokiIs VillainCards.loki1
      _ <- attachNow you AttachmentCards.masterOfIllusions
      deck <- encounterDeckSize
      heroAttackFor you 3
      deck' <- encounterDeckSize
      dmg <- villainDamageNow
      liftIO $ (deck - deck', dmg) `shouldBe` (0, 0)

  it "Master of Illusions: nor while a tough status card prevents the damage" do
    tg <- lokiGame
    let you = player1 tg
    inGame tg do
      lokiIs VillainCards.loki3
      _ <- attachNow you AttachmentCards.masterOfIllusions
      vid <- villainIdNow
      resolve [VillainMessage vid VillainBecomeTough]
      discard <- length <$> encounterDiscardNow
      heroAttackFor you 3
      discard' <- length <$> encounterDiscardNow
      tough <- villainTough <$> villainAttrs
      liftIO $ (discard' - discard, tough) `shouldBe` (0, False)

  it "Master of Illusions: the top card discarded; not a treachery, the damage is dealt" do
    tg <- lokiGame
    let you = player1 tg
    inGame tg do
      lokiIs VillainCards.loki3
      mi <- attachNow you AttachmentCards.masterOfIllusions
      _ <- stackEncounterDeck [MinionCards.hydraMercenary]
      heroAttackFor you 3
      dmg <- villainDamageNow
      top <- headMay <$> encounterDiscardCodes
      attached <- villainAttachments <$> villainAttrs
      liftIO $ (dmg, top, member mi attached) `shouldBe` (3, Just (toCardCode MinionCards.hydraMercenary), True)

  it "Master of Illusions: a treachery discarded, all damage from the attack is prevented and it is discarded" do
    tg <- lokiGame
    let you = player1 tg
    inGame tg do
      lokiIs VillainCards.loki3
      _ <- attachNow you AttachmentCards.masterOfIllusions
      _ <- stackEncounterDeck [TreacheryCards.deviousSorcery]
      heroAttackFor you 3
      dmg <- villainDamageNow
      left <- attachmentsOf AttachmentCards.masterOfIllusions
      liftIO $ (dmg, left) `shouldBe` (0, [])

  it "Master of Illusions: a treachery discarded, every instance of damage the attack deals Loki is prevented until it ends" do
    tg <- lokiGame
    let you = player1 tg
    inGame tg do
      lokiIs VillainCards.loki3
      _ <- attachNow you AttachmentCards.masterOfIllusions
      _ <- stackEncounterDeck [TreacheryCards.deviousSorcery]
      vid <- villainIdNow
      eid <- getRandom
      let loki = EnemyRef (EnemyVillainId vid)
          -- an attack event dealing its damage in two instances, its attack ending after both
          instance' n = DamageEnemy loki (EventRef eid) (toDamage n (FromPlayerAttack you))
      runNow [Ask you (ChooseOne [Label "the attack" [instance' 3, instance' 2]])]
      choose "the attack" (const True)
      chooseIf isAnyAbility
      resolve []
      during <- villainDamageNow
      left <- attachmentsOf AttachmentCards.masterOfIllusions
      resolve [CheckWindows [W.Window W.After (W.IdentityAttack you (EnemyVillainId vid))]]
      heroAttackFor you 3
      after <- villainDamageNow
      liftIO $ (during, left, after) `shouldBe` (0, [], 3)

  it "Master of Illusions answers an attack's overkill, and prevents the rest of that attack's damage" do
    tg <- lokiGame
    let you = player1 tg
    inGame tg do
      lokiIs VillainCards.loki3
      _ <- attachNow you AttachmentCards.masterOfIllusions
      _ <- stackEncounterDeck [TreacheryCards.deviousSorcery]
      vid <- villainIdNow
      let loki = EnemyRef (EnemyVillainId vid)
      runNow [Ask you (ChooseOne [Label "the attack" [DamageEnemy loki (toSource you) (toDamage 2 FromOverkill)]])]
      choose "the attack" (const True)
      chooseIf isAnyAbility
      resolve []
      dmg <- villainDamageNow
      left <- attachmentsOf AttachmentCards.masterOfIllusions
      liftIO $ (dmg, left) `shouldBe` (0, [])

  it "Master of Illusions does not answer damage that is not from an attack" do
    tg <- lokiGame
    let you = player1 tg
    inGame tg do
      lokiIs VillainCards.loki3
      _ <- attachNow you AttachmentCards.masterOfIllusions
      _ <- stackEncounterDeck [TreacheryCards.deviousSorcery]
      damageFromGame 2
      dmg <- villainDamageNow
      left <- length <$> attachmentsOf AttachmentCards.masterOfIllusions
      liftIO $ (dmg, left) `shouldBe` (2, 1)

treacheriesSpec :: Spec
treacheriesSpec = describe "The Loki set's treacheries" do
  it "Devious Sorcery (alter-ego): you are stunned; already stunned, 2 threat on the main scheme" do
    tg <- lokiGame
    let you = player1 tg
    inGame tg do
      before <- mainSchemeThreatNow
      _ <- revealNow you TreacheryCards.deviousSorcery
      resolve []
      stunned <- isStunned you
      mid' <- mainSchemeThreatNow
      _ <- revealNow you TreacheryCards.deviousSorcery
      resolve []
      after <- mainSchemeThreatNow
      liftIO $ (stunned, mid' - before, after - mid') `shouldBe` (True, 0, 2)

  it "Devious Sorcery (hero): you are stunned; already stunned, you take 2 damage" do
    tg <- lokiGame
    let you = player1 tg
    inGame tg do
      asHero you
      -- no Enhanced Spider-Sense to cancel it
      clearHand you
      _ <- revealNow you TreacheryCards.deviousSorcery
      resolve []
      stunned <- isStunned you
      dmg <- damageOf you
      _ <- revealNow you TreacheryCards.deviousSorcery
      resolve []
      dmg' <- damageOf you
      liftIO $ (stunned, dmg, dmg') `shouldBe` (True, 0, 2)

  it "Infinite Mischief: the stone discard pile is shuffled into the stone deck, and its top card revealed" do
    tg <- lokiGame
    let you = player1 tg
    inGame tg do
      clearStones
      Pile _ discard <- stonePile
      _ <- revealNow you TreacheryCards.infiniteMischief
      resolve []
      Pile deck discard' <- stonePile
      stones <- length <$> stonesInPlay
      liftIO $ (length discard, length discard', length deck, stones) `shouldBe` (1, 0, 5, 1)

  it "Infinite Mischief's boost: the top stone is discarded, its boost icons counted for the activation" do
    tg <- lokiGame
    inGame tg do
      lokiIs VillainCards.loki3
      removeGauntlet
      clearStones
      stoneOnTop EnvironmentCards.timeStone
      _ <- stackEncounterDeck [TreacheryCards.infiniteMischief]
      before <- mainSchemeThreatNow
      vid <- villainIdNow
      resolve [VillainMessage vid VillainSchemes]
      after <- mainSchemeThreatNow
      Pile _ discarded <- stonePile
      -- his 1 SCH and the Time Stone's 4 boost icons
      liftIO $ (after - before, map toCardCode discarded) `shouldBe` (5, [toCardCode EnvironmentCards.timeStone])

  it "The Trickster's boost: Loki is given another boost card and a tough status card" do
    tg <- lokiGame
    inGame tg do
      lokiIs VillainCards.loki3
      removeGauntlet
      _ <- stackEncounterDeck [TreacheryCards.theTrickster, MinionCards.hydraMercenary]
      vid <- villainIdNow
      resolve [VillainMessage vid VillainSchemes]
      a <- villainAttrs
      liftIO $ (villainTough a, length (villainBoostCards a)) `shouldBe` (True, 0)
