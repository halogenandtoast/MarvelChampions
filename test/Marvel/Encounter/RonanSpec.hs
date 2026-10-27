-- | Ronan the Accuser (The Galaxy's Most Wanted, scenario 5) and the Kree Militants
-- modular set, each card played as a table would.
module Marvel.Encounter.RonanSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Attachment.Cards qualified as AttachmentCards
import Marvel.Attachment.Types (Attachment, Attrs (..))
import Marvel.EncounterSet (EncounterSet (KreeMilitants))
import Marvel.Environment.Cards qualified as EnvironmentCards
import Marvel.Attack (Attack (..))
import Marvel.Hp (HP (..))
import Marvel.MainScheme.Cards qualified as MainSchemeCards
import Marvel.Minion.Cards qualified as MinionCards
import Marvel.Minion.Types qualified as Minion
import Marvel.SideScheme.Cards qualified as SideSchemeCards
import Marvel.Support.Cards qualified as SupportCards
import Marvel.Treachery.Cards qualified as TreacheryCards
import Marvel.Villain.Types qualified as Villain
import Marvel.Villain.Types (villainIsTough)
import TestImport

ronan :: [Text] -> IO TestGame
ronan = testGameAgainst "16103"

ronanExpert :: [Text] -> IO TestGame
ronanExpert heroes = do
  (app, ran, logged) <- dealtGameLogged (ScenarioSetup "16103" [KreeMilitants] Expert) (map starter heroes) 1
  seats <- runGameApp app (getsGame gameSeats)
  let tg = TestGame app ran logged seats
  inGame tg toFirstTurn
  pure tg

attachmentsOf :: CardDef -> GameAppT [Attrs Attachment]
attachmentsOf def = getsGame (filter ((== toCardCode def) . toCardCode . attachmentCardDef) . map toAttrs . HashMap.elems . gameAttachments)

attachmentOf :: CardDef -> GameAppT (Attrs Attachment)
attachmentOf def = attachmentsOf def >>= \case
  [a] -> pure a
  as -> error $ "expected one " <> show (cdName def) <> ", found " <> show (length as)

environmentCodes :: GameAppT [CardCode]
environmentCodes = getsGame (map (toCardCode . getCardDef) . HashMap.elems . gameEnvironments)

sideSchemeCodes :: GameAppT [CardCode]
sideSchemeCodes = getsGame (map (toCardCode . getCardDef) . HashMap.elems . gameSideSchemes)

mainSchemeCode :: GameAppT CardCode
mainSchemeCode = getsGame (maybe (error "no main scheme") (toCardCode . getCardDef) . headMay . HashMap.elems . gameMainSchemes)

milanoExhausted :: GameAppT Bool
milanoExhausted = supportsWith (toCardCode SupportCards.milano) >>= \case
  [m] -> pure m.supportExhausted
  ms -> error $ "expected the Milano, found " <> show (length ms)

exhaustTheMilano :: GameAppT ()
exhaustTheMilano = supportsWith (toCardCode SupportCards.milano) >>= \case
  [m] -> resolve [SupportMessage (supportId m) ExhaustedSupport]
  _ -> error "no Milano"

villainAttrs :: GameAppT (Attrs Villain)
villainAttrs = toAttrs <$> getVillain

-- | The Power Stone back on Ronan.
stoneOnRonan :: GameAppT ()
stoneOnRonan = do
  vid <- villainIdNow
  stone <- maybe (error "no Power Stone") pure =<< getPowerStone
  resolve [AttachmentMessage stone (AttachedToEnemy (EnemyVillainId vid))]

spec :: Spec
spec = do
  scenarioSpec
  villainSpec
  mainSchemesSpec
  encounterSpec
  kreeMilitantsSpec

finished :: GameAppT (Maybe FinishedStatus)
finished = getsGame \g -> case gameState g of
  Finished status -> Just status
  _ -> Nothing

asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass

-- | The Power Stone moved to the player.
stoneTo :: IdentityId -> GameAppT ()
stoneTo you = do
  stone <- maybe (error "no Power Stone") pure =<< getPowerStone
  resolve [AttachmentMessage stone (AttachedToIdentity you)]

-- | Ronan's tough status card taken off.
untough :: GameAppT ()
untough = villainIdNow >>= \vid -> resolve [VillainMessage vid (VillainDamaged GameRef (toDamage 1 FromAbility))]

villainBoosts :: GameAppT Int
villainBoosts = length . Villain.villainBoostCards <$> villainAttrs

-- | The treachery's windows passed and its "When Revealed" resolved.
settle :: GameAppT ()
settle = pass >> pressWhenRevealed >> pass

-- | Past the windows to the question of who defends.
toDefense :: GameAppT ()
toDefense = do
  defense <- offers isNoDefender
  declinable <- offers (isLabel "$label.noResponse")
  unless defense do
    if declinable then choose "no response" (isLabel "$label.noResponse") else choose "a forced ability" isAnyAbility
    toDefense

scenarioSpec :: Spec
scenarioSpec = describe "Ronan the Accuser (scenario)" do
  it "Interception Imminent's setup: the Kree Command Ship and the Milano in play, the Universal Weapon on Ronan, the Power Stone on the first player" do
    tg <- ronan ["spider-man", "captain-marvel"]
    inGame tg do
      vid <- villainIdNow
      envs <- environmentCodes
      weapon <- attachmentOf AttachmentCards.universalWeapon
      holder <- getPowerStoneHolder
      milano <- supportsWith (toCardCode SupportCards.milano)
      code <- mainSchemeCode
      threat <- mainSchemeThreatNow
      deck <- map toCardCode <$> scenarioEncounterDeckNow
      liftIO do
        envs `shouldBe` [toCardCode EnvironmentCards.kreeCommandShip]
        attachmentEnemy weapon `shouldBe` Just (EnemyVillainId vid)
        holder `shouldBe` Just (IdentityCharacter (player1 tg))
        length milano `shouldBe` 1
        code `shouldBe` toCardCode MainSchemeCards.interceptionImminent
        -- 2 per player
        threat `shouldBe` 4
        deck `shouldSatisfy` notElem (toCardCode EnvironmentCards.kreeCommandShip)

  it "Ronan the Accuser (I) enters play tough (toughness), with 14 hit points per player" do
    tg <- ronan ["spider-man", "captain-marvel"]
    inGame tg do
      v <- getVillain
      liftIO do
        villainIsTough v `shouldBe` True
        Villain.villainHp (toAttrs v) `shouldBe` HP 28
        toCardCode (getCardDef v) `shouldBe` "16103"

  it "the encounter deck is the Ronan the Accuser, Power Stone, Ship Command, Kree Militants and standard sets" do
    tg <- ronan ["spider-man"]
    inGame tg do
      deck <- map toCardCode <$> scenarioEncounterDeckNow
      dealt <- concatMap (map toCardCode) <$> (traverse dealtTo =<< players)
      discard <- map toCardCode <$> encounterDiscardNow
      let cards = deck <> dealt <> discard
      liftIO do
        for_ [TreacheryCards.youStandAccused, TreacheryCards.singleMindedFury, MinionCards.kreeCommando, SideSchemeCards.pincerManeuver] \c ->
          toCardCode c `elem` cards `shouldBe` True
        -- Ship Command's Blind Side, and the standard set's Advance
        "16145" `elem` cards `shouldBe` True
        "01186" `elem` cards `shouldBe` True

  it "expert: Ronan the Accuser (II), who reveals Cut the Power as setup ends" do
    tg <- ronanExpert ["spider-man"]
    inGame tg do
      v <- getVillain
      schemes <- sideSchemeCodes
      liftIO do
        toCardCode (getCardDef v) `shouldBe` "16104"
        villainIsTough v `shouldBe` True
        schemes `shouldBe` [toCardCode SideSchemeCards.cutThePower]

villainSpec :: Spec
villainSpec = describe "Ronan the Accuser (villain)" do
  it "attacking the player who controls the Power Stone, he is given another boost card" do
    tg <- ronan ["spider-man"]
    let you = player1 tg
    inGame tg do
      vid <- villainIdNow
      -- Shocker's 2 boost icons and Hydra Mercenary's 1, on his 2 ATK
      _ <- stackEncounterDeck [MinionCards.shocker, MinionCards.hydraMercenary]
      resolve [VillainMessage vid (VillainAttacks you)]
      pass
      damageOf you `shouldReturn` 5

  it "attacking a player without the Power Stone, he is given none more" do
    tg <- ronan ["spider-man"]
    let you = player1 tg
    inGame tg do
      vid <- villainIdNow
      stoneOnRonan
      _ <- stackEncounterDeck [MinionCards.shocker, MinionCards.hydraMercenary]
      resolve [VillainMessage vid (VillainAttacks you)]
      pass
      damageOf you `shouldReturn` 4

  it "scheming against the player who controls the Power Stone, he is given another boost card too" do
    tg <- ronan ["spider-man", "captain-marvel"]
    inGame tg do
      vid <- villainIdNow
      _ <- stackEncounterDeck [MinionCards.shocker, MinionCards.hydraMercenary]
      before <- mainSchemeThreatNow
      resolve [VillainMessage vid VillainSchemes]
      pass
      after <- mainSchemeThreatNow
      -- his 2 SCH, 2 and 1 boost icons
      liftIO $ after - before `shouldBe` 5

  it "is stalwart with the Universal Weapon: he is not stunned" do
    tg <- ronan ["spider-man"]
    inGame tg do
      vid <- villainIdNow
      resolve [GiveStatus (VillainRef vid) StunnedStatus, GiveStatus (VillainRef vid) ConfusedStatus]
      a <- villainAttrs
      liftIO $ (Villain.villainStunned a, Villain.villainConfused a) `shouldBe` (False, False)

  it "Ronan (I) defeated: Ronan (II), tough again, reveals Cut the Power; Ronan (II) defeated wins" do
    tg <- ronan ["spider-man"]
    inGame tg do
      vid <- villainIdNow
      untough
      resolve [VillainMessage vid VillainDefeated]
      pass
      v <- getVillain
      schemes <- sideSchemeCodes
      weapon <- attachmentOf AttachmentCards.universalWeapon
      liftIO do
        toCardCode (getCardDef v) `shouldBe` "16104"
        villainIsTough v `shouldBe` True
        Villain.villainHp (toAttrs v) `shouldBe` HP 18
        schemes `shouldBe` [toCardCode SideSchemeCards.cutThePower]
        attachmentEnemy weapon `shouldBe` Just (EnemyVillainId vid)
      resolve [VillainMessage vid VillainDefeated]
      finished `shouldReturn` Just Won

  it "expert: Ronan (II) defeated: Ronan (III), with retaliate 1, reveals Superior Tactics, which takes the Power Stone" do
    tg <- ronanExpert ["spider-man"]
    inGame tg do
      vid <- villainIdNow
      resolve [VillainMessage vid VillainDefeated]
      pass
      v <- getVillain
      schemes <- sideSchemeCodes
      holder <- getPowerStoneHolder
      liftIO do
        toCardCode (getCardDef v) `shouldBe` "16105"
        Villain.villainHp (toAttrs v) `shouldBe` HP 25
        schemes `shouldMatchList` [toCardCode SideSchemeCards.cutThePower, toCardCode SideSchemeCards.superiorTactics]
        holder `shouldBe` Just (VillainCharacter vid)

mainSchemesSpec :: Spec
mainSchemesSpec = describe "Ronan the Accuser (main schemes)" do
  it "Interception Imminent: the first player may exhaust the Milano to remove 3 threat from it" do
    tg <- ronan ["spider-man", "captain-marvel"]
    inGame tg do
      m <- mainSchemeIdNow
      before <- mainSchemeThreatNow
      choose "Interception Imminent" (isAbilityOf m)
      pass
      after <- mainSchemeThreatNow
      exhausted <- milanoExhausted
      liftIO $ (before - after, exhausted) `shouldBe` (3, True)

  it "Interception Imminent's action is the first player's only, and needs the Milano ready" do
    tg <- ronan ["spider-man", "captain-marvel"]
    inGame tg do
      m <- mainSchemeIdNow
      endTurn
      secondOffered <- offers (isAbilityOf m)
      liftIO $ secondOffered `shouldBe` False
    tg' <- ronan ["spider-man", "captain-marvel"]
    inGame tg' do
      m <- mainSchemeIdNow
      exhaustTheMilano
      exhaustedOffered <- offers (isAbilityOf m)
      liftIO $ exhaustedOffered `shouldBe` False

  it "Interception Imminent completed: \"Take What Is Mine\", which takes the Power Stone for Ronan" do
    tg <- ronan ["spider-man"]
    inGame tg do
      m <- mainSchemeIdNow
      vid <- villainIdNow
      resolve [AdvanceMainScheme m]
      pass
      code <- mainSchemeCode
      holder <- getPowerStoneHolder
      boosts <- villainBoosts
      liftIO do
        code `shouldBe` toCardCode MainSchemeCards.takeWhatIsMine
        holder `shouldBe` Just (VillainCharacter vid)
        boosts `shouldBe` 0

  it "\"Take What Is Mine\" with the Power Stone on Ronan already gives him a facedown boost card" do
    tg <- ronan ["spider-man"]
    inGame tg do
      m <- mainSchemeIdNow
      stoneOnRonan
      resolve [AdvanceMainScheme m]
      pass
      villainBoosts `shouldReturn` 1

  it "\"Take What Is Mine\": no threat is removed from it while the Power Stone is on Ronan" do
    tg <- ronan ["spider-man"]
    let you = player1 tg
    inGame tg do
      m <- mainSchemeIdNow
      resolve [AdvanceMainScheme m]
      pass
      asHero you
      m2 <- mainSchemeIdNow
      before <- mainSchemeThreatNow
      thwartOffered <- offers (isHeroThwart you)
      resolve [MainSchemeMessage m2 (MainSchemeThreatRemoved GameRef 1), MainSchemeMessage m2 (MainSchemeThwarted (IdentityRef you) 1)]
      locked <- mainSchemeThreatNow
      stoneTo you
      freed <- offers (isHeroThwart you)
      resolve [MainSchemeMessage m2 (MainSchemeThreatRemoved GameRef 1)]
      after <- mainSchemeThreatNow
      liftIO $ (thwartOffered, before - locked, freed, before - after) `shouldBe` (False, 0, True, 1)

  it "\"Take What Is Mine\" completed, the players lose" do
    tg <- ronan ["spider-man"]
    inGame tg do
      m <- mainSchemeIdNow
      resolve [AdvanceMainScheme m]
      pass
      m2 <- mainSchemeIdNow
      resolve [AdvanceMainScheme m2]
      finished `shouldReturn` Just Lost

encounterSpec :: Spec
encounterSpec = describe "Ronan the Accuser (encounter set)" do
  it "Kree Command Ship: the first player may exhaust the Milano and spend a resource to cancel a treachery's When Revealed" do
    tg <- ronan ["spider-man"]
    let you = player1 tg
    inGame tg do
      [ship] <- getsGame (HashMap.keys . gameEnvironments)
      shooter <- putInHand you "01008"
      before <- mainSchemeThreatNow
      _ <- revealNow you TreacheryCards.youStandAccused
      choose "the Kree Command Ship" (isAbilityOf ship)
      payWith [shooter]
      pass
      after <- mainSchemeThreatNow
      exhausted <- milanoExhausted
      discard <- encounterDiscardCodes
      liftIO do
        (after - before, exhausted) `shouldBe` (0, True)
        discard `shouldSatisfy` elem (toCardCode TreacheryCards.youStandAccused)

  it "Kree Command Ship: not with the Milano exhausted" do
    tg <- ronan ["spider-man"]
    let you = player1 tg
    inGame tg do
      [ship] <- getsGame (HashMap.keys . gameEnvironments)
      exhaustTheMilano
      _ <- revealNow you TreacheryCards.youStandAccused
      offers (isAbilityOf ship) `shouldReturn` False

  it "Universal Weapon: a hero takes 2 damage and is dealt an encounter card to shuffle it into the encounter deck" do
    tg <- ronan ["spider-man"]
    let you = player1 tg
    inGame tg do
      asHero you
      vid <- villainIdNow
      weapon <- attachmentOf AttachmentCards.universalWeapon
      dealt <- length <$> dealtTo you
      choose "the Universal Weapon" (isAbilityOf (attachmentId weapon))
      pass
      damage <- damageOf you
      dealt' <- length <$> dealtTo you
      left <- attachmentsOf AttachmentCards.universalWeapon
      deck <- map toCardCode <$> scenarioEncounterDeckNow
      resolve [GiveStatus (VillainRef vid) StunnedStatus]
      stunned <- Villain.villainStunned <$> villainAttrs
      liftIO do
        (damage, dealt' - dealt, length left) `shouldBe` (2, 1, 0)
        deck `shouldSatisfy` elem (toCardCode AttachmentCards.universalWeapon)
        -- no longer stalwart
        stunned `shouldBe` True

  it "Universal Weapon's boost attaches it to Ronan" do
    tg <- ronan ["spider-man"]
    inGame tg do
      vid <- villainIdNow
      weapon <- attachmentOf AttachmentCards.universalWeapon
      resolve [RemoveFromGame (AttachmentRef (attachmentId weapon))]
      card <- encounterCard AttachmentCards.universalWeapon
      resolve [RevealBoostCard card (EnemyVillainId vid), ClearBoosts]
      weapon' <- attachmentOf AttachmentCards.universalWeapon
      liftIO $ attachmentEnemy weapon' `shouldBe` Just (EnemyVillainId vid)

  it "Fanaticism surges, attaches to Ronan with 1 fury counter and 1 per player: +1 ATK, and his attack gains piercing" do
    tg <- ronan ["spider-man"]
    let you = player1 tg
    inGame tg do
      vid <- villainIdNow
      dealt <- length <$> dealtTo you
      _ <- revealNow you AttachmentCards.fanaticism
      pass
      dealt' <- length <$> dealtTo you
      fan <- attachmentOf AttachmentCards.fanaticism
      liftIO do
        dealt' - dealt `shouldBe` 1
        attachmentEnemy fan `shouldBe` Just (EnemyVillainId vid)
        attachmentCounters fan `shouldBe` 2
      stoneOnRonan
      resolve [GiveStatus (IdentityRef you) ToughStatus]
      _ <- stackEncounterDeck [MinionCards.hydraMercenary]
      resolve [VillainMessage vid (VillainAttacks you)]
      pass
      -- 2 ATK, +1, and a boost icon, through the tough status card
      damageOf you `shouldReturn` 4
      (attachmentCounters <$> attachmentOf AttachmentCards.fanaticism) `shouldReturn` 1

  it "Fanaticism: the attack gains overkill too, and with its last fury counter gone it is discarded" do
    tg <- ronan ["spider-man"]
    let you = player1 tg
    inGame tg do
      vid <- villainIdNow
      _ <- revealNow you AttachmentCards.fanaticism
      pass
      fan <- attachmentOf AttachmentCards.fanaticism
      resolve [RemoveCounters (AttachmentRef (attachmentId fan)) 1]
      asHero you
      runNow [VillainMessage vid (VillainAttacks you)]
      toDefense
      attack <- Villain.villainAttacking <$> villainAttrs
      choose "no defender" isNoDefender
      pass
      left <- attachmentsOf AttachmentCards.fanaticism
      discard <- encounterDiscardCodes
      liftIO do
        (attackOverkill <$> attack, attackPiercing <$> attack) `shouldBe` (Just True, Just True)
        length left `shouldBe` 0
        discard `shouldSatisfy` elem (toCardCode AttachmentCards.fanaticism)

  it "Cut the Power: its crisis icon keeps the main scheme from being thwarted" do
    tg <- ronan ["spider-man"]
    let you = player1 tg
    inGame tg do
      card <- revealNow you SideSchemeCards.cutThePower
      pass
      threat <- sideSchemeThreat (sideSchemeOf card)
      asHero you
      m <- mainSchemeIdNow
      choose "thwart" (isHeroThwart you)
      mainOffered <- offers (isTargetChoice m)
      sideOffered <- offers (isTargetChoice (sideSchemeOf card))
      liftIO $ (threat, mainOffered, sideOffered) `shouldBe` (3, False, True)

  it "Cut the Power's boost: exhaust the Milano, or 2 threat on the main scheme" do
    tg <- ronan ["spider-man"]
    inGame tg do
      vid <- villainIdNow
      card <- encounterCard SideSchemeCards.cutThePower
      runNow [RevealBoostCard card (EnemyVillainId vid)]
      threatOffered <- offers (isLabel (countLabel "placeThreatOnMainScheme" 2))
      choose "exhaust the Milano" (isLabel "$label.exhaustTheMilano")
      pass
      exhausted <- milanoExhausted
      before <- mainSchemeThreatNow
      card' <- encounterCard SideSchemeCards.cutThePower
      runNow [RevealBoostCard card' (EnemyVillainId vid)]
      pass
      after <- mainSchemeThreatNow
      liftIO $ (threatOffered, exhausted, after - before) `shouldBe` (True, True, 2)

  it "Pincer Maneuver: Hinder 2 per player on its 3, the first player's Milano removes 3, and two acceleration icons" do
    tg <- ronan ["spider-man"]
    let you = player1 tg
    inGame tg do
      card <- revealNow you SideSchemeCards.pincerManeuver
      pass
      let sid = sideSchemeOf card
      threat <- sideSchemeThreat sid
      before <- mainSchemeThreatNow
      resolve [AccelerateMainScheme]
      accelerated <- mainSchemeThreatNow
      choose "Pincer Maneuver" (isAbilityOf sid)
      pass
      after <- sideSchemeThreat sid
      exhausted <- milanoExhausted
      -- Interception Imminent's 2 per player and the two icons
      liftIO $ (threat, accelerated - before, after, exhausted) `shouldBe` (5, 4, 2, True)

  it "Superior Tactics takes the Power Stone for Ronan, and while in play keeps it on him" do
    tg <- ronan ["spider-man"]
    let you = player1 tg
    inGame tg do
      vid <- villainIdNow
      card <- revealNow you SideSchemeCards.superiorTactics
      pass
      let sid = sideSchemeOf card
      threat <- sideSchemeThreat sid
      taken <- getPowerStoneHolder
      untough
      resolve [VillainMessage vid (VillainDamaged (toSource you) (toDamage 3 (FromPlayerAttack you)))]
      kept <- getPowerStoneHolder
      resolve [SideSchemeMessage sid (SideSchemeThwarted (IdentityRef you) 2)]
      resolve [VillainMessage vid (VillainDamaged (toSource you) (toDamage 3 (FromPlayerAttack you)))]
      freed <- getPowerStoneHolder
      liftIO do
        threat `shouldBe` 2
        (taken, kept) `shouldBe` (Just (VillainCharacter vid), Just (VillainCharacter vid))
        freed `shouldBe` Just (IdentityCharacter you)

  it "Superior Tactics with the Power Stone on Ronan already: 1 more threat per player" do
    tg <- ronan ["spider-man"]
    let you = player1 tg
    inGame tg do
      stoneOnRonan
      card <- revealNow you SideSchemeCards.superiorTactics
      pass
      sideSchemeThreat (sideSchemeOf card) `shouldReturn` 3

  it "Single-Minded Fury: Ronan attacks the alter-ego holding the Power Stone; with no one holding it, it surges" do
    tg <- ronan ["spider-man"]
    let you = player1 tg
    inGame tg do
      dealt <- length <$> dealtTo you
      _ <- stackEncounterDeck [MinionCards.hydraMercenary, MinionCards.hydraMercenary]
      _ <- revealNow you TreacheryCards.singleMindedFury
      settle
      damage <- damageOf you
      dealt' <- length <$> dealtTo you
      -- his 3 or more damage took the Power Stone back
      holder <- getPowerStoneHolder
      _ <- revealNow you TreacheryCards.singleMindedFury
      settle
      damage' <- damageOf you
      dealt'' <- length <$> dealtTo you
      vid <- villainIdNow
      liftIO do
        (damage, dealt' - dealt) `shouldBe` (4, 0)
        holder `shouldBe` Just (VillainCharacter vid)
        (damage', dealt'' - dealt') `shouldBe` (4, 1)

  it "Single-Minded Fury's boost attaches the Power Stone to Ronan" do
    tg <- ronan ["spider-man"]
    inGame tg do
      vid <- villainIdNow
      card <- encounterCard TreacheryCards.singleMindedFury
      runNow [RevealBoostCard card (EnemyVillainId vid)]
      pass
      getPowerStoneHolder `shouldReturn` Just (VillainCharacter vid)

  it "Kree Physiology surges, and with Ronan tough already its player takes 1 damage" do
    tg <- ronan ["spider-man"]
    let you = player1 tg
    inGame tg do
      dealt <- length <$> dealtTo you
      _ <- revealNow you TreacheryCards.kreePhysiology
      settle
      dealt' <- length <$> dealtTo you
      damage <- damageOf you
      liftIO $ (dealt' - dealt, damage) `shouldBe` (1, 1)

  it "Kree Physiology gives Ronan a tough status card" do
    tg <- ronan ["spider-man"]
    let you = player1 tg
    inGame tg do
      untough
      _ <- revealNow you TreacheryCards.kreePhysiology
      settle
      tough <- villainIsTough <$> getVillain
      damage <- damageOf you
      liftIO $ (tough, damage) `shouldBe` (True, 0)

  it "\"You Stand Accused!\" (hero): Ronan attacks you with +1 ATK" do
    tg <- ronan ["spider-man"]
    let you = player1 tg
    inGame tg do
      stoneOnRonan
      asHero you
      _ <- stackEncounterDeck [MinionCards.hydraMercenary]
      _ <- revealNow you TreacheryCards.youStandAccused
      -- no one defends
      settle
      damageOf you `shouldReturn` 4

  it "\"You Stand Accused!\" (alter-ego): Ronan schemes with +1 SCH" do
    tg <- ronan ["spider-man"]
    let you = player1 tg
    inGame tg do
      stoneOnRonan
      _ <- stackEncounterDeck [MinionCards.hydraMercenary]
      before <- mainSchemeThreatNow
      _ <- revealNow you TreacheryCards.youStandAccused
      settle
      after <- mainSchemeThreatNow
      liftIO $ after - before `shouldBe` 4

  it "\"You Stand Accused!\"'s boost gives the villain another boost card" do
    tg <- ronan ["spider-man"]
    inGame tg do
      vid <- villainIdNow
      card <- encounterCard TreacheryCards.youStandAccused
      before <- villainBoosts
      runNow [RevealBoostCard card (EnemyVillainId vid)]
      after <- villainBoosts
      liftIO $ after - before `shouldBe` 1

kreeMilitantsSpec :: Spec
kreeMilitantsSpec = describe "Kree Militants" do
  it "Kree Combat Armor attaches to the enemy with the highest ATK, its player choosing among those tied" do
    tg <- ronan ["spider-man"]
    let you = player1 tg
    inGame tg do
      vid <- villainIdNow
      -- Kree Commando's 2 ATK, as Ronan's
      m <- addMinion you MinionCards.kreeCommando
      pass
      _ <- revealNow you AttachmentCards.kreeCombatArmor
      ronanOffered <- offers (isTargetChoice vid)
      choose "Kree Commando" (isTargetChoice m)
      pass
      armor <- attachmentOf AttachmentCards.kreeCombatArmor
      liftIO $ (ronanOffered, attachmentEnemy armor) `shouldBe` (True, Just (EnemyMinionId m))

  it "Kree Combat Armor: each attack on Ronan deals 1 less" do
    tg <- ronan ["spider-man"]
    let you = player1 tg
    inGame tg do
      untough
      _ <- revealNow you AttachmentCards.kreeCombatArmor
      pass
      asHero you
      choose "attack" (isHeroAttack you)
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 1

  it "Kree Combat Armor: a hero spends 3 resources of the same type to discard it" do
    tg <- ronan ["spider-man"]
    let you = player1 tg
    inGame tg do
      _ <- revealNow you AttachmentCards.kreeCombatArmor
      pass
      armor <- attachmentOf AttachmentCards.kreeCombatArmor
      asHero you
      clearHand you
      none <- offers (isAbilityOf (attachmentId armor))
      strength <- putInHand you "01090"
      shooter <- putInHand you "01008"
      choose "spend 3 physical" (isLabeledAbility (countLabel "spendPhysicalResources" 3))
      payWith [strength, shooter]
      pass
      left <- attachmentsOf AttachmentCards.kreeCombatArmor
      discard <- encounterDiscardCodes
      liftIO do
        (none, length left) `shouldBe` (False, 0)
        discard `shouldSatisfy` elem (toCardCode AttachmentCards.kreeCombatArmor)

  it "Kree Commando: patrol" do
    tg <- ronan ["spider-man"]
    let you = player1 tg
    inGame tg do
      asHero you
      mainOffered <- offers (isHeroThwart you)
      _ <- addMinion you MinionCards.kreeCommando
      pass
      patrolled <- offers (isHeroThwart you)
      liftIO $ (mainOffered, patrolled) `shouldBe` (True, False)

  it "Kree Commando's boost gives an attack piercing" do
    tg <- ronan ["spider-man"]
    let you = player1 tg
    inGame tg do
      vid <- villainIdNow
      stoneOnRonan
      resolve [GiveStatus (IdentityRef you) ToughStatus]
      _ <- stackEncounterDeck [MinionCards.kreeCommando]
      resolve [VillainMessage vid (VillainAttacks you)]
      pass
      -- 2 ATK and its 2 boost icons, through the tough status card
      damageOf you `shouldReturn` 4

  it "Kree Lieutenant: guard and stalwart" do
    tg <- ronan ["spider-man"]
    let you = player1 tg
    inGame tg do
      vid <- villainIdNow
      m <- addMinion you MinionCards.kreeLieutenant
      pass
      resolve [GiveStatus (MinionRef m) StunnedStatus]
      stunned <- getsGame (fmap (Minion.minionStunned . toAttrs) . HashMap.lookup m . gameMinions)
      asHero you
      choose "attack" (isHeroAttack you)
      ronanOffered <- offers (isTargetChoice vid)
      liftIO $ (stunned, ronanOffered) `shouldBe` (Just False, False)

  it "Kree Lieutenant's boost: +3 boost icons for an attack, none for a scheme" do
    tg <- ronan ["spider-man"]
    let you = player1 tg
    inGame tg do
      vid <- villainIdNow
      stoneOnRonan
      _ <- stackEncounterDeck [MinionCards.kreeLieutenant, MinionCards.kreeLieutenant]
      resolve [VillainMessage vid (VillainAttacks you)]
      pass
      damage <- damageOf you
      before <- mainSchemeThreatNow
      resolve [VillainMessage vid VillainSchemes]
      pass
      after <- mainSchemeThreatNow
      liftIO $ (damage, after - before) `shouldBe` (5, 2)

  it "Kree Private: quickstrike" do
    tg <- ronan ["spider-man"]
    let you = player1 tg
    inGame tg do
      asHero you
      _ <- addMinion you MinionCards.kreePrivate
      pass
      damageOf you `shouldReturn` 1

  it "Kree Private's boost gives an attack overkill" do
    tg <- ronan ["spider-man"]
    let you = player1 tg
    inGame tg do
      vid <- villainIdNow
      stoneOnRonan
      cat <- putIntoPlay you "01002"
      asHero you
      _ <- stackEncounterDeck [MinionCards.kreePrivate]
      runNow [VillainMessage vid (VillainAttacks you)]
      toDefense
      choose "Black Cat defends" \case
        AllyDefend a _ -> a == allyIdOf cat
        _ -> False
      pass
      -- 2 ATK and its 1 boost icon on Black Cat's 2 hit points: 1 past her
      damageOf you `shouldReturn` 1
