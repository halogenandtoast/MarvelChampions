-- | Hela (The Mad Titan's Shadow, scenario 4) and the Legions of Hel and Frost Giants
-- modular sets, each card played as a table would.
module Marvel.Encounter.HelaSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Ally.Cards qualified as AllyCards
import Marvel.Attachment.Cards qualified as AttachmentCards
import Marvel.Attachment.Types (Attachment)
import Marvel.Attachment.Types qualified as Attachment
import Marvel.EncounterSet (EncounterSet (FrostGiants, Hela, LegionsOfHel))
import Marvel.Hp (HP (..))
import Marvel.Keyword (Keyword (Overkill, Piercing))
import Marvel.MainScheme.Cards qualified as MainSchemeCards
import Marvel.Minion.Cards qualified as MinionCards
import Marvel.Minion.Types qualified as Minion
import Marvel.Modifier (Modifier (..))
import Marvel.SideScheme.Cards qualified as SideSchemeCards
import Marvel.Stats (Atk (..), Sch (..))
import Marvel.Status (StatusCard (..))
import Marvel.Trait (HasTraits (..), Trait (Shield, Undead))
import Marvel.Treachery.Cards qualified as TreacheryCards
import Marvel.Villain.Cards qualified as VillainCards
import Marvel.Villain.Types qualified as Villain
import Marvel.Window qualified as W
import TestImport

hela :: [Text] -> IO TestGame
hela = testGameAgainst "21136a"

helaExpert :: [Text] -> IO TestGame
helaExpert heroes = do
  (app, ran, logged) <- dealtGameLogged (ScenarioSetup "21136a" [LegionsOfHel, FrostGiants] Expert) (map starter heroes) 1
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

villainAttrs :: GameAppT (Attrs Villain)
villainAttrs = toAttrs <$> getVillain

villainCode :: GameAppT CardCode
villainCode = toCardCode . getCardDef <$> getVillain

-- | The cards Odin's Torment holds, attached faceup.
heldOnScheme :: GameAppT [CardCode]
heldOnScheme = do
  m <- mainSchemeIdNow
  map toCardCode <$> getMainSchemeHeldCards m

-- | The Hela set's cards set aside (the nemesis sets are set aside too).
setAsideCodes :: GameAppT [CardCode]
setAsideCodes = map toCardCode . filter ((== Just Hela) . cdEncounterSet . getCardDef) <$> getSetAsideEncounterCards

victoryCodes :: GameAppT [CardCode]
victoryCodes = map toCardCode <$> getVictoryDisplay

sideSchemeIn :: CardDef -> GameAppT (Maybe SideSchemeId)
sideSchemeIn def = getsGame (headMay . map fst . filter ((== toCardCode def) . toCardCode . getCardDef . snd) . HashMap.toList . gameSideSchemes)

minionsOf :: CardDef -> GameAppT [Attrs Minion]
minionsOf def = getsGame (filter ((== toCardCode def) . toCardCode . minionCardDef) . map toAttrs . HashMap.elems . gameMinions)

minionAttrs :: MinionId -> GameAppT (Maybe (Attrs Minion))
minionAttrs m = getsGame (fmap toAttrs . HashMap.lookup m . gameMinions)

attachmentsOf :: CardDef -> GameAppT [Attrs Attachment]
attachmentsOf def = getsGame (filter ((== toCardCode def) . toCardCode . Attachment.attachmentCardDef) . map toAttrs . HashMap.elems . gameAttachments)

inDiscard :: CardDef -> GameAppT Bool
inDiscard def = elem (toCardCode def) <$> encounterDiscardCodes

-- | The side scheme defeated, as the last threat removed from it defeats it.
defeatScheme :: IdentityId -> SideSchemeId -> GameAppT ()
defeatScheme p sid =
  resolve
    [ CheckWindows [W.Window W.When (W.DefeatedSideScheme sid)]
    , SideSchemeMessage sid DefeatSideScheme
    , DefeatedLeavesPlay (SideSchemeRef sid)
    , CheckWindows [W.Window W.After (W.DefeatedSideSchemeBy sid p Nothing)]
    ]

-- | Gnipahellir, revealed at setup, defeated.
defeatGnipahellir :: IdentityId -> GameAppT ()
defeatGnipahellir p = sideSchemeIn SideSchemeCards.gnipahellir >>= traverse_ (defeatScheme p)

-- | A side scheme card in the victory display, put there without its defeat.
sideSchemeToVictory :: GameAppT ()
sideSchemeToVictory = do
  c <- encounterCard SideSchemeCards.legionsOfHel
  resolve [AddCardToVictoryDisplay c]

-- | Hall of Nastrond revealed and defeated: Odin rescued.
rescueOdin :: IdentityId -> GameAppT (Attrs Ally)
rescueOdin p = do
  hall <- revealNow p SideSchemeCards.hallOfNastrond
  pass
  defeatScheme p (sideSchemeOf hall)
  allyIn (toCardCode AllyCards.odinCaptive)

-- | Hela's attack or damage defeating her: as much damage as she has hit points.
damageHela :: IdentityId -> GameAppT ()
damageHela p = do
  v <- villainAttrs
  resolve [VillainMessage (Villain.villainId v) (VillainDamaged (toSource p) (toDamage (fromIntegral $ unHp $ Villain.villainHp v) FromAbility))]

-- | Replace the player's deck with new copies of these cards, the first on top.
setDeck :: IdentityId -> [CardCode] -> GameAppT [PlayerCard]
setDeck ident codes = do
  withGame_ $ entitiesL . playersL . ix ident %~ \(PlayerIdentity a) -> PlayerIdentity a {playerIdentityDeck = Deck []}
  stackDeck ident codes

spec :: Spec
spec = do
  setupSpec
  villainSpec
  odinSpec
  sideSchemeSpec
  minionSpec
  attachmentSpec
  treacherySpec
  legionsOfHelSpec
  frostGiantsSpec

setupSpec :: Spec
setupSpec = describe "Hela: setup" do
  it "Hela (A1) on her Mystic side; Odin attached to Odin's Torment, Captive side faceup; Gnipahellir and Garm revealed; the rest of the road set aside" do
    tg <- hela ["spider-man"]
    let p = player1 tg
    inGame tg do
      code <- villainCode
      v <- villainAttrs
      scheme <- getsGame (map (toCardCode . getCardDef) . HashMap.elems . gameMainSchemes)
      held <- heldOnScheme
      odin <- alliesWith (toCardCode AllyCards.odinCaptive)
      gnip <- sideSchemeIn SideSchemeCards.gnipahellir
      threat <- maybe (pure 0) sideSchemeThreat gnip
      garm <- minionsOf MinionCards.garm
      aside <- setAsideCodes
      deck <- map toCardCode <$> scenarioEncounterDeckNow
      liftIO do
        code `shouldBe` toCardCode VillainCards.helaA1
        Villain.villainHp v `shouldBe` HP 8
        scheme `shouldBe` [toCardCode MainSchemeCards.odinsTorment]
        held `shouldBe` [toCardCode AllyCards.odinCaptive]
        -- attached to the scheme, he is no ally in play
        odin `shouldBe` []
        -- 2 threat and hinder 1 per player
        threat `shouldBe` 3
        map minionEngagedIdentity garm `shouldBe` [p]
        aside `shouldMatchList` map toCardCode [SideSchemeCards.gjallerbru, MinionCards.skurge, SideSchemeCards.hallOfNastrond, MinionCards.nidhogg]
        deck `shouldSatisfy` elem (toCardCode MinionCards.draugr)
        deck `shouldSatisfy` elem (toCardCode MinionCards.laufey)
        deck `shouldSatisfy` notElem (toCardCode SideSchemeCards.gnipahellir)

  it "expert: Hela (B1), 9 hit points per player" do
    tg <- helaExpert ["spider-man"]
    inGame tg do
      code <- villainCode
      v <- villainAttrs
      liftIO do
        code `shouldBe` toCardCode VillainCards.helaB1
        Villain.villainHp v `shouldBe` HP 9

villainSpec :: Spec
villainSpec = describe "Hela (villain)" do
  it "Odin's Torment: Hela would be defeated with Odin attached: her attachments are discarded and she flips to Wounded, her status cards kept" do
    tg <- hela ["spider-man"]
    let p = player1 tg
    inGame tg do
      _ <- revealNow p AttachmentCards.nightsword
      pass
      vid <- villainIdNow
      resolve [VillainMessage vid (VillainStunned (toSource p))]
      damageHela p
      code <- villainCode
      v <- villainAttrs
      sword <- inDiscard AttachmentCards.nightsword
      done <- finished
      liftIO do
        code `shouldBe` toCardCode VillainCards.helaA2
        Villain.villainStunned v `shouldBe` True
        Villain.villainAttachments v `shouldBe` mempty
        sword `shouldBe` True
        done `shouldBe` Nothing

  it "Wounded: she takes damage but cannot be defeated" do
    tg <- hela ["spider-man"]
    let p = player1 tg
    inGame tg do
      damageHela p
      vid <- villainIdNow
      resolve [VillainMessage vid (VillainDamaged (toSource p) (toDamage 30 FromAbility))]
      code <- villainCode
      done <- finished
      liftIO $ (code, done) `shouldBe` (toCardCode VillainCards.helaA2, Nothing)

  it "Wounded: after a side scheme is defeated she flips to Mystic, her hit points reset with the victory display's bonus; Gnipahellir's chain goes on, the other player dealt a card" do
    tg <- hela ["spider-man", "captain-marvel"]
    let (p1, p2) = (player1 tg, player2 tg)
    inGame tg do
      damageHela p1
      dealt1 <- length <$> dealtTo p1
      dealt2 <- length <$> dealtTo p2
      defeatGnipahellir p1
      code <- villainCode
      v <- villainAttrs
      vd <- victoryCodes
      gjallerbru <- sideSchemeIn SideSchemeCards.gjallerbru
      skurge <- minionsOf MinionCards.skurge
      aside <- setAsideCodes
      dealt1' <- length <$> dealtTo p1
      dealt2' <- length <$> dealtTo p2
      liftIO do
        code `shouldBe` toCardCode VillainCards.helaA1
        -- 8 per player, and 2 per player for Gnipahellir in the victory display
        (Villain.villainHp v, Villain.villainMaxHp v) `shouldBe` (HP 20, HP 20)
        vd `shouldBe` [toCardCode SideSchemeCards.gnipahellir]
        isJust gjallerbru `shouldBe` True
        map minionEngagedIdentity skurge `shouldBe` [p1]
        aside `shouldMatchList` map toCardCode [SideSchemeCards.hallOfNastrond, MinionCards.nidhogg]
        (dealt1' - dealt1, dealt2' - dealt2) `shouldBe` (0, 1)

  it "Mystic: a side scheme reaching the victory display raises her hit points at once; +1 SCH and +1 ATK for each" do
    tg <- hela ["spider-man", "captain-marvel"]
    let p = player1 tg
    inGame tg do
      vid <- villainIdNow
      resolve [VillainMessage vid (VillainDamaged (toSource p) (toDamage 3 FromAbility))]
      defeatGnipahellir p
      v <- villainAttrs
      atk <- Villain.getModifiedAttack v
      sch <- Villain.getModifiedScheme v
      liftIO do
        (Villain.villainHp v, Villain.villainMaxHp v) `shouldBe` (HP 17, HP 20)
        (atk, sch) `shouldBe` (2, 2)

  it "with Odin rescued, defeating Hela wins the game" do
    tg <- hela ["spider-man"]
    let p = player1 tg
    inGame tg do
      _ <- rescueOdin p
      vid <- villainIdNow
      resolve [VillainMessage vid VillainWouldBeDefeated]
      finished `shouldReturn` Just Won

odinSpec :: Spec
odinSpec = describe "Odin, All-Father" do
  it "Hall of Nastrond defeated: the first player detaches Odin and takes control of him; each player is dealt an encounter card" do
    tg <- hela ["spider-man", "captain-marvel"]
    let (p1, p2) = (player1 tg, player2 tg)
    inGame tg do
      dealt1 <- length <$> dealtTo p1
      dealt2 <- length <$> dealtTo p2
      odin <- rescueOdin p1
      held <- heldOnScheme
      dealt1' <- length <$> dealtTo p1
      dealt2' <- length <$> dealtTo p2
      allies <- (.playerIdentityAllies) <$> playerAttrs p1
      liftIO do
        allyController odin `shouldBe` p1
        held `shouldBe` []
        allies `shouldSatisfy` member (allyId odin)
        (dealt1' - dealt1, dealt2' - dealt2) `shouldBe` (1, 1)

  it "Captive: if Odin leaves play, the players lose" do
    tg <- hela ["spider-man"]
    let p = player1 tg
    inGame tg do
      odin <- rescueOdin p
      resolve [AllyMessage (allyId odin) AllyDefeated]
      finished `shouldReturn` Just Lost

  it "the first player controls Odin: he follows the first player token, and goes to the next first player when the first is eliminated" do
    tg <- hela ["spider-man", "captain-marvel"]
    let (p1, p2) = (player1 tg, player2 tg)
    inGame tg do
      odin <- rescueOdin p1
      resolve [PassFirstPlayer]
      passed <- allyIn (toCardCode AllyCards.odinCaptive)
      allies2 <- (.playerIdentityAllies) <$> playerAttrs p2
      allies1 <- (.playerIdentityAllies) <$> playerAttrs p1
      liftIO do
        allyController passed `shouldBe` p2
        allies2 `shouldSatisfy` member (allyId odin)
        allies1 `shouldSatisfy` (not . member (allyId odin))
      resolve [IdentityMessage p2 IdentityDefeated]
      after <- allyIn (toCardCode AllyCards.odinCaptive)
      done <- finished
      liftIO do
        allyController after `shouldBe` p1
        done `shouldBe` Nothing

  it "flipped to his King side (RR Flip): the same ally, his damage, status cards and controller kept" do
    tg <- hela ["spider-man"]
    let p = player1 tg
    inGame tg do
      odin <- rescueOdin p
      resolve
        [ AllyMessage (allyId odin) (AllyDamaged (toSource p) (toDamage 2 FromAbility))
        , GiveStatus (AllyRef $ allyId odin) StunnedStatus
        , AllyMessage (allyId odin) FlipAlly
        ]
      kings <- alliesWith (toCardCode AllyCards.odinKing)
      captives <- alliesWith (toCardCode AllyCards.odinCaptive)
      liftIO do
        map allyId kings `shouldBe` [allyId odin]
        captives `shouldBe` []
        map (\k -> (allyDamage k, allyStunned k, allyController k)) kings `shouldBe` [(2, True, p)]

  it "King: leaving play, he is removed from the game instead; the players do not lose" do
    tg <- hela ["spider-man"]
    let p = player1 tg
    inGame tg do
      odin <- rescueOdin p
      resolve [AllyMessage (allyId odin) FlipAlly]
      resolve [AllyMessage (allyId odin) AllyDefeated]
      done <- finished
      discard <- discardOf p
      kings <- alliesWith (toCardCode AllyCards.odinKing)
      liftIO do
        done `shouldBe` Nothing
        kings `shouldBe` []
        map pcCardId discard `shouldSatisfy` notElem (CardId $ unAllyId $ allyId odin)

  it "he cannot have cards attached: Restrained passes him over for the hero, and cannot be attached to him" do
    tg <- hela ["spider-man"]
    let p = player1 tg
    inGame tg do
      asHero p
      odin <- rescueOdin p
      _ <- revealNow p AttachmentCards.restrained
      pass
      [r] <- attachmentsOf AttachmentCards.restrained
      resolve [AttachmentMessage (Attachment.attachmentId r) (AttachedToAlly $ allyId odin)]
      left <- attachmentsOf AttachmentCards.restrained
      gone <- inDiscard AttachmentCards.restrained
      liftIO do
        Attachment.attachmentIdentity r `shouldBe` Just p
        (length left, gone) `shouldBe` (0, True)

  it "he does not count against the ally limit, and no upgrade can be played on him" do
    tg <- hela ["captain-marvel"]
    let p = player1 tg
    inGame tg do
      odin <- rescueOdin p
      clearHand p
      inspired <- putInHand p "01074"
      _ <- putInHand p "01089"
      onlyOdin <- offers (isPlay inspired)
      for_ ["01066", "01067", "01068"] (putIntoPlay p)
      -- no ally is discarded for the limit: Odin is not counted
      allies <- length . (.playerIdentityAllies) <$> playerAttrs p
      maria <- allyIn "01067"
      choose "play Inspired" (isPlay inspired)
      onOdin <- offers (isTargetChoice (AllyRef $ allyId odin))
      onMaria <- offers (isTargetChoice (AllyRef $ allyId maria))
      liftIO do
        onlyOdin `shouldBe` False
        allies `shouldBe` 4
        (onOdin, onMaria) `shouldBe` (False, True)

sideSchemeSpec :: Spec
sideSchemeSpec = describe "Hela (side schemes)" do
  it "Garm in play: threat cannot be removed from Gnipahellir" do
    tg <- hela ["spider-man"]
    inGame tg do
      Just gnip <- sideSchemeIn SideSchemeCards.gnipahellir
      locked <- elem ThreatCannotBeRemoved <$> getModifiers (SideSchemeRef gnip)
      [garm] <- minionsOf MinionCards.garm
      resolve [RemoveFromPlay (MinionRef $ minionId garm)]
      unlocked <- notElem ThreatCannotBeRemoved <$> getModifiers (SideSchemeRef gnip)
      liftIO $ (locked, unlocked) `shouldBe` (True, True)

  it "Gjallerbru defeated: Hall of Nastrond and Nidhogg revealed, Nidhogg engaging the first player" do
    tg <- hela ["spider-man"]
    let p = player1 tg
    inGame tg do
      defeatGnipahellir p
      Just bridge <- sideSchemeIn SideSchemeCards.gjallerbru
      bridgeThreat <- sideSchemeThreat bridge
      defeatScheme p bridge
      hall <- sideSchemeIn SideSchemeCards.hallOfNastrond
      hallThreat <- maybe (pure 0) sideSchemeThreat hall
      nidhogg <- minionsOf MinionCards.nidhogg
      aside <- setAsideCodes
      liftIO do
        -- 3 threat and hinder 1 per player; 4 and the same
        (bridgeThreat, hallThreat) `shouldBe` (4, 5)
        map minionEngagedIdentity nidhogg `shouldBe` [p]
        aside `shouldBe` []

minionSpec :: Spec
minionSpec = describe "Hela (minions)" do
  it "Skurge engages the first player, revealed by another; he follows the first player token; 5 hit points per player, his attacks piercing" do
    tg <- hela ["spider-man", "captain-marvel"]
    let (p1, p2) = (player1 tg, player2 tg)
    inGame tg do
      m <- addMinion p2 MinionCards.skurge
      Just a <- minionAttrs m
      hp <- Minion.getModifiedHitPoints a
      keywords <- Minion.getModifiedKeywords a
      resolve [PassFirstPlayer]
      Just a' <- minionAttrs m
      liftIO do
        minionEngagedIdentity a `shouldBe` p1
        minionEngagedIdentity a' `shouldBe` p2
        hp `shouldBe` 10
        keywords `shouldSatisfy` elem Piercing

  it "Nidhogg's attacks gain overkill; Garm has 4 hit points per player" do
    tg <- hela ["spider-man", "captain-marvel"]
    let p = player1 tg
    inGame tg do
      m <- addMinion p MinionCards.nidhogg
      Just a <- minionAttrs m
      keywords <- Minion.getModifiedKeywords a
      [garm] <- minionsOf MinionCards.garm
      garmHp <- Minion.getModifiedHitPoints garm
      liftIO do
        keywords `shouldSatisfy` elem Overkill
        garmHp `shouldBe` 8

attachmentSpec :: Spec
attachmentSpec = describe "Hela (attachments)" do
  it "Nightsword attaches to Hela: +1 ATK, her attacks piercing" do
    tg <- hela ["spider-man"]
    let p = player1 tg
    inGame tg do
      _ <- revealNow p AttachmentCards.nightsword
      pass
      v <- villainAttrs
      atk <- Villain.getModifiedAttack v
      keywords <- Villain.getModifiedKeywords v
      liftIO do
        atk `shouldBe` 2
        keywords `shouldSatisfy` elem Piercing

  it "Nightsword as a boost card attaches to Hela" do
    tg <- hela ["spider-man"]
    let p = player1 tg
    inGame tg do
      asHero p
      _ <- stackEncounterDeck [AttachmentCards.nightsword]
      vid <- villainIdNow
      start [VillainMessage vid (VillainAttacks p)]
      settle
      swords <- attachmentsOf AttachmentCards.nightsword
      liftIO $ map Attachment.attachmentEnemy swords `shouldBe` [Just (EnemyVillainId vid)]

  it "Hela's Crown: +1 SCH; after Hela schemes, she is given a facedown boost card, kept for her next activation" do
    tg <- hela ["spider-man"]
    let p = player1 tg
    inGame tg do
      _ <- revealNow p AttachmentCards.helasCrown
      pass
      v <- villainAttrs
      sch <- Villain.getModifiedScheme v
      vid <- villainIdNow
      resolve [VillainMessage vid VillainSchemes]
      boosts <- Villain.villainBoostCards <$> villainAttrs
      liftIO do
        sch `shouldBe` 2
        length boosts `shouldBe` 1

  it "Hela's Cloak: Hela gains stalwart" do
    tg <- hela ["spider-man"]
    let p = player1 tg
    inGame tg do
      _ <- revealNow p AttachmentCards.helasCloak
      pass
      vid <- villainIdNow
      resolve [VillainMessage vid (VillainStunned (toSource p))]
      stunned <- Villain.villainStunned <$> villainAttrs
      liftIO $ stunned `shouldBe` False

treacherySpec :: Spec
treacherySpec = describe "Hela (treacheries)" do
  it "Hela's Domain: 1 threat on the main scheme, and 1 more for each side scheme in the victory display" do
    tg <- hela ["spider-man"]
    let p = player1 tg
    inGame tg do
      defeatGnipahellir p
      before <- mainSchemeThreatNow
      _ <- revealNow p TreacheryCards.helasDomain
      pass
      after <- mainSchemeThreatNow
      liftIO $ after - before `shouldBe` 2

  it "Hela's Domain as a boost: the attack's damage defeating an ally places 2 threat on the main scheme" do
    tg <- hela ["spider-man"]
    let p = player1 tg
    inGame tg do
      asHero p
      _ <- putIntoPlay p "01067"
      pass
      maria <- allyIn "01067"
      _ <- stackEncounterDeck [TreacheryCards.helasDomain]
      before <- mainSchemeThreatNow
      vid <- villainIdNow
      start [VillainMessage vid (VillainAttacks p)]
      chooseIf isNoResponse
      choose "Maria Hill defends" \case
        AllyDefend a _ -> a == allyId maria
        _ -> False
      settle
      after <- mainSchemeThreatNow
      gone <- alliesWith "01067"
      liftIO do
        gone `shouldBe` []
        after - before `shouldBe` 2

  it "The Queen of Hel, in alter-ego form: Hela schemes; then 1 threat on each side scheme" do
    tg <- hela ["spider-man"]
    let p = player1 tg
    inGame tg do
      Just gnip <- sideSchemeIn SideSchemeCards.gnipahellir
      before <- sideSchemeThreat gnip
      main <- mainSchemeThreatNow
      _ <- revealNow p TreacheryCards.theQueenOfHel
      settle
      after <- sideSchemeThreat gnip
      main' <- mainSchemeThreatNow
      liftIO do
        after - before `shouldBe` 1
        main' `shouldSatisfy` (> main)

  it "The Queen of Hel, in hero form: Hela attacks you; then 1 threat on each side scheme" do
    tg <- hela ["spider-man"]
    let p = player1 tg
    inGame tg do
      asHero p
      Just gnip <- sideSchemeIn SideSchemeCards.gnipahellir
      before <- sideSchemeThreat gnip
      dmg <- damageOf p
      _ <- revealNow p TreacheryCards.theQueenOfHel
      settle
      after <- sideSchemeThreat gnip
      dmg' <- damageOf p
      liftIO do
        after - before `shouldBe` 1
        dmg' `shouldSatisfy` (> dmg)

  it "The Wastes of Niffleheim: 1 indirect damage, and 1 more for each side scheme in the victory display" do
    tg <- hela ["spider-man"]
    let p = player1 tg
    inGame tg do
      sideSchemeToVictory
      dmg <- damageOf p
      _ <- revealNow p TreacheryCards.theWastesOfNiffleheim
      settle
      dmg' <- damageOf p
      liftIO $ dmg' - dmg `shouldBe` 2

  it "The Wastes of Niffleheim as a boost card: a boost icon for each side scheme in the victory display" do
    tg <- hela ["spider-man"]
    let p = player1 tg
    inGame tg do
      sideSchemeToVictory
      asHero p
      _ <- stackEncounterDeck [TreacheryCards.theWastesOfNiffleheim]
      dmg <- damageOf p
      vid <- villainIdNow
      start [VillainMessage vid (VillainAttacks p)]
      settle
      dmg' <- damageOf p
      -- her ATK 1, +1 for the side scheme in the victory display, and the boost icon
      -- the card gains for it (it prints none)
      liftIO $ dmg' - dmg `shouldBe` 3

legionsOfHelSpec :: Spec
legionsOfHelSpec = describe "Legions of Hel (modular set)" do
  it "Draugr: choose to take 1 damage" do
    tg <- hela ["spider-man"]
    let p = player1 tg
    inGame tg do
      _ <- revealNow p MinionCards.draugr
      choose "take 1 damage" (isLabel (countLabel "takeDamage" 1))
      pass
      damageOf p `shouldReturn` 1

  it "Draugr: or place 1 threat on the main scheme" do
    tg <- hela ["spider-man"]
    let p = player1 tg
    inGame tg do
      before <- mainSchemeThreatNow
      _ <- revealNow p MinionCards.draugr
      choose "place 1 threat" (isLabel (countLabel "placeThreatOnMainScheme" 1))
      pass
      after <- mainSchemeThreatNow
      dmg <- damageOf p
      liftIO $ (after - before, dmg) `shouldBe` (1, 0)

  it "Fallen Warrior: discards to an ally and puts it into play engaged with you, an Undead minion of the ally's printed stats, Fallen Warrior attached" do
    tg <- hela ["spider-man"]
    let p = player1 tg
    inGame tg do
      [energy, maria, _] <- setDeck p ["01088", "01067", "01089"]
      _ <- revealNow p AttachmentCards.fallenWarrior
      pass
      let mid = MinionId $ unCardId $ pcCardId maria
      Just m <- minionAttrs mid
      Just minion <- getsGame (HashMap.lookup mid . gameMinions)
      traits <- getTraits minion
      [fw] <- attachmentsOf AttachmentCards.fallenWarrior
      discard <- discardOf p
      liftIO do
        minionEngagedIdentity m `shouldBe` p
        (minionScheme m, minionAttack m, minionHitPoints m) `shouldBe` (Sch 2, Atk 1, HP 2)
        traits `shouldSatisfy` member Undead
        traits `shouldSatisfy` member Shield
        Attachment.attachmentEnemy fw `shouldBe` Just (EnemyMinionId mid)
        discard `shouldSatisfy` elem energy
        discard `shouldSatisfy` notElem maria

  it "Fallen Warrior's minion defeated: the ally card goes to its owner's discard pile, Fallen Warrior to the encounter discard pile" do
    tg <- hela ["spider-man"]
    let p = player1 tg
    inGame tg do
      [_, maria, _] <- setDeck p ["01088", "01067", "01089"]
      _ <- revealNow p AttachmentCards.fallenWarrior
      pass
      let mid = MinionId $ unCardId $ pcCardId maria
      resolve [MinionMessage mid MinionDefeated]
      discard <- discardOf p
      fw <- inDiscard AttachmentCards.fallenWarrior
      ally <- alliesWith "01067"
      liftIO do
        map pcCardId discard `shouldSatisfy` elem (pcCardId maria)
        fw `shouldBe` True
        ally `shouldBe` []

  it "Fallen Warrior leaving play first: the minion is its owner's ally again, its damage kept" do
    tg <- hela ["spider-man"]
    let p = player1 tg
    inGame tg do
      [_, maria, _] <- setDeck p ["01088", "01067", "01089"]
      _ <- revealNow p AttachmentCards.fallenWarrior
      pass
      let mid = MinionId $ unCardId $ pcCardId maria
      resolve [MinionMessage mid (MinionDamaged (toSource p) (toDamage 1 FromAbility))]
      [fw] <- attachmentsOf AttachmentCards.fallenWarrior
      resolve [RemoveFromPlay (AttachmentRef $ Attachment.attachmentId fw)]
      ally <- allyIn "01067"
      gone <- minionAttrs mid
      liftIO do
        (allyController ally, allyDamage ally) `shouldBe` (p, 1)
        isNothing gone `shouldBe` True

  it "Fallen Warrior with no ally in the deck: it is discarded, nothing attached" do
    tg <- hela ["spider-man"]
    let p = player1 tg
    inGame tg do
      _ <- setDeck p ["01088", "01089"]
      minions <- getsGame (HashMap.size . gameMinions)
      _ <- revealNow p AttachmentCards.fallenWarrior
      settle
      minions' <- getsGame (HashMap.size . gameMinions)
      fw <- inDiscard AttachmentCards.fallenWarrior
      liftIO $ (minions' - minions, fw) `shouldBe` (0, True)

  it "Fallen Warrior's ally matching a unique card in play: it cannot be put into play; Fallen Warrior is discarded" do
    tg <- hela ["captain-marvel"]
    let p = player1 tg
    inGame tg do
      _ <- putIntoPlay p "01067"
      pass
      [_, maria, _] <- setDeck p ["01088", "01067", "01089"]
      _ <- revealNow p AttachmentCards.fallenWarrior
      settle
      minion <- minionAttrs (MinionId $ unCardId $ pcCardId maria)
      fw <- inDiscard AttachmentCards.fallenWarrior
      discard <- discardOf p
      liftIO do
        isNothing minion `shouldBe` True
        fw `shouldBe` True
        map pcCardId discard `shouldSatisfy` elem (pcCardId maria)

  it "Legions of Hel: 2 more threat for each Undead minion in play" do
    tg <- hela ["spider-man"]
    let p = player1 tg
    inGame tg do
      _ <- revealNow p MinionCards.draugr
      chooseIf (isLabel (countLabel "takeDamage" 1))
      pass
      scheme <- revealNow p SideSchemeCards.legionsOfHel
      pass
      sideSchemeThreat (sideSchemeOf scheme) `shouldReturn` 4

  it "Legions of Hel with no Undead minion in play: it gains surge" do
    tg <- hela ["spider-man"]
    let p = player1 tg
    inGame tg do
      [merc] <- stackEncounterDeck [MinionCards.hydraMercenary]
      scheme <- revealNow p SideSchemeCards.legionsOfHel
      settle
      threat <- sideSchemeThreat (sideSchemeOf scheme)
      surged <- elem merc <$> dealtTo p
      liftIO $ (threat, surged) `shouldBe` (2, True)

  it "No Place for the Living: discard the highest-cost support or upgrade you control" do
    tg <- hela ["captain-marvel"]
    let p = player1 tg
    inGame tg do
      tac <- putIntoPlay p "01056"
      surveillance <- putInPlaySupport p "01064"
      _ <- revealNow p TreacheryCards.noPlaceForTheLiving
      damageOffered <- offers (isLabel (countLabel "takeDamage" 2))
      lowerOffered <- offers (isTargetChoice (SupportRef $ supportIdOf surveillance))
      choose "discard Tac Team" (isTargetChoice (SupportRef $ supportIdOf tac))
      settle
      supports <- getsGame (HashMap.keys . gameSupports)
      discard <- discardOf p
      liftIO do
        (damageOffered, lowerOffered) `shouldBe` (True, False)
        supports `shouldBe` [supportIdOf surveillance]
        map pcCardId discard `shouldSatisfy` elem (pcCardId tac)

  it "No Place for the Living: or take damage equal to the upgrades and supports you control" do
    tg <- hela ["captain-marvel"]
    let p = player1 tg
    inGame tg do
      _ <- putIntoPlay p "01056"
      _ <- putIntoPlay p "01064"
      pass
      dmg <- damageOf p
      _ <- revealNow p TreacheryCards.noPlaceForTheLiving
      choose "take 2 damage" (isLabel (countLabel "takeDamage" 2))
      settle
      dmg' <- damageOf p
      supports <- getsGame (HashMap.size . gameSupports)
      liftIO $ (dmg' - dmg, supports) `shouldBe` (2, 2)

  it "No Place for the Living with nothing controlled: 0 damage, nothing to discard" do
    tg <- hela ["spider-man"]
    let p = player1 tg
    inGame tg do
      dmg <- damageOf p
      _ <- revealNow p TreacheryCards.noPlaceForTheLiving
      settle
      dmg' <- damageOf p
      liftIO $ dmg' `shouldBe` dmg
 where
  putInPlaySupport p code = putIntoPlay p code <* pass

frostGiantsSpec :: Spec
frostGiantsSpec = describe "Frost Giants (modular set)" do
  it "Laufey: after he attacks and damages a character, that character is stunned" do
    tg <- hela ["spider-man"]
    let p = player1 tg
    inGame tg do
      asHero p
      m <- addMinion p MinionCards.laufey
      start [MinionMessage m (MinionAttacks p)]
      settle
      dmg <- damageOf p
      stunned <- isStunned p
      liftIO $ (dmg, stunned) `shouldBe` (4, True)

  it "Frost Giant (21157) is Thor's nemesis set's card: toughness" do
    tg <- hela ["spider-man"]
    let p = player1 tg
    inGame tg do
      m <- addMinion p MinionCards.frostGiantFrostGiants
      Just a <- minionAttrs m
      liftIO do
        toCardCode (minionCardDef a) `shouldBe` "21157"
        minionTough a `shouldBe` True

  it "Frozen: attached identity cannot ready; Alter-Ego Action: spend [energy] [physical] to discard it" do
    tg <- hela ["spider-man"]
    let p = player1 tg
    inGame tg do
      _ <- revealNow p AttachmentCards.frozen
      pass
      [f] <- attachmentsOf AttachmentCards.frozen
      resolve [IdentityMessage p ExhaustedIdentity, IdentityMessage p ReadyCards]
      stillExhausted <- isExhaustedNow p
      clearHand p
      energy <- putInHand p "01088"
      strength <- putInHand p "01090"
      choose "discard Frozen" (isAbilityOf (AttachmentRef $ Attachment.attachmentId f))
      payWith [energy, strength]
      pass
      gone <- inDiscard AttachmentCards.frozen
      liftIO do
        Attachment.attachmentIdentity f `shouldBe` Just p
        stillExhausted `shouldBe` True
        gone `shouldBe` True

  it "Unnatural Storm keeps a player card from readying a Frozen in Time hero: the attachment stays; the ready phase discards it instead" do
    tg <- hela ["spider-man"]
    let p = player1 tg
    inGame tg do
      asHero p
      _ <- revealNow p SideSchemeCards.unnaturalStorm
      pass
      _ <- revealNow p AttachmentCards.frozenInTime
      pass
      resolve [IdentityMessage p ExhaustedIdentity, IdentityMessage p ReadiedIdentity]
      stays <- length <$> attachmentsOf AttachmentCards.frozenInTime
      resolve [IdentityMessage p ReadyCards]
      gone <- inDiscard AttachmentCards.frozenInTime
      stillExhausted <- isExhaustedNow p
      liftIO do
        stays `shouldBe` 1
        (gone, stillExhausted) `shouldBe` (True, True)

  it "Unnatural Storm: each ally in play is exhausted; heroes and allies are not readied by player cards, but are by the ready phase" do
    tg <- hela ["spider-man"]
    let p = player1 tg
    inGame tg do
      asHero p
      _ <- putIntoPlay p "01067"
      pass
      maria <- allyIn "01067"
      _ <- revealNow p SideSchemeCards.unnaturalStorm
      pass
      exhaustedAlly <- allyExhausted <$> allyIn "01067"
      resolve [IdentityMessage p ExhaustedIdentity, IdentityMessage p ReadiedIdentity, AllyMessage (allyId maria) ReadiedAlly]
      heroStays <- isExhaustedNow p
      allyStays <- allyExhausted <$> allyIn "01067"
      resolve [IdentityMessage p ReadyCards]
      heroReady <- not <$> isExhaustedNow p
      allyReady <- not . allyExhausted <$> allyIn "01067"
      liftIO do
        exhaustedAlly `shouldBe` True
        (heroStays, allyStays) `shouldBe` (True, True)
        (heroReady, allyReady) `shouldBe` (True, True)
