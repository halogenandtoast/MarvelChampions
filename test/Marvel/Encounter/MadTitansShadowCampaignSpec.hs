-- | The Mad Titan's Shadow's campaign set (21180-21193), each card played as a table
-- would.
module Marvel.Encounter.MadTitansShadowCampaignSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Ally.Cards qualified as AllyCards
import Marvel.Attachment.Cards qualified as AttachmentCards
import Marvel.Attachment.Types (Attachment)
import Marvel.Attachment.Types qualified as Attachment
import Marvel.Hp (HP (..))
import Marvel.Matchers
import Marvel.Minion.Cards qualified as MinionCards
import Marvel.Modifier (Modifier (..))
import Marvel.Phase (Phase (PlayerPhase))
import Marvel.Query
import Marvel.SideScheme.Cards qualified as SideSchemeCards
import Marvel.SideScheme.Types qualified as SideScheme
import Marvel.Status (StatusCard (..))
import Marvel.Treachery.Cards qualified as TreacheryCards
import Marvel.Villain.Types qualified as Villain
import Marvel.Window qualified as W
import TestImport

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

sideSchemeIn :: CardDef -> GameAppT (Maybe SideSchemeId)
sideSchemeIn def = getsGame (headMay . map fst . filter ((== toCardCode def) . toCardCode . getCardDef . snd) . HashMap.toList . gameSideSchemes)

sideSchemeCodes :: GameAppT [CardCode]
sideSchemeCodes = map fst <$> getSideSchemesInPlay

countersOn :: SideSchemeId -> GameAppT Natural
countersOn sid = getsGame (maybe 0 (SideScheme.sideSchemeCounters . toAttrs) . HashMap.lookup sid . gameSideSchemes)

victoryCodes :: GameAppT [CardCode]
victoryCodes = map toCardCode <$> getVictoryDisplay

attachmentsOf :: CardDef -> GameAppT [Attrs Attachment]
attachmentsOf def = getsGame (filter ((== toCardCode def) . toCardCode . Attachment.attachmentCardDef) . map toAttrs . HashMap.elems . gameAttachments)

-- | All the threat on it removed by the player's thwart, answering what that asks with
-- its first choice.
thwartAll :: IdentityId -> SideSchemeId -> GameAppT ()
thwartAll p sid = do
  n <- sideSchemeThreat sid
  resolve [SideSchemeMessage sid (SideSchemeThwarted (IdentityRef p) n)]

-- | The side scheme defeated, as the last threat removed from it defeats it, whatever
-- keeps threat on it.
defeatScheme :: IdentityId -> SideSchemeId -> GameAppT ()
defeatScheme p sid =
  resolve
    [ CheckWindows [W.Window W.When (W.DefeatedSideScheme sid)]
    , SideSchemeMessage sid DefeatSideScheme
    , DefeatedLeavesPlay (SideSchemeRef sid)
    , CheckWindows [W.Window W.After (W.DefeatedSideSchemeBy sid p Nothing)]
    ]

-- | Rhino attacks the player, his boost card adding nothing.
rhinoAttacks :: IdentityId -> GameAppT ()
rhinoAttacks p = do
  _ <- stackEncounterDeck [TreacheryCards.imTough]
  vid <- villainIdNow
  runNow [VillainMessage vid (VillainAttacks p)]

isAllyDefend :: AllyId -> Choice -> Bool
isAllyDefend a = \case
  AllyDefend a' _ -> a' == a
  _ -> False

villainHpNow :: GameAppT Int
villainHpNow = unHp . Villain.villainHp . toAttrs <$> getVillain

-- | Hela's damage as much as she has hit points.
damageHela :: IdentityId -> GameAppT ()
damageHela p = do
  v <- toAttrs <$> getVillain
  resolve [VillainMessage (Villain.villainId v) (VillainDamaged (toSource p) (toDamage (fromIntegral $ unHp $ Villain.villainHp v) FromAbility))]

-- | Hall of Nastrond revealed and defeated: Odin rescued, under the first player.
rescueOdin :: IdentityId -> GameAppT (Attrs Ally)
rescueOdin p = do
  hall <- revealNow p SideSchemeCards.hallOfNastrond
  pass
  defeatScheme p (sideSchemeOf hall)
  allyIn (toCardCode AllyCards.odinCaptive)

-- | The Captive allies of the campaign set, set aside as Loki's campaign setup has them.
setAsideCaptives :: GameAppT [PlayerCard]
setAsideCaptives = do
  cards <- for ["21190", "21191", "21192", "21193"] \code -> do
    cid <- getRandom
    pure $ MkPlayerCard cid (lookupPlayerCard code) Nothing Nothing
  resolve [SetAside (map PlayerCard cards)]
  pure cards

isCardChoice :: CardCode -> Choice -> Bool
isCardChoice code = \case
  CardLabel c _ -> c == code
  _ -> False

spec :: Spec
spec = describe "The Mad Titan's Shadow campaign set" do
  describe "Secure the Landing Pad (21180a) and Cosmo (21180b)" do
    it "hinder 1 per player; defeated, it flips to Cosmo under the first player's control, not to the victory display" do
      tg <- testGame ["spider-man", "captain-marvel"]
      let (p1, p2) = (player1 tg, player2 tg)
      inGame tg do
        card <- revealNow p2 SideSchemeCards.secureTheLandingPad
        let sid = sideSchemeOf card
        sideSchemeThreat sid `shouldReturn` 4
        thwartAll p2 sid
        sideSchemeCodes >>= (liftIO . (`shouldNotContain` ["21180a"]))
        victoryCodes >>= (liftIO . (`shouldNotContain` ["21180a"]))
        cosmo <- allyIn "21180b"
        liftIO $ allyController cosmo `shouldBe` p1

    it "Cosmo follows the first player and does not count against the ally limit" do
      tg <- testGame ["spider-man", "captain-marvel"]
      let (p1, p2) = (player1 tg, player2 tg)
      inGame tg do
        for_ ["01059", "01076", "01058"] (putIntoPlay p1)
        card <- revealNow p1 SideSchemeCards.secureTheLandingPad
        thwartAll p1 (sideSchemeOf card)
        mine <- (.playerIdentityAllies) <$> playerAttrs p1
        liftIO $ length mine `shouldBe` 4
        resolve [PassFirstPlayer]
        cosmo <- allyIn "21180b"
        theirs <- (.playerIdentityAllies) <$> playerAttrs p2
        liftIO do
          allyController cosmo `shouldBe` p2
          theirs `shouldSatisfy` member (allyId cosmo)

    it "Cosmo leaving play is removed from the game, in no discard pile and no hand" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        card <- revealNow p SideSchemeCards.secureTheLandingPad
        thwartAll p (sideSchemeOf card)
        cosmo <- allyIn "21180b"
        resolve [AllyMessage (allyId cosmo) AllyDefeated]
        alliesWith "21180b" `shouldReturn` []
        discard <- map toCardCode <$> discardOf p
        hand <- map toCardCode <$> handOf p
        liftIO do
          discard `shouldNotContain` ["21180b"]
          hand `shouldNotContain` ["21180b"]

    it "Cosmo returned to hand is removed from the game instead" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        card <- revealNow p SideSchemeCards.secureTheLandingPad
        thwartAll p (sideSchemeOf card)
        cosmo <- allyIn "21180b"
        resolve [ReturnToHand (AllyRef $ allyId cosmo)]
        alliesWith "21180b" `shouldReturn` []
        hand <- map toCardCode <$> handOf p
        discard <- map toCardCode <$> discardOf p
        liftIO do
          hand `shouldNotContain` ["21180b"]
          discard `shouldNotContain` ["21180b"]

  describe "Security Breach (21181)" do
    it "when revealed each player places a random card from hand here; defeated, each returns to its owner's hand and it goes to the victory display" do
      tg <- testGame ["spider-man", "captain-marvel"]
      let (p1, p2) = (player1 tg, player2 tg)
      inGame tg do
        hand1 <- length <$> handOf p1
        hand2 <- length <$> handOf p2
        card <- revealNow p1 SideSchemeCards.securityBreach
        let sid = sideSchemeOf card
        -- 2 and hinder 2 per player
        sideSchemeThreat sid `shouldReturn` 6
        held <- getsGame (maybe [] (SideScheme.sideSchemeHeldCards . toAttrs) . HashMap.lookup sid . gameSideSchemes)
        (length <$> handOf p1) `shouldReturn` hand1 - 1
        (length <$> handOf p2) `shouldReturn` hand2 - 1
        liftIO $ length held `shouldBe` 2
        thwartAll p1 sid
        (length <$> handOf p1) `shouldReturn` hand1
        (length <$> handOf p2) `shouldReturn` hand2
        victoryCodes `shouldReturn` ["21181"]

  describe "Save the Shawarma Place (21182a), Black Swan (21182b) and Shawarma (21183)" do
    it "defeated: each player shuffles a Shawarma into their deck; it flips to Black Swan, engaged with the first player, tough, who has them discard a card" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        card <- revealNow p SideSchemeCards.saveTheShawarmaPlace
        let sid = sideSchemeOf card
        sideSchemeThreat sid `shouldReturn` 3
        hand <- length <$> handOf p
        thwartAll p sid
        deck <- map toCardCode . unDeck . (.playerIdentityDeck) <$> playerAttrs p
        swans <- getsGame (filter ((== "21182b") . toCardCode . minionCardDef) . map toAttrs . HashMap.elems . gameMinions)
        (length <$> handOf p) `shouldReturn` hand - 1
        sideSchemeCodes >>= (liftIO . (`shouldNotContain` ["21182a"]))
        liftIO do
          deck `shouldContain` ["21183"]
          map minionEngagedIdentity swans `shouldBe` [p]
          map minionTough swans `shouldBe` [True]

    it "Black Swan engages the first player: the token passed, she engages the next, who discards a card" do
      tg <- testGame ["spider-man", "captain-marvel"]
      let (p1, p2) = (player1 tg, player2 tg)
      inGame tg do
        swan <- addMinion p1 MinionCards.blackSwan
        hand2 <- length <$> handOf p2
        resolve [PassFirstPlayer]
        engaged <- getsGame (fmap (minionEngagedIdentity . toAttrs) . HashMap.lookup swan . gameMinions)
        liftIO $ engaged `shouldBe` Just p2
        (length <$> handOf p2) `shouldReturn` hand2 - 1

    it "Black Swan, Victory 2: defeated, she goes to the victory display" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        swan <- addMinion p MinionCards.blackSwan
        resolve [MinionMessage swan MinionDefeated]
        victoryCodes `shouldReturn` ["21182b"]

    it "Shawarma pays [energy][physical][mental]: alone, for a card costing 3" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        clearHand p
        helicarrier <- putInHand p "01092"
        shawarma <- putInHand p "21183"
        playWith helicarrier [shawarma]
        supportsWith "01092" >>= (liftIO . (`shouldSatisfy` ((== 1) . length)))
        map toCardCode <$> discardOf p >>= (liftIO . (`shouldContain` ["21183"]))

  describe "Hack Sanctuary's Computer (21184a) and Defensive Protocols (21184b)" do
    it "defeated: each player adds a card of their deck or discard pile to their hand; it flips to Defensive Protocols with its threat and hinder" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        card <- revealNow p SideSchemeCards.hackSanctuarysComputer
        let sid = sideSchemeOf card
        sideSchemeThreat sid `shouldReturn` 3
        hand <- length <$> handOf p
        thwartAll p sid
        (length <$> handOf p) `shouldReturn` hand + 1
        protocols <- sideSchemeIn SideSchemeCards.defensiveProtocols
        case protocols of
          Just s -> sideSchemeThreat s `shouldReturn` 4
          Nothing -> liftIO $ expectationFailure "no Defensive Protocols"
        sideSchemeIn SideSchemeCards.hackSanctuarysComputer `shouldReturn` Nothing

    it "Defensive Protocols: a crash counter as each player phase ends; at 2, each player adds System Shock to their hand and it is removed from the game" do
      tg <- testGame ["spider-man", "captain-marvel"]
      let (p1, p2) = (player1 tg, player2 tg)
      inGame tg do
        card <- revealNow p1 SideSchemeCards.defensiveProtocols
        let sid = sideSchemeOf card
        sideSchemeThreat sid `shouldReturn` 6
        -- its Forced Interrupt resolves first, before the player phase's end asks anything
        runNow [EndPhase PlayerPhase]
        countersOn sid `shouldReturn` 1
        hand1 <- map toCardCode <$> handOf p1
        liftIO $ hand1 `shouldNotContain` ["21185"]
        runNow [EndPhase PlayerPhase]
        sideSchemeIn SideSchemeCards.defensiveProtocols `shouldReturn` Nothing
        victoryCodes `shouldReturn` []
        for_ [p1, p2] \p -> map toCardCode <$> handOf p >>= (liftIO . (`shouldContain` ["21185"]))

    it "Defensive Protocols, Victory 2: defeated first, it goes to the victory display" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        card <- revealNow p SideSchemeCards.defensiveProtocols
        thwartAll p (sideSchemeOf card)
        victoryCodes `shouldReturn` ["21184b"]

  describe "System Shock (21185)" do
    it "in hand, its Alter-Ego Action spends a [mental] resource to remove it from the game" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        shock <- putInHand p "21185"
        genius <- putInHand p "01089"
        choose "System Shock" (isAbilityOf shock)
        payWith [genius]
        hand <- map pcCardId <$> handOf p
        discard <- map pcCardId <$> discardOf p
        liftIO do
          hand `shouldNotContain` [pcCardId shock]
          discard `shouldNotContain` [pcCardId shock]

    it "is never offered as a card to play, in either form" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        shock <- putInHand p "21185"
        _ <- putInHand p "01089"
        offers (isPlay shock) `shouldReturn` False
        asHero p
        offers (isPlay shock) `shouldReturn` False

    it "is not offered in hero form" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        asHero p
        shock <- putInHand p "21185"
        _ <- putInHand p "01089"
        offers (isAbilityOf shock) `shouldReturn` False

    it "drawn, it stays in hand rather than going into play" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        before <- obligationsInPlay
        [shock] <- stackDeck p ["21185"]
        resolve [IdentityMessage p (DrawCards FromDeck 1)]
        map pcCardId <$> handOf p >>= (liftIO . (`shouldContain` [pcCardId shock]))
        obligationsInPlay `shouldReturn` before

    it "cannot be chosen to discard: not to hand size, nor for a card's discard" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        shock <- putInHand p "21185"
        other <- putInHand p "01089"
        for_ [1 .. 6 :: Int] \_ -> putInHand p "01090"
        runNow [IdentityMessage p DrawOrDiscardToHandLimit]
        offers (isTargetChoice other) `shouldReturn` True
        offers (isTargetChoice shock) `shouldReturn` False
      tg' <- testGame ["spider-man"]
      let p' = player1 tg'
      inGame tg' do
        shock <- putInHand p' "21185"
        other <- putInHand p' "01089"
        runNow [IdentityMessage p' (DiscardFrom FromHand 1 Nothing)]
        offers (isTargetChoice other) `shouldReturn` True
        offers (isTargetChoice shock) `shouldReturn` False

  describe "Find the Norn Stones (21186a), Retrieve Odin's Armor (21186b) and Norn Stone (21187)" do
    it "Find the Norn Stones: threat cannot be removed unless Hela has the Wounded trait" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        card <- revealNow p SideSchemeCards.findTheNornStones
        let sid = sideSchemeOf card
        sideSchemeThreat sid `shouldReturn` 3
        resolve [SideSchemeMessage sid (SideSchemeThwarted (IdentityRef p) 1)]
        sideSchemeThreat sid `shouldReturn` 3
      tg' <- testGameAgainst "21136a" ["spider-man"]
      let p' = player1 tg'
      inGame tg' do
        card <- revealNow p' SideSchemeCards.findTheNornStones
        let sid = sideSchemeOf card
        -- Odin attached to the main scheme: Hela flips to Wounded instead of being defeated
        damageHela p'
        resolve [SideSchemeMessage sid (SideSchemeThwarted (IdentityRef p') 1)]
        sideSchemeThreat sid `shouldReturn` 2

    it "Find the Norn Stones defeated: each player puts a Norn Stone into play on its Setup side; it flips to Retrieve Odin's Armor" do
      tg <- testGame ["spider-man", "captain-marvel"]
      let (p1, p2) = (player1 tg, player2 tg)
      inGame tg do
        card <- revealNow p1 SideSchemeCards.findTheNornStones
        defeatScheme p1 (sideSchemeOf card)
        stones <- upgradesWith "21187a"
        liftIO $ map upgradeController stones `shouldMatchList` [p1, p2]
        armor <- sideSchemeIn SideSchemeCards.retrieveOdinsArmor
        case armor of
          Just s -> sideSchemeThreat s `shouldReturn` 6
          Nothing -> liftIO $ expectationFailure "no Retrieve Odin's Armor"

    it "Retrieve Odin's Armor: threat cannot be removed until the first player controls Odin; defeated, Odin is healed and flips to King" do
      tg <- testGameAgainst "21136a" ["spider-man"]
      let p = player1 tg
      inGame tg do
        card <- revealNow p SideSchemeCards.retrieveOdinsArmor
        let sid = sideSchemeOf card
        before <- sideSchemeThreat sid
        resolve [SideSchemeMessage sid (SideSchemeThwarted (IdentityRef p) 1)]
        sideSchemeThreat sid `shouldReturn` before
        odin <- rescueOdin p
        resolve [AllyMessage (allyId odin) (AllyDamaged (toSource p) (toDamage 2 FromAbility))]
        resolve [SideSchemeMessage sid (SideSchemeThwarted (IdentityRef p) 1)]
        sideSchemeThreat sid `shouldReturn` before - 1
        thwartAll p sid
        kings <- alliesWith (toCardCode AllyCards.odinKing)
        liftIO $ map (\k -> (allyId k, allyDamage k)) kings `shouldBe` [(allyId odin, 0)]
        victoryCodes >>= (liftIO . (`shouldContain` ["21186b"]))

    it "Norn Stone: +1 THW, ATK and DEF; its Setup side's Hero Action readies the hero and flips it" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        asHero p
        stone <- putIntoPlay p "21187a"
        modifiers <- getModifiers (IdentityRef p)
        liftIO $ [ThwartModifier 1, AttackModifier 1, DefenseModifier 1] `shouldSatisfy` all (`elem` modifiers)
        resolve [IdentityMessage p TestImport.ExhaustedIdentity]
        choose "Norn Stone" (isAbilityOfCard stone)
        exhaustedNow p `shouldReturn` False
        upgradeCodes >>= (liftIO . (`shouldContain` ["21187b"]))
        upgradeCodes >>= (liftIO . (`shouldNotContain` ["21187a"]))

    it "Norn Stone's other side: Alter-Ego Action, exhaust it to heal 1 damage from your identity" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        stone <- putIntoPlay p "21187b"
        setDamage p 2
        choose "Norn Stone" (isAbilityOfCard stone)
        damageOf p `shouldReturn` 1
        upgradeIsExhausted (upgradeIdOf stone) `shouldReturn` True
        asHero p
        offers (isAbilityOfCard stone) `shouldReturn` False

  describe "Summoned Back (21188)" do
    it "puts your nemesis minion into play engaged with you from the set-aside area, then surges" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        nemesis <- getNemesisMinionsOutOfPlay p
        liftIO $ nemesis `shouldSatisfy` (not . null)
        -- the deck shuffled, the surge reveals the one card left
        _ <- setEncounterDeck [TreacheryCards.summonedBack, TreacheryCards.imTough] []
        resolve [DrawAndRevealEncounterCard p]
        getNemesisMinionsOutOfPlay p `shouldReturn` drop 1 nemesis
        engaged <- selectList (MinionEngagedWith (IdentityWithId p))
        liftIO $ length engaged `shouldBe` 1
        -- surge: I'm Tough, the one card left, revealed after it
        encounterDeckSize `shouldReturn` 0
        encounterDiscardCodes >>= (liftIO . (`shouldContain` ["01105"]))

  describe "Open the Dungeons (21189a), Jormungand (21189b) and the Captive allies" do
    it "defeated: each player in turn chooses a set-aside Captive ally into play under their control; it flips to Jormungand on Loki, +4 per player hit points" do
      tg <- testGameAgainst "21160" ["spider-man", "captain-marvel"]
      let (p1, p2) = (player1 tg, player2 tg)
      inGame tg do
        _ <- setAsideCaptives
        card <- revealNow p1 SideSchemeCards.openTheDungeons
        let sid = sideSchemeOf card
        sideSchemeThreat sid `shouldReturn` 3
        hp <- villainHpNow
        runNow [SideSchemeMessage sid (SideSchemeThwarted (IdentityRef p1) 3)]
        choose "Fandral" (isCardChoice "21191")
        offers (isCardChoice "21191") `shouldReturn` False
        choose "Hogun" (isCardChoice "21192")
        settle
        fandral <- allyIn "21191"
        hogun <- allyIn "21192"
        liftIO $ (allyController fandral, allyController hogun) `shouldBe` (p1, p2)
        jormungand <- attachmentsOf AttachmentCards.jormungand
        vid <- villainIdNow
        liftIO $ map Attachment.attachmentEnemy jormungand `shouldBe` [Just (EnemyVillainId vid)]
        villainHpNow `shouldReturn` hp + 8

    it "Jormungand: Loki defeated, it is removed from the game, and the next Loki does not keep it" do
      tg <- testGameAgainst "21160" ["spider-man"]
      let p = player1 tg
      inGame tg do
        resolve [CardSpecific GameRef "lokiVictoryCondition" (toJSON (2 :: Natural))]
        card <- revealNow p SideSchemeCards.openTheDungeons
        thwartAll p (sideSchemeOf card)
        attachmentsOf AttachmentCards.jormungand >>= (liftIO . (`shouldSatisfy` ((== 1) . length)))
        vid <- villainIdNow
        resolve [VillainMessage vid VillainDefeated]
        attachmentsOf AttachmentCards.jormungand `shouldReturn` []
        victoryCodes >>= (liftIO . (`shouldNotContain` ["21189b"]))

    it "Lady Sif: Action, spend a [physical] resource to ready her" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        sif <- putIntoPlay p "21190"
        resolve [AllyMessage (allyIdOf sif) TestImport.ExhaustedAlly]
        strength <- putInHand p "01090"
        choose "Lady Sif" (isAbilityOfCard sif)
        payWith [strength]
        (.allyExhausted) <$> allyIn "21190" `shouldReturn` False

    it "Fandral's basic thwart ignores crisis icons; another ally's does not" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        placeMainThreat 2
        m <- mainSchemeIdNow
        _ <- revealNow p SideSchemeCards.crowdControl
        _ <- putIntoPlay p "21191"
        _ <- putIntoPlay p "21192"
        fandral <- allyIn "21191"
        hogun <- allyIn "21192"
        choose "Hogun thwarts" (isAllyThwart hogun)
        offers (isTargetChoice m) `shouldReturn` False
        settle
        choose "Fandral thwarts" (isAllyThwart fandral)
        offers (isTargetChoice m) `shouldReturn` True

    it "Hogun's attacks gain piercing" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        vid <- villainIdNow
        resolve [GiveStatus (VillainRef vid) ToughStatus]
        _ <- putIntoPlay p "21192"
        hogun <- allyIn "21192"
        choose "Hogun attacks" (isAllyAttack hogun)
        chooseIf (isTargetChoice vid)
        settle
        Villain.villainIsTough <$> getVillain `shouldReturn` False
        villainDamageNow `shouldReturn` 3

    it "Volstagg: toughness, and retaliate 1 as he is attacked" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        asHero p
        volstagg <- putIntoPlay p "21193"
        (.allyTough) <$> allyIn "21193" `shouldReturn` True
        rhinoAttacks p
        chooseIf isNoResponse
        choose "Volstagg defends" (isAllyDefend (allyIdOf volstagg))
        settle
        villainDamageNow `shouldReturn` 1
