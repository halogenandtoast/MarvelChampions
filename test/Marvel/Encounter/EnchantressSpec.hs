-- | The Enchantress modular set (The Mad Titan's Shadow), each card played as a table
-- would.
module Marvel.Encounter.EnchantressSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Ally.Cards qualified as AllyCards
import Marvel.Attachment.Cards qualified as AttachmentCards
import Marvel.Attachment.Types (Attachment)
import Marvel.Attachment.Types qualified as Attachment
import Marvel.Hp (HP (..))
import Marvel.Minion.Cards qualified as MinionCards
import Marvel.Minion.Types qualified as Minion
import Marvel.SideScheme.Cards qualified as SideSchemeCards
import Marvel.Stats (Atk (..), Sch (..))
import Marvel.Trait (HasTraits (..), Trait (Android, Enthralled))
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

attachmentsOf :: CardDef -> GameAppT [Attrs Attachment]
attachmentsOf def = getsGame (filter ((== toCardCode def) . toCardCode . Attachment.attachmentCardDef) . map toAttrs . HashMap.elems . gameAttachments)

inDiscard :: CardDef -> GameAppT Bool
inDiscard def = elem (toCardCode def) <$> encounterDiscardCodes

minionAttrs :: MinionId -> GameAppT (Maybe (Attrs Minion))
minionAttrs m = getsGame (fmap toAttrs . HashMap.lookup m . gameMinions)

-- | The minion the ally is treated as: the same card.
asMinion :: Attrs Ally -> MinionId
asMinion = MinionId . unAllyId . allyId

-- | Seduced attached to the player's identity.
seducedOn :: IdentityId -> GameAppT Bool
seducedOn p = any ((== Just p) . Attachment.attachmentIdentity) <$> attachmentsOf AttachmentCards.seduced

spec :: Spec
spec = do
  enchantressSpec
  seducedSpec
  beguiledSpec

enchantressSpec :: Spec
enchantressSpec = describe "Enchantress (21177)" do
  it "When Revealed: Seduced is found in the encounter deck and attached to your identity" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      _ <- stackEncounterDeck [MinionCards.hydraMercenary, AttachmentCards.seduced]
      _ <- revealNow p MinionCards.enchantress
      settle
      seducedOn p `shouldReturn` True

  it "or in the encounter discard pile" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      _ <- setEncounterDeck [MinionCards.hydraMercenary] [AttachmentCards.seduced]
      _ <- revealNow p MinionCards.enchantress
      settle
      seducedOn p `shouldReturn` True

  it "or in the set-aside area" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      _ <- setEncounterDeck [MinionCards.hydraMercenary] []
      seduced <- encounterCard AttachmentCards.seduced
      resolve [SetAside [EncounterCard seduced]]
      _ <- revealNow p MinionCards.enchantress
      settle
      attached <- seducedOn p
      aside <- getSetAsideEncounterCards
      liftIO do
        attached `shouldBe` True
        aside `shouldSatisfy` notElem seduced

  it "with no Seduced anywhere, nothing is attached" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      _ <- setEncounterDeck [MinionCards.hydraMercenary] []
      _ <- revealNow p MinionCards.enchantress
      settle
      seducedOn p `shouldReturn` False

seducedSpec :: Spec
seducedSpec = describe "Seduced (21179)" do
  it "you cannot make basic attacks or play Attack events; other attacks are not stopped" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      asHero p
      clearHand p
      kick <- putInHand p "01005"
      _ <- putInHand p "01088"
      _ <- putInHand p "01090"
      attackBefore <- offers (isHeroAttack p)
      kickBefore <- offers (isPlay kick)
      _ <- revealNow p AttachmentCards.seduced
      pass
      attackAfter <- offers (isHeroAttack p)
      kickAfter <- offers (isPlay kick)
      liftIO do
        (attackBefore, kickBefore) `shouldBe` (True, True)
        (attackAfter, kickAfter) `shouldBe` (False, False)

  it "stunned, you cannot attempt the basic attack that would remove it" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      asHero p
      _ <- revealNow p AttachmentCards.seduced
      pass
      resolve [IdentityMessage p IdentityStunned]
      offers (isHeroAttack p) `shouldReturn` False

  it "Alter-Ego Action: spend [energy] [mental] resources to discard it" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      _ <- revealNow p AttachmentCards.seduced
      pass
      [s] <- attachmentsOf AttachmentCards.seduced
      clearHand p
      energy <- putInHand p "01088"
      genius <- putInHand p "01089"
      choose "discard Seduced" (isAbilityOf (AttachmentRef $ Attachment.attachmentId s))
      payWith [energy, genius]
      pass
      gone <- inDiscard AttachmentCards.seduced
      attached <- seducedOn p
      liftIO $ (gone, attached) `shouldBe` (True, False)

beguiledSpec :: Spec
beguiledSpec = describe "Beguiled (21178)" do
  it "attaches to the ally with the highest cost: an Enthralled minion engaged with its controller, SCH its printed THW, its damage and attachments kept, ally-only bonuses inactive" do
    tg <- testGame ["captain-marvel"]
    let p = player1 tg
    inGame tg do
      _ <- putIntoPlay p "01067"
      _ <- putIntoPlay p "01068"
      pass
      vision <- allyIn "01068"
      clearHand p
      inspired <- putInHand p "01074"
      genius <- putInHand p "01089"
      playOn inspired (allyId vision) [genius]
      pass
      resolve [AllyMessage (allyId vision) (AllyDamaged (toSource p) (toDamage 1 FromAbility))]
      _ <- revealNow p AttachmentCards.beguiled
      settle
      let mid = asMinion vision
      Just m <- minionAttrs mid
      Just minion <- getsGame (HashMap.lookup mid . gameMinions)
      traits <- getTraits minion
      [b] <- attachmentsOf AttachmentCards.beguiled
      visions <- alliesWith "01068"
      marias <- alliesWith "01067"
      allies <- (.playerIdentityAllies) <$> playerAttrs p
      discard <- discardOf p
      Minion.getModifiedAttack m `shouldReturn` 2
      liftIO do
        minionEngagedIdentity m `shouldBe` p
        (minionScheme m, minionAttack m, minionHitPoints m, minionDamage m) `shouldBe` (Sch 1, Atk 2, HP 3, 1)
        traits `shouldSatisfy` member Enthralled
        traits `shouldSatisfy` member Android
        Attachment.attachmentEnemy b `shouldBe` Just (EnemyMinionId mid)
        (length visions, length marias) `shouldBe` (0, 1)
        allies `shouldSatisfy` (not . member (allyId vision))
        map pcCardId discard `shouldSatisfy` notElem (pcCardId inspired)

  it "its minion defeated: the ally card goes to its owner's discard pile, Beguiled to the encounter discard pile" do
    tg <- testGame ["captain-marvel"]
    let p = player1 tg
    inGame tg do
      vision <- putIntoPlay p "01068"
      pass
      a <- allyIn "01068"
      _ <- revealNow p AttachmentCards.beguiled
      settle
      resolve [MinionMessage (asMinion a) MinionDefeated]
      discard <- discardOf p
      gone <- inDiscard AttachmentCards.beguiled
      liftIO do
        map pcCardId discard `shouldSatisfy` elem (pcCardId vision)
        gone `shouldBe` True

  it "ties for the highest cost: the revealing player chooses" do
    tg <- testGame ["captain-marvel"]
    let p = player1 tg
    inGame tg do
      _ <- putIntoPlay p "01020"
      _ <- putIntoPlay p "01066"
      pass
      hellcat <- allyIn "01020"
      hawkeye <- allyIn "01066"
      _ <- revealNow p AttachmentCards.beguiled
      both <- (&&) <$> offers (isTargetChoice (AllyRef $ allyId hellcat)) <*> offers (isTargetChoice (AllyRef $ allyId hawkeye))
      choose "Hawkeye" (isTargetChoice (AllyRef $ allyId hawkeye))
      settle
      hawkeyeMinion <- isJust <$> minionAttrs (asMinion hawkeye)
      hellcats <- alliesWith "01020"
      liftIO do
        both `shouldBe` True
        hawkeyeMinion `shouldBe` True
        length hellcats `shouldBe` 1

  it "any player's ally: it engages its own controller" do
    tg <- testGame ["spider-man", "captain-marvel"]
    let (p1, p2) = (player1 tg, player2 tg)
    inGame tg do
      _ <- putIntoPlay p2 "01068"
      pass
      vision <- allyIn "01068"
      _ <- revealNow p1 AttachmentCards.beguiled
      settle
      Just m <- minionAttrs (asMinion vision)
      liftIO $ minionEngagedIdentity m `shouldBe` p2

  it "with no ally in play, it gains surge" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      [merc] <- stackEncounterDeck [MinionCards.hydraMercenary]
      _ <- revealNow p AttachmentCards.beguiled
      settle
      surged <- elem merc <$> dealtTo p
      gone <- inDiscard AttachmentCards.beguiled
      liftIO $ (surged, gone) `shouldBe` (True, True)

  it "Beguiled leaving play first: the minion is its owner's ally again, its damage kept" do
    tg <- testGame ["captain-marvel"]
    let p = player1 tg
    inGame tg do
      _ <- putIntoPlay p "01068"
      pass
      vision <- allyIn "01068"
      _ <- revealNow p AttachmentCards.beguiled
      settle
      resolve [MinionMessage (asMinion vision) (MinionDamaged (toSource p) (toDamage 2 FromAbility))]
      [b] <- attachmentsOf AttachmentCards.beguiled
      resolve [RemoveFromPlay (AttachmentRef $ Attachment.attachmentId b)]
      back <- allyIn "01068"
      liftIO $ (allyController back, allyDamage back) `shouldBe` (p, 2)

  it "Odin cannot have cards attached: with only him in play, it gains surge" do
    tg <- testGameAgainst "21136a" ["spider-man"]
    let p = player1 tg
    inGame tg do
      hall <- revealNow p SideSchemeCards.hallOfNastrond
      pass
      let sid = sideSchemeOf hall
      resolve
        [ CheckWindows [W.Window W.When (W.DefeatedSideScheme sid)]
        , SideSchemeMessage sid DefeatSideScheme
        , DefeatedLeavesPlay (SideSchemeRef sid)
        , CheckWindows [W.Window W.After (W.DefeatedSideSchemeBy sid p Nothing)]
        ]
      odin <- allyIn (toCardCode AllyCards.odinCaptive)
      [merc] <- stackEncounterDeck [MinionCards.hydraMercenary]
      _ <- revealNow p AttachmentCards.beguiled
      settle
      surged <- elem merc <$> dealtTo p
      still <- alliesWith (toCardCode AllyCards.odinCaptive)
      liftIO do
        surged `shouldBe` True
        map allyId still `shouldBe` [allyId odin]
