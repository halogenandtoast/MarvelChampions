module Marvel.Encounter.SpectrumNemesisSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Data.List (sort)
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Attachment.Types (Attrs (..))
import Marvel.Boost (BoostIcon (Star))
import Marvel.EncounterSet (EncounterSet (SpectrumNemesis), getNemesisSet)
import Marvel.Minion.Cards qualified as Minions
import Marvel.Scenario.Types (Attrs (scenarioSetAsideCards))
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Treachery.Cards qualified as Treacheries
import TestImport

asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass

hellcat :: CardCode
hellcat = "01020"

hellcatDamage :: GameAppT Natural
hellcatDamage = (.allyDamage) <$> allyIn hellcat

attachmentOf :: EncounterCard -> AttachmentId
attachmentOf = AttachmentId . unCardId . ecCardId

-- | The identity the attachment is attached to, Nothing once it has left play.
attachedIdentity :: EncounterCard -> GameAppT (Maybe (Maybe IdentityId))
attachedIdentity card = getsGame (fmap (attachmentIdentity . toAttrs) . HashMap.lookup (attachmentOf card) . gameAttachments)

-- | The minion attacks the player, undefended.
attackedBy :: MinionId -> IdentityId -> GameAppT ()
attackedBy m p = do
  -- its boost card: Hail Hydra! has no boost icons
  _ <- stackEncounterDeck [Treacheries.hailHydra]
  start [MinionMessage m (MinionAttacks p)]
  choose "no defender" isNoDefender
  chooseIf (isAbilityOf m)
  pass

minionCodes :: GameAppT [CardCode]
minionCodes = getsGame (map (toCardCode . getCardDef) . HashMap.elems . gameMinions)

sideSchemeCodes :: GameAppT [CardCode]
sideSchemeCodes = getsGame (map (toCardCode . getCardDef) . HashMap.elems . gameSideSchemes)

spec :: Spec
spec = describe "Spectrum's nemesis set" do
  it "is Spectrum's nemesis set" do
    getNemesisSet "21001" `shouldBe` SpectrumNemesis :: IO ()

  it "is set aside at setup" do
    tg <- testGame ["spectrum"]
    inGame tg do
      aside <- getsGame (map toCardCode . (.scenarioSetAsideCards) . toAttrs . gameScenario)
      for_ ["21027", "21028", "21029", "21030"] \c -> aside `shouldSatisfy` elem c
      liftIO $ length (filter (== "21029") aside) `shouldBe` 2

  it "Shadow of the Past brings out Radioactive Man and Reactor Meltdown, the rest shuffled in" do
    tg <- testGame ["spectrum"]
    let p = player1 tg
    inGame tg do
      _ <- revealNow p Treacheries.shadowOfThePast
      pass
      minionCodes >>= (`shouldSatisfy` elem "21027")
      sideSchemeCodes >>= (`shouldSatisfy` elem "21028")
      sort . filter (`elem` ["21029", "21030"]) . map toCardCode <$> scenarioEncounterDeckNow
        `shouldReturn` ["21029", "21029", "21030"]

  describe "Radioactive Man (21027)" do
    it "is unique, Elite and Genius, with a [star] boost" do
      (liftIO :: IO () -> IO ()) do
        cdUnique Minions.radioactiveManNemesis `shouldBe` True
        cdBoostIcons Minions.radioactiveManNemesis `shouldBe` [Star]

    it "after he attacks you, deals 1 damage to each character you control" do
      tg <- testGame ["spider-man", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        asHero p
        _ <- putIntoPlay p hellcat
        m <- addMinion p Minions.radioactiveManNemesis
        m `attackedBy` p
        -- his 1 ATK and 1 more
        damageOf p `shouldReturn` 2
        hellcatDamage `shouldReturn` 1
        damageOf other `shouldReturn` 0

    it "after he schemes while engaged with you, deals 1 damage to each character you control" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        _ <- putIntoPlay p hellcat
        m <- addMinion p Minions.radioactiveManNemesis
        before <- mainSchemeThreatNow
        resolve [MinionMessage m MinionSchemes]
        after <- mainSchemeThreatNow
        liftIO $ after - before `shouldBe` 1
        damageOf p `shouldReturn` 1
        hellcatDamage `shouldReturn` 1

    it "does not answer another player's characters" do
      tg <- testGame ["spider-man", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        _ <- putIntoPlay other hellcat
        m <- addMinion p Minions.radioactiveManNemesis
        resolve [MinionMessage m MinionSchemes]
        damageOf p `shouldReturn` 1
        hellcatDamage `shouldReturn` 0
        damageOf other `shouldReturn` 0

    it "as a boost card, deals 1 damage to each character the attacked player controls" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        asHero p
        _ <- putIntoPlay p hellcat
        _ <- stackEncounterDeck [Minions.radioactiveManNemesis]
        vid <- villainIdNow
        runNow [VillainMessage vid (VillainAttacks p)]
        pass
        -- Rhino's 2 ATK, no boost icons, and the boost's 1
        damageOf p `shouldReturn` 3
        hellcatDamage `shouldReturn` 1

  describe "Reactor Meltdown (21028)" do
    it "enters with 2 threat per player and has a crisis icon" do
      tg <- testGame ["spider-man", "spider-man"]
      let p = player1 tg
      inGame tg do
        scheme <- sideSchemeOf <$> revealNow p SideSchemes.reactorMeltdown
        pass
        sideSchemeThreat scheme `shouldReturn` 4
        m <- mainSchemeIdNow
        asHero p
        choose "thwart" (isHeroThwart p)
        offers (isTargetChoice m) `shouldReturn` False

    it "when defeated, deals 1 damage to each friendly character in play" do
      tg <- testGame ["spider-man", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        _ <- putIntoPlay other hellcat
        scheme <- sideSchemeOf <$> revealNow p SideSchemes.reactorMeltdown
        pass
        resolve [SideSchemeMessage scheme (SideSchemeThwarted (IdentityRef p) 4)]
        getsGame (HashMap.member scheme . gameSideSchemes) `shouldReturn` False
        damageOf p `shouldReturn` 1
        damageOf other `shouldReturn` 1
        hellcatDamage `shouldReturn` 1

  describe "Sap Power (21029)" do
    it "attaches to your identity; after your turn ends you take 1 damage" do
      tg <- testGame ["spider-man", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        card <- revealNow p Attachments.sapPower
        pass
        attachedIdentity card `shouldReturn` Just (Just p)
        damageOf p `shouldReturn` 0
        endTurn
        chooseIf (isAbilityOf (attachmentOf card))
        damageOf p `shouldReturn` 1
        damageOf other `shouldReturn` 0

    it "Alter-Ego Action: spend [energy][energy] to discard it" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        card <- revealNow p Attachments.sapPower
        pass
        clearHand p
        e1 <- putInHand p "01088"
        e2 <- putInHand p "01088"
        choose "discard Sap Power" (isAbilityOf (attachmentOf card))
        payWith [e1, e2]
        attachedIdentity card `shouldReturn` Nothing

    it "is no hero action, nor paid with a single energy" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        card <- revealNow p Attachments.sapPower
        pass
        clearHand p
        -- White Tiger's single [energy]: an Energy card would be two
        _ <- putInHand p "21013"
        offers (isAbilityOf (attachmentOf card)) `shouldReturn` False
        _ <- putInHand p "21013"
        asHero p
        offers (isAbilityOf (attachmentOf card)) `shouldReturn` False

  describe "Radioactive Blast (21030)" do
    it "in alter-ego, places 2 threat on the main scheme" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        before <- mainSchemeThreatNow
        _ <- revealNow p Treacheries.radioactiveBlast
        pass
        after <- mainSchemeThreatNow
        liftIO $ after - before `shouldBe` 2
        damageOf p `shouldReturn` 0

    it "as a hero, you take 2 damage" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        asHero p
        before <- mainSchemeThreatNow
        _ <- revealNow p Treacheries.radioactiveBlast
        pass
        damageOf p `shouldReturn` 2
        mainSchemeThreatNow `shouldReturn` before
