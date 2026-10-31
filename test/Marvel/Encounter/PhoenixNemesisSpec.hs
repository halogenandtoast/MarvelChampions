module Marvel.Encounter.PhoenixNemesisSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Minion.Cards qualified as Minions
import Marvel.Minion.Types qualified as Minion
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.SideScheme.Types qualified as SideScheme
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Message.Lifted qualified as Lifted
import Marvel.Query
import TestImport

-- Resolve the permanent upgrade's mandatory response before continuing an event.
settlePhoenix :: GameAppT ()
settlePhoenix = do
  pass
  forces <- (<>) <$> upgradesWith "34002a" <*> upgradesWith "34002b"
  offered <- filterM (offers . isAbilityOf . upgradeId) forces
  for_ (headMay offered) $ \force -> do
    choose "forced Phoenix Force flip" (isAbilityOf $ upgradeId force)
    settlePhoenix

forceId :: IdentityId -> GameAppT UpgradeId
forceId you = selectJust (UpgradeNamed "Phoenix Force" <> UpgradeControlledBy (IdentityWithId you))

powerOf :: IdentityId -> GameAppT Natural
powerOf you = do
  uid <- forceId you
  getsGame (upgradeUses . toAttrs . fromMaybe (error "Phoenix Force missing") . HashMap.lookup uid . gameUpgrades)

unleash :: IdentityId -> GameAppT ()
unleash you = do
  uid <- forceId you
  resolve $ replicate 4 (UpgradeMessage uid SpendUpgradeUse)

getDarkPhoenix :: MinionId -> GameAppT (Attrs Minion)
getDarkPhoenix mid = getsGame (toAttrs . fromMaybe (error "Dark Phoenix missing") . HashMap.lookup mid . gameMinions)

revealConsume :: IdentityId -> GameAppT SideSchemeId
revealConsume you = do
  aside <- filter ((== "34030") . toCardCode) <$> getSetAsideEncounterCards
  card <- maybe (encounterCard SideSchemes.consumeTheWorld) pure (headMay aside)
  runNow [RevealEncounterCard you card]
  pass
  pure $ sideSchemeOf card

spec :: Spec
spec = describe "PhoenixNemesis" do
  it "Burning Hunger removes a restrained power counter, surges, and is discarded" do
    tg <- testGame ["phoenix"]
    inGame tg do
      let you = player1 tg
      _ <- stackEncounterDeck [Treacheries.falseAlarm]
      _ <- revealNow you Obligations.burningHunger
      settlePhoenix
      powerOf you `shouldReturn` 3
      encounterDiscardCodes >>= (`shouldSatisfy` elem "34028")
      (map toCardCode <$> dealtTo you) >>= (`shouldSatisfy` elem "01112")
      selectAny (MinionIs Minions.darkPhoenix) `shouldReturn` False

  it "Burning Hunger is passed to Jean Grey when another player reveals it" do
    tg <- testGame ["spider-man", "phoenix"]
    inGame tg do
      let jean = player2 tg
      _ <- stackEncounterDeck [Treacheries.falseAlarm]
      _ <- revealNow (player1 tg) Obligations.burningHunger
      settlePhoenix
      powerOf jean `shouldReturn` 3

  it "Burning Hunger spending the last restrained counter does not summon Dark Phoenix" do
    tg <- testGame ["phoenix"]
    inGame tg do
      let you = player1 tg
      uid <- forceId you
      resolve $ replicate 3 (UpgradeMessage uid SpendUpgradeUse)
      _ <- stackEncounterDeck [Treacheries.falseAlarm]
      _ <- revealNow you Obligations.burningHunger
      settlePhoenix
      powerOf you `shouldReturn` 0
      upgradeCodes >>= (`shouldSatisfy` elem "34002b")
      selectAny (MinionIs Minions.darkPhoenix) `shouldReturn` False

  it "Burning Hunger unleashes the set-aside nemesis and her scheme then removes itself" do
    tg <- testGame ["phoenix"]
    inGame tg do
      let you = player1 tg
      unleash you
      _ <- revealNow you Obligations.burningHunger
      settlePhoenix
      selectAny (MinionIs Minions.darkPhoenix <> MinionEngagedWith (IdentityWithId you)) `shouldReturn` True
      sid <- selectJust (SideSchemeIs SideSchemes.consumeTheWorld)
      sideSchemeThreat sid `shouldReturn` 6
      encounterDiscardCodes >>= (`shouldSatisfy` notElem "34028")
      getsGame (HashMap.size . gameObligations) `shouldReturn` 0

  for_ ["deck", "discard"] $ \zone ->
    it ("Burning Hunger can find Dark Phoenix in the encounter " <> zone) do
      tg <- testGame ["phoenix"]
      inGame tg do
        let you = player1 tg
        unleash you
        if zone == "deck"
          then void $ setEncounterDeck [Minions.darkPhoenix, Treacheries.hardToKeepDown] []
          else void $ setEncounterDeck [Treacheries.hardToKeepDown] [Minions.darkPhoenix]
        _ <- revealNow you Obligations.burningHunger
        settlePhoenix
        selectAny (MinionIs Minions.darkPhoenix) `shouldReturn` True

  it "Dark Phoenix reveals Consume the World from the set-aside area and enters tough" do
    tg <- testGame ["phoenix"]
    inGame tg do
      let you = player1 tg
      card <- revealNow you Minions.darkPhoenix
      settlePhoenix
      attrs <- getDarkPhoenix (minionOf card)
      minionTough attrs `shouldBe` True
      Minion.getModifiedKeywords attrs >>= (`shouldSatisfy` (\ks -> all (`elem` ks) [Steady, Toughness, Villainous]))
      sid <- selectJust (SideSchemeIs SideSchemes.consumeTheWorld)
      sideSchemeThreat sid `shouldReturn` 6

  for_ ["deck", "discard"] $ \zone ->
    it ("Dark Phoenix can find Consume the World in the encounter " <> zone) do
      tg <- testGame ["phoenix"]
      inGame tg do
        let you = player1 tg
        aside <- filter ((== "34030") . toCardCode) <$> getSetAsideEncounterCards
        resolve [RemoveFromGame (CardIdRef $ ecCardId c) | c <- aside]
        if zone == "deck"
          then void $ setEncounterDeck [SideSchemes.consumeTheWorld, Treacheries.hardToKeepDown] []
          else void $ setEncounterDeck [Treacheries.hardToKeepDown] [SideSchemes.consumeTheWorld]
        _ <- revealNow you Minions.darkPhoenix
        settlePhoenix
        sid <- selectJust (SideSchemeIs SideSchemes.consumeTheWorld)
        sideSchemeThreat sid `shouldReturn` 6

  it "Dark Phoenix's villainous scheme places boosted threat on Consume the World" do
    tg <- testGame ["phoenix"]
    inGame tg do
      let you = player1 tg
      card <- revealNow you Minions.darkPhoenix
      settlePhoenix
      sid <- selectJust (SideSchemeIs SideSchemes.consumeTheWorld)
      before <- mainSchemeThreatNow
      _ <- stackEncounterDeck [Treacheries.explosion]
      resolve [MinionMessage (minionOf card) MinionSchemes]
      sideSchemeThreat sid `shouldReturn` 11
      mainSchemeThreatNow `shouldReturn` before

  it "Dark Phoenix schemes onto the main scheme if Consume the World is absent" do
    tg <- testGame ["phoenix"]
    inGame tg do
      let you = player1 tg
      card <- encounterCard Minions.darkPhoenix
      resolve [PutEncounterCardIntoPlay you card]
      let mid = minionOf card
      before <- mainSchemeThreatNow
      _ <- stackEncounterDeck [Treacheries.hardToKeepDown]
      resolve [MinionMessage mid MinionSchemes]
      mainSchemeThreatNow `shouldReturn` (before + 2)

  it "Dark Phoenix's steady requires two confused cards to replace a scheme" do
    tg <- testGame ["phoenix"]
    inGame tg do
      let you = player1 tg
      mid <- addMinion you Minions.darkPhoenix
      sid <- selectJust (SideSchemeIs SideSchemes.consumeTheWorld)
      _ <- stackEncounterDeck [Treacheries.hardToKeepDown, Treacheries.hardToKeepDown]
      resolve [GiveStatus (MinionRef mid) ConfusedStatus, MinionMessage mid MinionSchemes]
      sideSchemeThreat sid `shouldReturn` 9
      resolve [GiveStatus (MinionRef mid) ConfusedStatus, MinionMessage mid MinionSchemes]
      sideSchemeThreat sid `shouldReturn` 9
      attrs <- getDarkPhoenix mid
      (minionConfused attrs, minionAdditionalConfuse attrs) `shouldBe` (False, False)

  it "Consume the World stays at zero threat, loses amplify, and regains it when threat is placed" do
    tg <- testGame ["phoenix"]
    inGame tg do
      let you = player1 tg
      sid <- revealConsume you
      getAmplifyCount `shouldReturn` 1
      resolve [SideSchemeMessage sid (SideSchemeThreatRemoved GameRef 6)]
      sideSchemeThreat sid `shouldReturn` 0
      selectAny (SideSchemeWithId sid) `shouldReturn` True
      getAmplifyCount `shouldReturn` 0
      resolve [SideSchemeMessage sid (SideSchemePlaceThreat 1)]
      getAmplifyCount `shouldReturn` 1

  it "Consume the World loses the game only when threat placed reaches twelve" do
    tg <- testGame ["phoenix"]
    inGame tg do
      sid <- revealConsume (player1 tg)
      resolve [SideSchemeMessage sid (SideSchemePlaceThreat 5)]
      getsGame gameState `shouldReturn` InProgress
      resolve [SideSchemeMessage sid (SideSchemePlaceThreat 1)]
      getsGame gameState `shouldReturn` Finished Lost

  it "Fiery Rage without Dark Phoenix places one threat and surges" do
    tg <- testGame ["phoenix"]
    inGame tg do
      let you = player1 tg
      sid <- revealConsume you
      _ <- stackEncounterDeck [Treacheries.falseAlarm]
      _ <- revealNow you Treacheries.fieryRage
      settlePhoenix
      sideSchemeThreat sid `shouldReturn` 7
      (map toCardCode <$> dealtTo you) >>= (`shouldSatisfy` elem "01112")

  it "Fiery Rage surges even when neither nemesis card is in play" do
    tg <- testGame ["phoenix"]
    inGame tg do
      let you = player1 tg
      _ <- stackEncounterDeck [Treacheries.falseAlarm]
      _ <- revealNow you Treacheries.fieryRage
      settlePhoenix
      (map toCardCode <$> dealtTo you) >>= (`shouldSatisfy` elem "01112")
      selectAny (SideSchemeIs SideSchemes.consumeTheWorld) `shouldReturn` False

  it "Fiery Rage makes Dark Phoenix scheme in alter-ego without surging" do
    tg <- testGame ["phoenix"]
    inGame tg do
      let you = player1 tg
      _ <- addMinion you Minions.darkPhoenix
      sid <- selectJust (SideSchemeIs SideSchemes.consumeTheWorld)
      _ <- stackEncounterDeck [Treacheries.hardToKeepDown, Treacheries.hardToKeepDown]
      before <- encounterDeckSize
      _ <- revealNow you Treacheries.fieryRage
      settlePhoenix
      sideSchemeThreat sid `shouldReturn` 9
      encounterDeckSize `shouldReturn` (before - 1)

  it "Fiery Rage makes Dark Phoenix attack its revealing hero rather than its engaged player" do
    tg <- testGame ["phoenix", "spider-man"]
    inGame tg do
      let jean = player1 tg
          peter = player2 tg
      _ <- addMinion jean Minions.darkPhoenix
      resolve [IdentityMessage peter (ChangedToForm A)]
      _ <- stackEncounterDeck [Treacheries.hardToKeepDown]
      _ <- revealNow peter Treacheries.fieryRage
      settlePhoenix
      damageOf peter `shouldReturn` 3
      damageOf jean `shouldReturn` 0

  it "Fiery Rage keeps peril active and suppresses another player's defense event throughout its attack" do
    tg <- testGame ["phoenix", "spider-man"]
    inGame tg do
      let jean = player1 tg
          peter = player2 tg
      resolve [IdentityMessage jean (ChangedToForm A), IdentityMessage peter (ChangedToForm A)]
      clearHand jean
      clearHand peter
      _ <- addMinion jean Minions.darkPhoenix
      backflip <- putInHand peter "01003"
      _ <- stackEncounterDeck [Treacheries.explosion]
      _ <- revealNow jean Treacheries.fieryRage
      choose "ask Spider-Man to defend" (isTargetChoice peter)
      getsGame gamePeril `shouldReturn` Just jean
      choose "Spider-Man defends" (isTargetChoice peter)
      offers (isPlay backflip) `shouldReturn` False
      settlePhoenix
      damageOf peter `shouldReturn` 2
      damageOf jean `shouldReturn` 0
      getsGame gamePeril `shouldReturn` Nothing

  it "Burning Hunger is discarded rather than removed when Dark Phoenix cannot be found" do
    tg <- testGame ["phoenix"]
    inGame tg do
      let you = player1 tg
      unleash you
      cards <- filter ((== "34029") . toCardCode) <$> getSetAsideEncounterCards
      for_ cards $ \card -> resolve [PutEncounterCardIntoPlay you card]
      _ <- setEncounterDeck [Treacheries.falseAlarm] []
      _ <- revealNow you Obligations.burningHunger
      settlePhoenix
      encounterDiscardCodes >>= (`shouldSatisfy` elem "34028")

  it "Shadow of the Past initializes Consume the World only once when Dark Phoenix reveals it" do
    tg <- testGame ["phoenix"]
    inGame tg do
      let you = player1 tg
      liftIO $ writeIORef tg.ran []
      _ <- revealNow you Treacheries.shadowOfThePast
      settlePhoenix
      sid <- selectJust (SideSchemeIs SideSchemes.consumeTheWorld)
      sideSchemeThreat sid `shouldReturn` 6
      selectAny (MinionIs Minions.darkPhoenix) `shouldReturn` True
      trace <- liftIO $ readIORef tg.ran
      let initialized = tshow $ SideSchemeMessage sid SideSchemePlaceInitialThreat
      length (filter (== initialized) trace) `shouldBe` 1
