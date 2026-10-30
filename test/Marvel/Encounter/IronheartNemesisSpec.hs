module Marvel.Encounter.IronheartNemesisSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Ability.Types qualified as Ability
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Attachment.Types qualified as Attachment
import Marvel.EncounterSet (EncounterSet (IronheartNemesis), getNemesisSet)
import Marvel.Game.Source
import Marvel.Keyword (Keyword (Retaliate))
import Marvel.Matchers hiding (ExhaustedAlly, ExhaustedIdentity)
import Marvel.Minion.Cards qualified as Minions
import Marvel.Minion.Types qualified as Minion
import Marvel.Modifier
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.Phase
import Marvel.Query
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Status
import Marvel.Treachery.Cards qualified as Treacheries
import TestImport

settle :: GameAppT ()
settle = do
  pressWhenRevealed
  forced <- offers isForced
  if forced then choose "resolve the forced encounter ability" isForced >> settle
    else do
      pass
      whenM (offers isForced) settle
 where
  isForced = \case
    UseAbility a -> abilityType a `elem` [Ability.ForcedInterrupt, Ability.ForcedResponse]
    _ -> False

minionAttrs :: MinionId -> GameAppT (Attrs Minion)
minionAttrs mid = getsGame (maybe (error "missing minion") toAttrs . HashMap.lookup mid . gameMinions)

attackOf :: MinionId -> GameAppT Natural
attackOf mid = Minion.getModifiedAttack =<< minionAttrs mid

schemeOf :: MinionId -> GameAppT Natural
schemeOf mid = Minion.getModifiedScheme =<< minionAttrs mid

attachmentOf :: EncounterCard -> AttachmentId
attachmentOf = AttachmentId . unCardId . ecCardId

surgeDealtTo :: IdentityId -> GameAppT Bool
surgeDealtTo p = any ((== toCardCode Minions.hydraMercenary) . toCardCode) <$> dealtTo p

spec :: Spec
spec = describe "Ironheart nemesis" do
  it "registers the five-card nemesis set" do
    getNemesisSet "29001a" `shouldBe` IronheartNemesis :: IO ()

  describe "A Minor Setback" do
    it "removes one progress counter and discards without dealing an encounter card" do
      tg <- testGame ["ironheart"]
      let p = player1 tg
      inGame tg do
        resolve [PlaceCounters (IdentityRef p) 3]
        before <- length <$> dealtTo p
        _ <- revealNow p Obligations.aMinorSetback
        settle
        playerIdentityCounters <$> playerAttrs p `shouldReturn` 2
        length <$> dealtTo p `shouldReturn` before
        encounterDiscardCodes >>= (`shouldSatisfy` elem "29028")

    it "deals an encounter card then shuffles itself instead of discarding when no counter exists" do
      tg <- testGame ["ironheart"]
      let p = player1 tg
      inGame tg do
        _ <- stackEncounterDeck [Minions.hydraMercenary]
        before <- length <$> dealtTo p
        _ <- revealNow p Obligations.aMinorSetback
        settle
        length <$> dealtTo p `shouldReturn` before + 1
        encounterDiscardCodes >>= (`shouldSatisfy` notElem "29028")
        deck <- scenarioEncounterDeckNow
        map toCardCode deck `shouldSatisfy` elem "29028"
        obligationsInPlay `shouldReturn` 0

  describe "Rule by Force" do
    it "starts with four fixed threat and exchanges acceleration for hazard as Lucia enters and leaves" do
      tg <- testGame ["ironheart", "spider-man"]
      let p = player1 tg
      inGame tg do
        card <- revealNow p Schemes.ruleByForce
        settle
        sideSchemeThreat (sideSchemeOf card) `shouldReturn` 4
        mods <- getModifiers (SideSchemeRef $ sideSchemeOf card)
        mods `shouldSatisfy` elem (AccelerationIconModifier 1)
        mods `shouldSatisfy` notElem (HazardIconModifier 1)
        lucia <- addMinion p Minions.luciaVonBardas
        getModifiers (SideSchemeRef $ sideSchemeOf card) >>= (`shouldSatisfy` elem (HazardIconModifier 1))
        resolve [RemoveFromPlay (MinionRef lucia)]
        getModifiers (SideSchemeRef $ sideSchemeOf card) >>= (`shouldSatisfy` elem (AccelerationIconModifier 1))

  describe "Lucia von Bardas" do
    it "gets one ATK and SCH only while tough and gains tough after the villain phase" do
      tg <- testGame ["ironheart"]
      let p = player1 tg
      inGame tg do
        lucia <- addMinion p Minions.luciaVonBardas
        attackOf lucia `shouldReturn` 1
        schemeOf lucia `shouldReturn` 2
        resolve [EndPhase VillainPhase]
        minionTough <$> minionAttrs lucia `shouldReturn` True
        attackOf lucia `shouldReturn` 2
        schemeOf lucia `shouldReturn` 3
        resolve [RemoveStatus (MinionRef lucia) ToughStatus]
        attackOf lucia `shouldReturn` 1
        schemeOf lucia `shouldReturn` 2
        runNow [BeginPhase PlayerPhase]
        minionTough <$> minionAttrs lucia `shouldReturn` False

  describe "Cyborg Tech" do
    it "attaches to the minion with most traits and grants printed attack, hit points and retaliate" do
      tg <- testGame ["ironheart"]
      let p = player1 tg
      inGame tg do
        _ <- addMinion p Minions.hydraSoldier
        lucia <- addMinion p Minions.luciaVonBardas
        card <- revealNow p Attachments.cyborgTech
        settle
        target <- getsGame (fmap (Attachment.attachmentEnemy . toAttrs) . HashMap.lookup (attachmentOf card) . gameAttachments)
        target `shouldBe` Just (Just $ EnemyMinionId lucia)
        attackOf lucia `shouldReturn` 2
        attrs <- minionAttrs lucia
        Minion.getModifiedHitPoints attrs `shouldReturn` 7
        Minion.getModifiedKeywords attrs >>= (`shouldSatisfy` elem (Retaliate 1))

    it "lets the first player choose among tied most-trait minions" do
      tg <- testGame ["ironheart", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        one <- addMinion p Minions.hydraSoldier
        two <- addMinion p Minions.hydraSoldier
        card <- revealNow other Attachments.cyborgTech
        pressWhenRevealed
        fmap fst <$> asked `shouldReturn` Just p
        offers (isTargetChoice one) `shouldReturn` True
        offers (isTargetChoice two) `shouldReturn` True
        choose "attach to the second tied minion" (isTargetChoice two)
        settle
        target <- getsGame (fmap (Attachment.attachmentEnemy . toAttrs) . HashMap.lookup (attachmentOf card) . gameAttachments)
        target `shouldBe` Just (Just $ EnemyMinionId two)

    it "surges without a minion and is discarded" do
      tg <- testGame ["ironheart"]
      let p = player1 tg
      inGame tg do
        _ <- stackEncounterDeck [Minions.hydraMercenary]
        _ <- revealNow p Attachments.cyborgTech
        settle
        surgeDealtTo p `shouldReturn` True
        encounterDiscardCodes >>= (`shouldSatisfy` elem "29031")

    it "its boost is dealt facedown rather than discarded" do
      tg <- testGame ["ironheart"]
      let p = player1 tg
      inGame tg do
        changeForm p
        pass
        [boost] <- stackEncounterDeck [Attachments.cyborgTech]
        vid <- villainIdNow
        runNow [VillainMessage vid (VillainAttacks p)]
        settle
        map ecCardId <$> dealtTo p >>= (`shouldSatisfy` elem (ecCardId boost))
        encounterDiscardCodes >>= (`shouldSatisfy` notElem "29031")

  describe "Political Retribution" do
    it "makes Lucia scheme and places three threat on Rule by Force when both are present" do
      tg <- testGame ["ironheart"]
      let p = player1 tg
      inGame tg do
        _ <- addMinion p Minions.luciaVonBardas
        card <- revealNow p Schemes.ruleByForce
        settle
        before <- mainSchemeThreatNow
        _ <- revealNow p Treacheries.politicalRetribution
        settle
        mainSchemeThreatNow `shouldReturn` before + 2
        sideSchemeThreat (sideSchemeOf card) `shouldReturn` 7

    it "places three threat without surging when only Rule by Force is present" do
      tg <- testGame ["ironheart"]
      let p = player1 tg
      inGame tg do
        card <- revealNow p Schemes.ruleByForce
        settle
        _ <- stackEncounterDeck [Minions.hydraMercenary]
        _ <- revealNow p Treacheries.politicalRetribution
        settle
        sideSchemeThreat (sideSchemeOf card) `shouldReturn` 7
        surgeDealtTo p `shouldReturn` False

    it "makes Lucia scheme without surging when the side scheme is absent" do
      tg <- testGame ["ironheart"]
      let p = player1 tg
      inGame tg do
        lucia <- addMinion p Minions.luciaVonBardas
        resolve [GiveStatus (MinionRef lucia) ToughStatus]
        before <- mainSchemeThreatNow
        _ <- stackEncounterDeck [Minions.hydraMercenary]
        _ <- revealNow p Treacheries.politicalRetribution
        settle
        mainSchemeThreatNow `shouldReturn` before + 3
        surgeDealtTo p `shouldReturn` False

    it "surges only when neither nemesis is in play" do
      tg <- testGame ["ironheart"]
      let p = player1 tg
      inGame tg do
        _ <- stackEncounterDeck [Minions.hydraMercenary]
        _ <- revealNow p Treacheries.politicalRetribution
        settle
        surgeDealtTo p `shouldReturn` True
