module Marvel.Encounter.RogueNemesisSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Attachment.Types qualified as Attachment
import Marvel.Message.Lifted qualified as L
import Marvel.Ally.Types qualified as Ally
import Marvel.Minion.Cards qualified as Minions
import Marvel.Minion.Types qualified as Minion
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Treachery.Cards qualified as Treacheries
import TestImport

settle :: GameAppT ()
settle = go (200 :: Int)
 where
  go 0 = error "Rogue encounter fixture did not settle"
  go n = openQuestion >>= \case
    Just (_, QuestionLabel "$label.yourTurn" _ _) -> pure ()
    Just (you, q) -> do
      let passing c = isNoResponse c || isNoDefender c || isLabel "$label.declineToDefend" c
      answerQuestion you (fromMaybe 0 $ headMay [i | (i,c) <- zip [0..] (choicesOf q), passing c])
      go (n-1)
    Nothing -> pure ()

perform :: L.QueueT GameAppT () -> GameAppT ()
perform action = L.runQueueT (L.capture action) >>= resolve

attachmentIdOf :: EncounterCard -> AttachmentId
attachmentIdOf = AttachmentId . unCardId . ecCardId

attachmentPresent :: EncounterCard -> GameAppT Bool
attachmentPresent card = getsGame (HashMap.member (attachmentIdOf card) . gameAttachments)

minionsWith :: CardCode -> GameAppT [Attrs Minion]
minionsWith code = getsGame (filter ((== code) . toCardCode) . map toAttrs . HashMap.elems . gameMinions)

playerDeckCards :: IdentityId -> GameAppT [PlayerCard]
playerDeckCards p = unDeck . playerIdentityDeck . toAttrs <$> getPlayer p

attachEnhancements :: IdentityId -> MinionId -> GameAppT EncounterCard
attachEnhancements p mid = do
  card <- revealNow p Attachments.cyberneticEnhancements
  settle
  host <- getsGame (fmap (Attachment.attachmentEnemy . toAttrs) . HashMap.lookup (attachmentIdOf card) . gameAttachments)
  host `shouldBe` Just (Just $ EnemyMinionId mid)
  pure card

spec :: Spec
spec = describe "Rogue nemesis and Reavers modular" do
  describe "Deadly Touch" do
    it "damages the friendly host and discards without adding threat even when a different player reveals it" do
      tg <- testGame ["rogue", "spider-man"]
      let p = player1 tg
          q = player2 tg
      inGame tg do
        ally <- putIntoPlay q "01020"
        perform $ L.findAndAttachTouched p (AllyCharacter $ allyIdOf ally)
        before <- mainSchemeThreatNow
        card <- revealNow q Obligations.deadlyTouch
        settle
        allyDamage <$> allyIn "01020" `shouldReturn` 2
        mainSchemeThreatNow `shouldReturn` before
        encounterDiscardNow >>= (`shouldSatisfy` elem card)

    it "adds two threat when Touched is set aside" do
      tg <- testGame ["rogue"]
      let p = player1 tg
      inGame tg do
        before <- mainSchemeThreatNow
        card <- revealNow p Obligations.deadlyTouch
        settle
        mainSchemeThreatNow `shouldReturn` before + 2
        encounterDiscardNow >>= (`shouldSatisfy` elem card)

    it "adds threat rather than damaging an enemy host" do
      tg <- testGame ["rogue"]
      let p = player1 tg
      inGame tg do
        vid <- villainIdNow
        perform $ L.findAndAttachTouched p (VillainCharacter vid)
        before <- mainSchemeThreatNow
        _ <- revealNow p Obligations.deadlyTouch
        settle
        villainDamageNow `shouldReturn` 0
        mainSchemeThreatNow `shouldReturn` before + 2

  describe "Mystique" do
    for_ ["deck", "discard", "set aside"] $ \zone ->
      it ("enters tough and shuffles Misled from the " <> zone <> " into the engaged player's deck") do
        tg <- testGame ["spider-man"]
        let p = player1 tg
        inGame tg do
          (deck, discard) <- setEncounterDeck
            ([Treacheries.misled | zone == "deck"] <> [Treacheries.advance])
            [Treacheries.misled | zone == "discard"]
          aside <- encounterCard Treacheries.misled
          when (zone == "set aside") $ resolve [SetAside [EncounterCard aside]]
          let expected = case zone of "deck" -> head deck; "discard" -> head discard; _ -> aside
          card <- revealNow p Minions.rogueMystique
          settle
          getsGame (fmap (Minion.minionTough . toAttrs) . HashMap.lookup (minionOf card) . gameMinions) `shouldReturn` Just True
          playerDeckCards p >>= (`shouldSatisfy` any ((== ecCardId expected) . pcCardId))
          encounterDiscardNow >>= (`shouldSatisfy` all ((/= ecCardId expected) . ecCardId))
          getSetAsideEncounterCards >>= (`shouldSatisfy` all ((/= ecCardId expected) . ecCardId))

    it "does not invent a Misled when none is available" do
      tg <- testGame ["spider-man"]
      inGame tg do
        _ <- setEncounterDeck [Treacheries.advance] []
        _ <- addMinion (player1 tg) Minions.rogueMystique
        playerDeckCards (player1 tg) >>= (`shouldSatisfy` all ((/= "38027") . toCardCode))

  describe "Mystique's Manipulations" do
    it "gives Misled to the defeating player instead of the revealer" do
      tg <- testGame ["spider-man", "captain-marvel"]
      let p = player1 tg
          q = player2 tg
      inGame tg do
        (_, discarded) <- setEncounterDeck [Treacheries.advance] [Treacheries.misled]
        scheme <- revealNow p Schemes.mystiquesManipulations
        settle
        resolve [SideSchemeMessage (sideSchemeOf scheme) $ SideSchemeThreatRemoved (IdentityRef q) 4]
        playerDeckCards q >>= (`shouldSatisfy` any ((== ecCardId (head discarded)) . pcCardId))
        playerDeckCards p >>= (`shouldSatisfy` all ((/= "38027") . toCardCode))

    it "does not search the set-aside area" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        _ <- setEncounterDeck [Treacheries.advance] []
        aside <- encounterCard Treacheries.misled
        resolve [SetAside [EncounterCard aside]]
        scheme <- revealNow p Schemes.mystiquesManipulations
        settle
        resolve [SideSchemeMessage (sideSchemeOf scheme) $ SideSchemeThreatRemoved (IdentityRef p) 2]
        getSetAsideEncounterCards >>= (`shouldSatisfy` elem aside)
        playerDeckCards p >>= (`shouldSatisfy` all ((/= "38027") . toCardCode))

  describe "Misled" do
    it "shuffles itself into the revealing player's deck, surges, then triggers on entering hand" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        _ <- setEncounterDeck [Treacheries.advance, Treacheries.assault] []
        beforeDealt <- length <$> dealtTo p
        card <- revealNow p Treacheries.misled
        settle
        (length <$> dealtTo p) `shouldReturn` beforeDealt + 1
        playerDeckCards p >>= (`shouldSatisfy` any ((== ecCardId card) . pcCardId))
        encounterDiscardNow >>= (`shouldSatisfy` notElem card)
        before <- mainSchemeThreatNow
        resolve [IdentityMessage p $ AddToHand $ encounterCardInPlayerDeck p card]
        mainSchemeThreatNow `shouldReturn` before + 2
        resolve [DiscardedCard $ PlayerCard $ encounterCardInPlayerDeck p card]
        encounterDiscardNow >>= (`shouldSatisfy` elem card)

  describe "Reaver engagement responses" do
    it "Donald Pierce reveals only the topmost discarded Reaver minion" do
      tg <- testGame ["spider-man", "captain-marvel"]
      let p = player1 tg
      inGame tg do
        (_, discarded) <- setEncounterDeck [Treacheries.advance, Treacheries.assault] [Treacheries.advance, Minions.wadeCole, Minions.murrayReese]
        _ <- revealNow p Minions.donaldPierce
        settle
        length <$> minionsWith "38032" `shouldReturn` 1
        minionsWith "38033" `shouldReturn` []
        encounterDiscardNow >>= (`shouldSatisfy` elem (discarded !! 2))

    it "Donald Pierce does nothing without a discarded Reaver minion" do
      tg <- testGame ["spider-man"]
      inGame tg do
        _ <- setEncounterDeck [Treacheries.advance] [Treacheries.assault]
        _ <- addMinion (player1 tg) Minions.donaldPierce
        getsGame (length . gameMinions) `shouldReturn` 1

    it "Skullbuster counts itself and enters tough" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        before <- mainSchemeThreatNow
        mid <- addMinion p Minions.skullbuster
        mainSchemeThreatNow `shouldReturn` before + 1
        getsGame (fmap (Minion.minionTough . toAttrs) . HashMap.lookup mid . gameMinions) `shouldReturn` Just True

    it "Bonebreaker counts itself for indirect damage and enters tough" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        mid <- addMinion p Minions.bonebreaker
        damageOf p `shouldReturn` 1
        getsGame (fmap (Minion.minionTough . toAttrs) . HashMap.lookup mid . gameMinions) `shouldReturn` Just True

    it "does not count Reavers engaged with another player" do
      tg <- testGame ["spider-man", "captain-marvel"]
      let p = player1 tg
          q = player2 tg
      inGame tg do
        _ <- addMinion q Minions.wadeCole
        mid <- addMinion q Minions.skullbuster
        before <- mainSchemeThreatNow
        perform $ L.engageMinion mid p
        mainSchemeThreatNow `shouldReturn` before + 1

  describe "Wade Cole and Murray Reese" do
    for_ [Minions.wadeCole, Minions.murrayReese] $ \def -> do
      for_ [True, False] $ \inDeck ->
        it (show (toCardCode def) <> " searches " <> (if inDeck then "the deck" else "the discard") <> " and attaches Cybernetic Enhancements to itself") do
          tg <- testGame ["spider-man"]
          let p = player1 tg
          inGame tg do
            _ <- setEncounterDeck
              ([Attachments.cyberneticEnhancements | inDeck] <> [Treacheries.advance])
              [Attachments.cyberneticEnhancements | not inDeck]
            mid <- addMinion p def
            attachments <- getsGame (map (Attachment.attachmentEnemy . toAttrs) . HashMap.elems . gameAttachments)
            attachments `shouldBe` [Just $ EnemyMinionId mid]
            resolve [MinionMessage mid $ MinionDamaged GameRef $ toDamage 99 FromAbility]
            minionDamageOf mid `shouldReturn` Just 0

      it (show (toCardCode def) <> " still enters play when no enhancements exist") do
        tg <- testGame ["spider-man"]
        inGame tg do
          _ <- setEncounterDeck [Treacheries.advance] []
          mid <- addMinion (player1 tg) def
          minionDamageOf mid `shouldReturn` Just 0
          getsGame (null . gameAttachments) `shouldReturn` True

  it "Teamwork activates only the entering Reaver, after its When Revealed search" do
    tg <- testGame ["spider-man", "captain-marvel"]
    let p = player1 tg
    inGame tg do
      _ <- setEncounterDeck [Treacheries.advance, Treacheries.assault] []
      _ <- addMinion p Minions.wadeCole
      before <- mainSchemeThreatNow
      _ <- addMinion p Minions.murrayReese
      mainSchemeThreatNow `shouldReturn` before + 1

  describe "The Reavers" do
    it "discards until a Reaver minion and reveals it for the defeating player, resolving When Revealed" do
      tg <- testGame ["spider-man", "captain-marvel"]
      let p = player1 tg
          q = player2 tg
      inGame tg do
        (deck, _) <- setEncounterDeck [Treacheries.advance, Minions.wadeCole, Attachments.cyberneticEnhancements, Treacheries.assault] []
        scheme <- revealNow p Schemes.theReavers
        settle
        resolve [SideSchemeMessage (sideSchemeOf scheme) $ SideSchemeThreatRemoved (IdentityRef q) 5]
        minions <- minionsWith "38032"
        map Minion.minionEngagedIdentity minions `shouldBe` [q]
        getsGame (length . gameAttachments) `shouldReturn` 1
        encounterDiscardNow >>= (`shouldSatisfy` elem (head deck))

  describe "Cybernetic Enhancements" do
    it "adds one ATK, prevents damage, then discards only after the attached minion attacks" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        mid <- addMinion p Minions.shocker
        card <- attachEnhancements p mid
        resolve [MinionMessage mid $ MinionDamaged GameRef $ toDamage 5 FromAbility]
        minionDamageOf mid `shouldReturn` Just 0
        runNow [MinionMessage mid $ MinionAttacks p]
        settle
        damageOf p `shouldReturn` 3
        attachmentPresent card `shouldReturn` False
        resolve [MinionMessage mid $ MinionDamaged GameRef $ toDamage 1 FromAbility]
        minionDamageOf mid `shouldReturn` Just 1

    it "does not discard after a different enemy attacks or the host schemes" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        host <- addMinion p Minions.shocker
        card <- attachEnhancements p host
        other <- addMinion p Minions.armoredGuard
        runNow [MinionMessage other $ MinionAttacks p]
        settle
        attachmentPresent card `shouldReturn` True
        resolve [MinionMessage host $ MinionSchemesAgainst p]
        attachmentPresent card `shouldReturn` True

    it "stays attached after Karma's conversion but neither boosts nor protects the ally" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        mid <- addMinion p Minions.shocker
        card <- attachEnhancements p mid
        perform $ L.takeControlOfMinionWithConsequential p mid 2
        let aid = AllyId $ unMinionId mid
        attachmentPresent card `shouldReturn` True
        resolve [AllyMessage aid $ AllyDamaged GameRef $ toDamage 1 FromAbility]
        allyDamage <$> allyIn "01103" `shouldReturn` 1
        (Ally.getModifiedAttack =<< allyIn "01103") `shouldReturn` 2

    it "surges without a minion" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        _ <- setEncounterDeck [Treacheries.advance, Treacheries.assault] []
        before <- length <$> dealtTo p
        card <- revealNow p Attachments.cyberneticEnhancements
        settle
        attachmentPresent card `shouldReturn` False
        (length <$> dealtTo p) `shouldReturn` before + 1
