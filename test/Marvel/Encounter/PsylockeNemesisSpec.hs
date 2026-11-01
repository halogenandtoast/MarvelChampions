module Marvel.Encounter.PsylockeNemesisSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Message.Lifted qualified as L
import Marvel.Minion.Cards qualified as Minions
import Marvel.Minion.Runner qualified as Minion
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.Obligation.Types qualified as Obligation
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.SideScheme.Types qualified as SideScheme
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Villain.Types (villainConfused)
import TestImport

settle :: GameAppT ()
settle = go (200 :: Int)
 where
  go 0 = error "Psylocke encounter fixture did not settle"
  go n = openQuestion >>= \case
    Just (_, QuestionLabel "$label.yourTurn" _ _) -> pure ()
    Just (you, q) -> do
      let passing c = isNoResponse c || isNoDefender c || isLabel "$label.declineToDefend" c
      answerQuestion you (fromMaybe 0 $ headMay [i | (i,c) <- zip [0..] (choicesOf q), passing c])
      go (n-1)
    Nothing -> pure ()

-- Reach the named interactive step while declining optional responses. Forced
-- effects, including Illusion itself, remain in their normal game windows.
advanceTo :: (Choice -> Bool) -> GameAppT ()
advanceTo wanted = go (100 :: Int)
 where
  go 0 = error "Psylocke fixture did not reach its choice"
  go n = do
    reached <- offers wanted
    unless reached $ openQuestion >>= \case
      Just (you, q) -> do
        let passing c = isNoResponse c || isNoDefender c || isLabel "$label.declineToDefend" c
        answerQuestion you (fromMaybe 0 $ headMay [i | (i,c) <- zip [0..] (choicesOf q), passing c])
        go (n-1)
      Nothing -> error "Psylocke fixture ended before the expected choice"

perform :: L.QueueT GameAppT () -> GameAppT ()
perform action = L.runQueueT (L.capture action) >>= resolve

attachmentIdOf :: EncounterCard -> AttachmentId
attachmentIdOf = AttachmentId . unCardId . ecCardId

attachmentPresent :: EncounterCard -> GameAppT Bool
attachmentPresent card = getsGame (HashMap.member (attachmentIdOf card) . gameAttachments)

obligationIdOf :: EncounterCard -> ObligationId
obligationIdOf = ObligationId . unCardId . ecCardId

asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass >> clearHand p

flipWeapons :: GameAppT ()
flipWeapons = do
  knives <- upgradesWith "41002a"
  for_ knives $ \u -> perform (L.flipPsiEnergy $ upgradeId u)

boostDragon :: GameAppT ()
boostDragon = do
  vid <- villainIdNow
  card <- encounterCard Treacheries.telekineticDragon
  runNow [RevealBoostCard card (EnemyVillainId vid)]

basicAttackVillain :: IdentityId -> GameAppT ()
basicAttackVillain p = do
  choose "make a basic attack" (isHeroAttack p)
  pass
  vid <- villainIdNow
  choose "attack the villain" (isTargetChoice vid)
  advanceTo (isLabel "$label.mental")

spec :: Spec
spec = describe "Psylocke nemesis" do
  describe "Body Swapped" do
    it "belongs to Betsy when another player reveals it, flips and exhausts both weapons, and locks Katana" do
      tg <- testGame ["psylocke", "spider-man"]
      let p = player1 tg
          q = player2 tg
      inGame tg do
        card <- revealNow q Obligations.bodySwapped
        settle
        owner <- getsGame (fmap (Obligation.obligationOwner . toAttrs) . HashMap.lookup (obligationIdOf card) . gameObligations)
        owner `shouldBe` Just (Just p)
        knives <- upgradesWith "41002a"
        knives `shouldBe` []
        katanas <- upgradesWith "41002b"
        length katanas `shouldBe` 2
        for_ katanas $ \u -> do
          upgradeIsExhausted (upgradeId u) `shouldReturn` True
          perform (L.flipPsiEnergy $ upgradeId u)
        length <$> upgradesWith "41002b" `shouldReturn` 2
        isExhaustedNow q `shouldReturn` False
        obligationsInPlay `shouldReturn` 1

    it "discards a Psionic card as the alter-ego action cost, discards itself, and unlocks the weapons" do
      tg <- testGame ["psylocke"]
      let p = player1 tg
      inGame tg do
        clearHand p
        payment <- putInHand p "41005"
        card <- revealNow p Obligations.bodySwapped
        settle
        choose "discard Body Swapped" (isAbilityOf $ ObligationRef $ obligationIdOf card)
        choose "discard a Psionic card" (isTargetChoice payment)
        settle
        obligationsInPlay `shouldReturn` 0
        discardOf p >>= (`shouldSatisfy` elem payment)
        encounterDiscardNow >>= (`shouldSatisfy` elem card)
        katanas <- upgradesWith "41002b"
        for_ katanas $ \u -> perform (L.flipPsiEnergy $ upgradeId u)
        length <$> upgradesWith "41002a" `shouldReturn` 2

    it "offers no discard action without a Psionic card and none in hero form" do
      tg <- testGame ["psylocke"]
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- putInHand p "01088"
        card <- revealNow p Obligations.bodySwapped
        settle
        offers (isAbilityOf $ ObligationRef $ obligationIdOf card) `shouldReturn` False
        _ <- putInHand p "41005"
        offers (isAbilityOf $ ObligationRef $ obligationIdOf card) `shouldReturn` True
        asHero p
        _ <- putInHand p "41005"
        offers (isAbilityOf $ ObligationRef $ obligationIdOf card) `shouldReturn` False

  describe "Chimera" do
    it "adds printed mental resources, including both Knife faces, to a scheme activation and resets afterward" do
      tg <- testGame ["psylocke"]
      let p = player1 tg
      inGame tg do
        _ <- putIntoPlay p "41024"
        _ <- putIntoPlay p "41031"
        mid <- addMinion p Minions.chimera
        before <- mainSchemeThreatNow
        runNow [MinionMessage mid MinionSchemes]
        settle
        mainSchemeThreatNow `shouldReturn` before + 4
        attrs <- getsGame (maybe (error "Chimera missing") toAttrs . HashMap.lookup mid . gameMinions)
        Minion.getModifiedAttack attrs `shouldReturn` 1
        Minion.getModifiedScheme attrs `shouldReturn` 1

    it "uses the activated-against player's cards rather than cards controlled by another player, and wild does not count" do
      tg <- testGame ["psylocke", "spider-man"]
      let p = player1 tg
          q = player2 tg
      inGame tg do
        asHero p
        _ <- putIntoPlay p "41031"
        _ <- putIntoPlay q "41024"
        mid <- addMinion p Minions.chimera
        runNow [MinionMessage mid $ MinionAttacks p]
        settle
        damageOf p `shouldReturn` 3
        damageOf q `shouldReturn` 0

    it "counts Katana's physical face as zero mental symbols" do
      tg <- testGame ["psylocke"]
      let p = player1 tg
      inGame tg do
        flipWeapons
        mid <- addMinion p Minions.chimera
        before <- mainSchemeThreatNow
        runNow [MinionMessage mid MinionSchemes]
        settle
        mainSchemeThreatNow `shouldReturn` before + 1

  describe "Interdimensional Plunder" do
    it "starts with two flat threat and a crisis icon, then counts every player's upgrades including permanents" do
      tg <- testGame ["psylocke", "spider-man"]
      let p = player1 tg
          q = player2 tg
      inGame tg do
        _ <- putIntoPlay p "41024"
        _ <- putIntoPlay q "41033"
        n <- getsGame (fromIntegral . length . gameUpgrades)
        card <- revealNow p Schemes.interdimensionalPlunder
        settle
        sideSchemeThreat (sideSchemeOf card) `shouldReturn` 2 + n
        crisis <- getsGame (fmap (SideScheme.sideSchemeCrisis . toAttrs) . HashMap.lookup (sideSchemeOf card) . gameSideSchemes)
        crisis `shouldBe` Just True

  describe "Psionic Illusion" do
    it "keeps the original attack and attachment when the discarded card has the named printed resource" do
      tg <- testGame ["psylocke"]
      let p = player1 tg
      inGame tg do
        asHero p
        card <- revealNow p Attachments.psionicIllusion
        settle
        discarded <- stackDeck p ["41005"]
        basicAttackVillain p
        choose "name mental" (isLabel "$label.mental")
        settle
        villainDamageNow `shouldReturn` 1
        damageOf p `shouldReturn` 0
        attachmentPresent card `shouldReturn` True
        discardOf p >>= (`shouldSatisfy` elem (testHead discarded))

    it "redirects a failed basic attack to the attacking hero and discards itself; wild is not mental" do
      tg <- testGame ["psylocke"]
      let p = player1 tg
      inGame tg do
        asHero p
        card <- revealNow p Attachments.psionicIllusion
        settle
        _ <- stackDeck p ["41020"]
        basicAttackVillain p
        choose "name mental" (isLabel "$label.mental")
        choose "attack Psylocke instead" (isTargetChoice p)
        settle
        villainDamageNow `shouldReturn` 0
        damageOf p `shouldReturn` 1
        attachmentPresent card `shouldReturn` False
        encounterDiscardNow >>= (`shouldSatisfy` elem card)

    it "allows naming wild and recognizes a printed wild resource" do
      tg <- testGame ["psylocke"]
      let p = player1 tg
      inGame tg do
        asHero p
        card <- revealNow p Attachments.psionicIllusion
        settle
        _ <- stackDeck p ["41020"]
        basicAttackVillain p
        choose "name wild" (isLabel "$label.wild")
        settle
        villainDamageNow `shouldReturn` 1
        attachmentPresent card `shouldReturn` True

    it "preserves event damage when redirected to an ally, leaving later Flurry choices independent" do
      tg <- testGame ["psylocke"]
      let p = player1 tg
      inGame tg do
        asHero p
        ally <- putIntoPlay p "01084"
        card <- revealNow p Attachments.psionicIllusion
        settle
        event <- putInHand p "41004"
        payment <- traverse (putInHand p) ["01088", "01089"]
        _ <- stackDeck p ["41020", "41005"]
        playWith event payment
        vid <- villainIdNow
        choose "first Flurry target" (isTargetChoice vid)
        advanceTo (isLabel "$label.mental")
        choose "name mental" (isLabel "$label.mental")
        choose "redirect damage to Nick Fury" (isTargetChoice $ allyIdOf ally)
        advanceTo (isTargetChoice vid)
        choose "first knife confuses villain" (isTargetChoice vid)
        settle
        villainDamageNow `shouldReturn` 0
        allyDamage <$> allyIn "01084" `shouldReturn` 2
        attachmentPresent card `shouldReturn` False
        (villainConfused . toAttrs <$> getVillain) `shouldReturn` True
        cards <- discardOf p
        length (filter ((== "41020") . toCardCode) cards) `shouldBe` 1

    it "checks each distinct Flurry target, including confusion-only targets, but not a repeated target" do
      tg <- testGame ["psylocke"]
      let p = player1 tg
      inGame tg do
        asHero p
        mid <- addMinion p Minions.whiplash
        card <- revealNow p Attachments.psionicIllusion
        settle
        event <- putInHand p "41004"
        payment <- traverse (putInHand p) ["01088", "01089"]
        tops <- stackDeck p ["41005", "41005", "01088"]
        playWith event payment
        vid <- villainIdNow
        choose "damage villain" (isTargetChoice vid)
        advanceTo (isLabel "$label.mental")
        choose "match the initial target's resource" (isLabel "$label.mental")
        choose "confuse Whiplash" (isTargetChoice mid)
        advanceTo (isLabel "$label.mental")
        choose "match the new confusion-only target's resource" (isLabel "$label.mental")
        choose "choose Whiplash again without another check" (isTargetChoice mid)
        settle
        villainDamageNow `shouldReturn` 2
        damageOf p `shouldReturn` 1
        attachmentPresent card `shouldReturn` True
        discarded <- discardOf p
        discarded `shouldSatisfy` elem (tops !! 0)
        discarded `shouldSatisfy` elem (tops !! 1)
        discarded `shouldSatisfy` notElem (tops !! 2)

    it "redirects Psi-Flail Strike's damage and stun to the same friendly target" do
      tg <- testGame ["psylocke"]
      let p = player1 tg
      inGame tg do
        asHero p
        mid <- addMinion p Minions.weaponsRunner
        card <- revealNow p Attachments.psionicIllusion
        settle
        event <- putInHand p "41032"
        payment <- putInHand p "01089"
        _ <- stackDeck p ["41020"]
        runNow [MinionMessage mid $ MinionAttacks p]
        advanceTo isDefend
        choose "Psylocke defends" isDefend
        advanceTo (isPlay event)
        playWith event [payment]
        advanceTo (isLabel "$label.mental")
        choose "name mental and miss" (isLabel "$label.mental")
        choose "redirect the strike to Psylocke" (isTargetChoice p)
        settle
        damageOf p `shouldReturn` 3
        isStunned p `shouldReturn` True
        minionDamageOf mid `shouldReturn` Just 0
        stunned <- getsGame (fmap (Minion.minionStunned . toAttrs) . HashMap.lookup mid . gameMinions)
        stunned `shouldBe` Just False
        attachmentPresent card `shouldReturn` False

    for_ [False, True] $ \physical ->
      it ("redirects Concussive Blow's confusion with " <> (if physical then "physical damage" else "no physical payment")) do
        tg <- testGame ["psylocke"]
        let p = player1 tg
        inGame tg do
          asHero p
          card <- revealNow p Attachments.psionicIllusion
          settle
          event <- putInHand p "41014"
          payment <- traverse (putInHand p) [if physical then "01090" else "01088", "01089"]
          _ <- stackDeck p ["41020"]
          playWith event payment
          vid <- villainIdNow
          choose "Concussive Blow targets the villain" (isTargetChoice vid)
          advanceTo (isLabel "$label.mental")
          choose "name mental and miss" (isLabel "$label.mental")
          choose "redirect the confusion to Psylocke" (isTargetChoice p)
          settle
          isConfused p `shouldReturn` True
          damageOf p `shouldReturn` (if physical then 3 else 0)
          villainDamageNow `shouldReturn` 0
          (villainConfused . toAttrs <$> getVillain) `shouldReturn` False
          attachmentPresent card `shouldReturn` False

    for_ [False, True] $ \allMatch ->
      it ("resolves simultaneous Dive Bomb target checks " <> if allMatch then "for every successful target" else "only until Illusion is discarded") do
        tg <- testGame ["psylocke"]
        let p = player1 tg
        inGame tg do
          asHero p
          _ <- putIntoPlay p "01017"
          first <- addMinion p Minions.chimera
          second <- addMinion p Minions.radioactiveMan
          card <- revealNow p Attachments.psionicIllusion
          settle
          event <- putInHand p "17028"
          payment <- traverse (putInHand p) ["01088", "01089"]
          tops <- stackDeck p ["41005", if allMatch then "41005" else "41020", "41005"]
          playWith event payment
          vid <- villainIdNow
          choose "Dive Bomb hits the villain first" (isTargetChoice vid)
          advanceTo (isLabel "$label.mental")
          choose "match the initial target's resource" (isLabel "$label.mental")
          advanceTo (isLabel "$label.mental")
          choose "check the first simultaneous target" (isLabel "$label.mental")
          if allMatch
            then do
              advanceTo (isLabel "$label.mental")
              choose "check the second simultaneous target" (isLabel "$label.mental")
            else do
              choose "redirect the failed target to Psylocke" (isTargetChoice p)
              offers (isLabel "$label.mental") `shouldReturn` False
          settle
          villainDamageNow `shouldReturn` 7
          damageOf p `shouldReturn` (if allMatch then 0 else 1)
          firstDamage <- fromMaybe 0 <$> minionDamageOf first
          secondDamage <- fromMaybe 0 <$> minionDamageOf second
          firstDamage + secondDamage `shouldBe` (if allMatch then 2 else 1)
          attachmentPresent card `shouldReturn` allMatch
          discarded <- discardOf p
          discarded `shouldSatisfy` elem (tops !! 0)
          discarded `shouldSatisfy` elem (tops !! 1)
          discarded `shouldSatisfy` (if allMatch then elem else notElem) (tops !! 2)

    it "lets Superhuman Strength stun a friendly target after She-Hulk's redirected attack, and pays its discard cost" do
      tg <- testGame ["she-hulk"]
      let p = player1 tg
      inGame tg do
        asHero p
        strength <- putIntoPlay p "01028"
        card <- revealNow p Attachments.psionicIllusion
        settle
        _ <- stackDeck p ["41020"]
        choose "She-Hulk makes a basic attack" (isHeroAttack p)
        vid <- villainIdNow
        choose "attack the villain" (isTargetChoice vid)
        advanceTo (isLabel "$label.mental")
        choose "name mental and miss" (isLabel "$label.mental")
        choose "redirect She-Hulk's attack to herself" (isTargetChoice p)
        advanceTo (isAbilityOfCard strength)
        choose "resolve Superhuman Strength" (isAbilityOfCard strength)
        settle
        damageOf p `shouldReturn` 5
        isStunned p `shouldReturn` True
        villainDamageNow `shouldReturn` 0
        getsGame (HashMap.member (upgradeOf strength) . gameUpgrades) `shouldReturn` False
        discardOf p >>= (`shouldSatisfy` elem strength)
        attachmentPresent card `shouldReturn` False

  describe "Telekinetic Dragon" do
    it "deals indirect damage equal to mental symbols, ignoring wild symbols and other players' cards" do
      tg <- testGame ["psylocke", "spider-man"]
      let p = player1 tg
          q = player2 tg
      inGame tg do
        _ <- putIntoPlay p "41031"
        _ <- putIntoPlay q "41024"
        _ <- revealNow p Treacheries.telekineticDragon
        advanceTo (isTargetChoice p)
        choose "assign first indirect damage to Psylocke" (isTargetChoice p)
        choose "assign second indirect damage to Psylocke" (isTargetChoice p)
        settle
        damageOf p `shouldReturn` 2
        damageOf q `shouldReturn` 0
        allyDamage <$> allyIn "41031" `shouldReturn` 0

    it "surges when the player controls zero mental symbols" do
      tg <- testGame ["psylocke"]
      let p = player1 tg
      inGame tg do
        flipWeapons
        _ <- setEncounterDeck [Treacheries.advance] []
        before <- length <$> dealtTo p
        _ <- revealNow p Treacheries.telekineticDragon
        settle
        damageOf p `shouldReturn` 0
        (length <$> dealtTo p) `shouldReturn` before + 1

    it "offers spending a mental resource to avoid the boost's confusion" do
      tg <- testGame ["psylocke"]
      let p = player1 tg
      inGame tg do
        clearHand p
        payment <- putInHand p "01089"
        boostDragon
        advanceTo (isLabel "$label.spendMentalResource")
        choose "spend mental" (isLabel "$label.spendMentalResource")
        payWith [payment]
        settle
        isConfused p `shouldReturn` False
        discardOf p >>= (`shouldSatisfy` elem payment)

    it "confuses the identity when the boost resource is declined or unavailable" do
      tg <- testGame ["psylocke"]
      let p = player1 tg
      inGame tg do
        clearHand p
        boostDragon
        advanceTo (isLabel "$label.confuseYourIdentity")
        offers (isLabel "$label.spendMentalResource") `shouldReturn` False
        choose "accept confusion" (isLabel "$label.confuseYourIdentity")
        settle
        isConfused p `shouldReturn` True
