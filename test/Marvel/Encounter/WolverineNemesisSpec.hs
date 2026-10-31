module Marvel.Encounter.WolverineNemesisSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Attachment.Types qualified as Attachment
import Marvel.Minion.Cards qualified as Minions
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Status
import Marvel.Window qualified as W
import TestImport

settle :: GameAppT ()
settle = go (100 :: Int)
 where
  go 0 = error "Wolverine encounter fixture did not settle"
  go n = openQuestion >>= \case
    Just (_, QuestionLabel "$label.yourTurn" _ _) -> pure ()
    Just (you, q) -> do
      let cs = choicesOf q
          passing c = isNoResponse c || isNoDefender c || isLabel "$label.declineToDefend" c
      answerQuestion you (fromMaybe 0 $ headMay [i | (i, c) <- zip [0 ..] cs, passing c])
      go (n - 1)
    Nothing -> pure ()

attachmentIdOf :: EncounterCard -> AttachmentId
attachmentIdOf = AttachmentId . unCardId . ecCardId

attachmentPresent :: EncounterCard -> GameAppT Bool
attachmentPresent card = getsGame (HashMap.member (attachmentIdOf card) . gameAttachments)

boostNow :: CardDef -> GameAppT ()
boostNow def = do
  vid <- villainIdNow
  card <- encounterCard def
  runNow [RevealBoostCard card (EnemyVillainId vid)]
  settle

spec :: Spec
spec = describe "Wolverine nemesis and Lady Deathstrike modular" do
  describe "Past Demons" do
    it "allows Logan to exhaust and remove the obligation" do
      tg <- testGame ["wolverine"]
      inGame tg do
        let p = player1 tg
        _ <- revealNow p Obligations.pastDemons
        choose "exhaust Logan" (isLabel "$label.exhaustLoganRemovePastDemons")
        isExhaustedNow p `shouldReturn` True
        encounterDiscardCodes >>= (`shouldSatisfy` notElem "35027")
        isStunned p `shouldReturn` False

    it "offers the optional flip before exhausting Logan" do
      tg <- testGame ["wolverine"]
      inGame tg do
        let p = player1 tg
        changeForm p
        pass
        _ <- revealNow p Obligations.pastDemons
        choose "flip to Logan" (isLabel "$label.changeToB")
        choose "exhaust Logan" (isLabel "$label.exhaustLoganRemovePastDemons")
        isExhaustedNow p `shouldReturn` True
        isStunned p `shouldReturn` False

    it "stuns and confuses when the player declines to exhaust" do
      tg <- testGame ["wolverine"]
      inGame tg do
        let p = player1 tg
        _ <- revealNow p Obligations.pastDemons
        choose "accept statuses" (isLabel "$label.pastDemonsStunConfuse")
        isStunned p `shouldReturn` True
        isConfused p `shouldReturn` True
        encounterDiscardCodes >>= (`shouldSatisfy` elem "35027")

    it "does not offer the exhaust choice when Logan is already exhausted" do
      tg <- testGame ["wolverine"]
      inGame tg do
        let p = player1 tg
        resolve [IdentityMessage p ExhaustedIdentity]
        _ <- revealNow p Obligations.pastDemons
        offers (isLabel "$label.exhaustLoganRemovePastDemons") `shouldReturn` False
        choose "accept statuses" (isLabel "$label.pastDemonsStunConfuse")

  describe "Omega Red and The Carbonadium Synthesizer" do
    it "damages the attacked player's identity and allies before attack damage" do
      tg <- testGame ["wolverine", "spider-man"]
      inGame tg do
        let p = player1 tg
        _ <- putIntoPlay p "01067"
        _ <- putIntoPlay (player2 tg) "01020"
        mid <- addMinion p Minions.omegaRed
        changeForm p
        pass
        runNow [MinionMessage mid $ MinionAttacks p]
        settle
        damageOf p `shouldReturn` 3
        damageOf (player2 tg) `shouldReturn` 0
        alliesWith "01020" >>= \case
          [a] -> allyDamage a `shouldBe` 0
          _ -> error "Hellcat missing"
        alliesWith "01067" >>= \case
          [a] -> allyDamage a `shouldBe` 1
          _ -> error "Tigra missing"

    it "applies damage to all characters before a response to any of that damage" do
      tg <- testGame ["spider-ham"]
      inGame tg do
        let p = player1 tg
        _ <- putIntoPlay p "01067"
        _ <- putIntoPlay p "01020"
        mid <- addMinion p Minions.omegaRed
        changeForm p
        pass
        clearHand p
        runNow [MinionMessage mid $ MinionAttacks p]
        choose "Omega Red's forced interrupt" (isAbilityOf $ MinionRef mid)
        offers (isNamedAbility "$label.spiderNonsense") `shouldReturn` True
        damageOf p `shouldReturn` 1
        for_ ["01067", "01020"] \code -> alliesWith code >>= \case
          [a] -> allyDamage a `shouldBe` 1
          _ -> error "Omega Red damage fixture ally missing"
        choose "respond to damage" (isNamedAbility "$label.spiderNonsense")
        settle
        damageOf p `shouldReturn` 3

    it "does not trigger the attack interrupt when stun replaces the attack" do
      tg <- testGame ["wolverine"]
      inGame tg do
        let p = player1 tg
        mid <- addMinion p Minions.omegaRed
        resolve [GiveStatus (MinionRef mid) StunnedStatus, GiveStatus (MinionRef mid) StunnedStatus]
        runNow [MinionMessage mid $ MinionAttacks p]
        settle
        damageOf p `shouldReturn` 0

    it "prevents lethal defeat and overkill until the synthesizer is gone" do
      tg <- testGame ["wolverine"]
      inGame tg do
        let p = player1 tg
        scheme <- revealNow p Schemes.theCarbonadiumSynthesizer
        settle
        mid <- addMinion p Minions.omegaRed
        before <- villainDamageNow
        resolve [MinionMessage mid $ MinionDamaged (IdentityRef p) $ withOverkill $ toDamage 12 $ FromPlayerAttack p]
        minionDamageOf mid `shouldReturn` Just 12
        villainDamageNow `shouldReturn` before
        changeForm p
        pass
        resolve [SideSchemeMessage (sideSchemeOf scheme) $ SideSchemeThreatRemoved (IdentityRef p) 2]
        track <- putInHand p "35011"
        resource <- putInHand p "01088"
        playWith track [resource]
        choose "clear the synthesizer" (isTargetChoice $ sideSchemeOf scheme)
        settle
        minionDamageOf mid `shouldReturn` Nothing

    it "retaliates after a basic attack and requires two stun cards because it is steady" do
      tg <- testGame ["wolverine"]
      inGame tg do
        let p = player1 tg
        mid <- addMinion p Minions.omegaRed
        resolve [GiveStatus (MinionRef mid) StunnedStatus]
        changeForm p
        pass
        clearHand p
        choose "basic attack" (isHeroAttack p)
        choose "attack Omega Red" (isTargetChoice mid)
        pass
        damageOf p `shouldReturn` 1
        minionDamageOf mid `shouldReturn` Just 2

  describe "Death Factor" do
    it "attaches to its revealing identity and damages only after that player's turn" do
      tg <- testGame ["wolverine", "spider-man"]
      inGame tg do
        let p = player1 tg
        card <- revealNow p Attachments.deathFactor
        settle
        runNow [CheckWindows [W.Window W.After $ W.TurnEnded $ player2 tg]]
        settle
        damageOf p `shouldReturn` 0
        runNow [CheckWindows [W.Window W.After $ W.TurnEnded p]]
        settle
        damageOf p `shouldReturn` 1
        attachmentPresent card `shouldReturn` True

    it "can replace basic recovery healing while retaining its exhaustion" do
      tg <- testGame ["wolverine"]
      inGame tg do
        let p = player1 tg
        setDamage p 4
        card <- revealNow p Attachments.deathFactor
        settle
        choose "basic recovery" $ \case UseAbility a -> abilitySource a == IdentityRef p && abilityIndex a == 200; _ -> False
        choose "discard Death Factor" (isAbilityOf $ AttachmentRef $ attachmentIdOf card)
        settle
        damageOf p `shouldReturn` 4
        isExhaustedNow p `shouldReturn` True
        attachmentPresent card `shouldReturn` False

    it "still opens after-recovery responses when its interrupt replaces the healing" do
      tg <- testGame ["spider-ham"]
      inGame tg do
        let p = player1 tg
        clearHand p
        setDamage p 4
        card <- revealNow p Attachments.deathFactor
        settle
        before <- playerIdentityCounters <$> playerAttrs p
        choose "basic recovery" $ \case UseAbility a -> abilitySource a == IdentityRef p && abilityIndex a == 200; _ -> False
        choose "discard Death Factor" (isAbilityOf $ AttachmentRef $ attachmentIdOf card)
        offers (isNamedAbility "$label.cartoonPower") `shouldReturn` True
        choose "Cartoon Power" (isNamedAbility "$label.cartoonPower")
        settle
        damageOf p `shouldReturn` 4
        (playerIdentityCounters <$> playerAttrs p) `shouldReturn` (before + 1)
        isExhaustedNow p `shouldReturn` True

    it "allows declining the interrupt to heal normally" do
      tg <- testGame ["wolverine"]
      inGame tg do
        let p = player1 tg
        setDamage p 4
        card <- revealNow p Attachments.deathFactor
        settle
        choose "basic recovery" $ \case UseAbility a -> abilitySource a == IdentityRef p && abilityIndex a == 200; _ -> False
        choose "decline interrupt" isNoResponse
        settle
        damageOf p `shouldReturn` 0
        attachmentPresent card `shouldReturn` True

    it "does not permit basic recovery at full health" do
      tg <- testGame ["wolverine"]
      inGame tg do
        _ <- revealNow (player1 tg) Attachments.deathFactor
        settle
        offers (\case UseAbility a -> abilityIndex a == 200; _ -> False) `shouldReturn` False

  describe "Tentacle Strike" do
    for_ [False, True] $ \boosted -> do
      it (if boosted then "its boost stuns and deals one damage" else "its reveal stuns and deals one damage") do
        tg <- testGame ["wolverine"]
        inGame tg do
          let p = player1 tg
          if boosted then boostNow Treacheries.tentacleStrike else void (revealNow p Treacheries.tentacleStrike) >> settle
          isStunned p `shouldReturn` True
          damageOf p `shouldReturn` 1
      it (if boosted then "its boost deals four damage when already stunned" else "its reveal deals four damage when already stunned") do
        tg <- testGame ["wolverine"]
        inGame tg do
          let p = player1 tg
          resolve [GiveStatus (IdentityRef p) StunnedStatus]
          if boosted then boostNow Treacheries.tentacleStrike else void (revealNow p Treacheries.tentacleStrike) >> settle
          isStunned p `shouldReturn` True
          damageOf p `shouldReturn` 4

  describe "Lady Deathstrike and Seeking Vengeance" do
    it "quickstrikes and discards a random card after damaging the hero" do
      tg <- testGame ["wolverine"]
      inGame tg do
        let p = player1 tg
        changeForm p
        pass
        clearHand p
        card <- putInHand p "01088"
        _ <- revealNow p Minions.ladyDeathstrike
        settle
        damageOf p `shouldReturn` 2
        handOf p >>= (`shouldSatisfy` notElem card)

    it "does not discard a card when its attack deals no damage" do
      tg <- testGame ["wolverine"]
      inGame tg do
        let p = player1 tg
        changeForm p
        pass
        clearHand p
        card <- putInHand p "01088"
        resolve [GiveStatus (IdentityRef p) ToughStatus]
        _ <- revealNow p Minions.ladyDeathstrike
        settle
        damageOf p `shouldReturn` 0
        handOf p >>= (`shouldSatisfy` elem card)

    it "finds Lady Deathstrike in the encounter discard and puts her into play" do
      tg <- testGame ["wolverine"]
      inGame tg do
        let p = player1 tg
        _ <- setEncounterDeck [Treacheries.assault] [Minions.ladyDeathstrike]
        _ <- revealNow p Schemes.seekingVengeance
        settle
        getsGame (length . filter ((== "35034") . toCardCode . getCardDef) . HashMap.elems . gameMinions) `shouldReturn` 1

    it "activates an existing Lady Deathstrike against the revealing hero" do
      tg <- testGame ["wolverine"]
      inGame tg do
        let p = player1 tg
        _ <- addMinion p Minions.ladyDeathstrike
        changeForm p
        pass
        _ <- revealNow p Schemes.seekingVengeance
        settle
        damageOf p `shouldReturn` 2

    it "schemes instead when the revealing player is in alter-ego" do
      tg <- testGame ["wolverine"]
      inGame tg do
        let p = player1 tg
        _ <- addMinion p Minions.ladyDeathstrike
        before <- mainSchemeThreat
        _ <- revealNow p Schemes.seekingVengeance
        settle
        mainSchemeThreat `shouldReturn` (before + 1)
        damageOf p `shouldReturn` 0

    it "waits for Lady Deathstrike's current attack before activating her again" do
      tg <- testGame ["wolverine"]
      inGame tg do
        let p = player1 tg
        mid <- addMinion p Minions.ladyDeathstrike
        changeForm p
        pass
        clearHand p
        liftIO $ writeIORef tg.ran []
        runNow [MinionMessage mid $ MinionAttacks p]
        offers isNoDefender `shouldReturn` True
        _ <- revealNow p Schemes.seekingVengeance
        settle
        damageOf p `shouldReturn` 4
        ran <- reverse <$> liftIO (readIORef tg.ran)
        let positions message = [i | (i, text) <- zip [0 :: Int ..] ran, text == tshow message]
        case (positions (MinionMessage mid $ MinionAttacks p), positions (MinionMessage mid MinionEndAttack)) of
          ([firstStart, secondStart], [firstEnd, secondEnd]) -> do
            firstStart `shouldSatisfy` (< firstEnd)
            firstEnd `shouldSatisfy` (< secondStart)
            secondStart `shouldSatisfy` (< secondEnd)
          _ -> error "expected two complete Lady Deathstrike attacks"

    it "schemes against its revealing alter-ego even when engaged with another player" do
      tg <- testGame ["wolverine", "spider-man"]
      inGame tg do
        let p = player1 tg
            other = player2 tg
        mid <- addMinion other Minions.ladyDeathstrike
        liftIO $ writeIORef tg.ran []
        _ <- revealNow p Schemes.seekingVengeance
        settle
        ran <- liftIO $ readIORef tg.ran
        for_ [W.When, W.After] \timing -> do
          let against you = tshow $ CheckWindows [W.Window timing $ W.EnemyActivation (EnemyMinionId mid) you]
          ran `shouldSatisfy` elem (against p)
          ran `shouldSatisfy` notElem (against other)

    it "finds Lady Deathstrike in the encounter deck as well as the discard" do
      tg <- testGame ["wolverine"]
      inGame tg do
        let p = player1 tg
        _ <- setEncounterDeck [Minions.ladyDeathstrike, Treacheries.assault] []
        _ <- revealNow p Schemes.seekingVengeance
        settle
        getsGame (length . filter ((== "35034") . toCardCode . getCardDef) . HashMap.elems . gameMinions) `shouldReturn` 1

    it "discards from an ally defender's owner when that ally takes attack damage" do
      tg <- testGame ["wolverine"]
      inGame tg do
        let p = player1 tg
        ally <- putIntoPlay p "01067"
        mid <- addMinion p Minions.ladyDeathstrike
        changeForm p
        pass
        clearHand p
        card <- putInHand p "01088"
        runNow [MinionMessage mid $ MinionAttacks p]
        choose "defend with Tigra" (\case AllyDefend aid _ -> aid == allyIdOf ally; _ -> False)
        settle
        handOf p >>= (`shouldSatisfy` notElem card)

    it "does not discard a player's card when Lady Deathstrike damages scenario-owned Robert Kelly" do
      tg <- testGame ["wolverine"]
      inGame tg do
        let p = player1 tg
        kelly <- putIntoPlay p "32066"
        mid <- addMinion p Minions.ladyDeathstrike
        changeForm p
        pass
        clearHand p
        card <- putInHand p "01088"
        runNow [MinionMessage mid $ MinionAttacks p]
        choose "Robert Kelly defends" (\case AllyDefend aid _ -> aid == allyIdOf kelly; _ -> False)
        settle
        handOf p >>= (`shouldSatisfy` elem card)
        alliesWith "32066" >>= \case
          [a] -> allyDamage a `shouldBe` 2
          _ -> error "Robert Kelly missing after surviving Lady Deathstrike's attack"
        damageOf p `shouldReturn` 0

  describe "Adamantium Upgrades" do
    it "can attach to a minion and gives it two ATK and piercing" do
      tg <- testGame ["wolverine"]
      inGame tg do
        let p = player1 tg
        mid <- addMinion p Minions.hydraBomber
        changeForm p
        pass
        card <- revealNow p Attachments.adamantiumUpgrades
        choose "attach to the bomber" (isTargetChoice mid)
        settle
        setDamage p 0
        resolve [GiveStatus (IdentityRef p) ToughStatus]
        runNow [MinionMessage mid $ MinionAttacks p]
        settle
        damageOf p `shouldReturn` 3
        attachmentPresent card `shouldReturn` True

    it "does not offer an enemy that already has a copy attached" do
      tg <- testGame ["wolverine"]
      inGame tg do
        let p = player1 tg
        vid <- villainIdNow
        _ <- revealNow p Attachments.adamantiumUpgrades
        settle
        mid <- addMinion p Minions.hydraBomber
        _ <- revealNow p Attachments.adamantiumUpgrades
        offers (isTargetChoice vid) `shouldReturn` False
        choose "attach elsewhere" (isTargetChoice mid)
        settle

    it "surges when every enemy already has a copy attached" do
      tg <- testGame ["wolverine"]
      inGame tg do
        let p = player1 tg
        _ <- revealNow p Attachments.adamantiumUpgrades
        settle
        _ <- setEncounterDeck [Treacheries.advance, Treacheries.assault] []
        before <- length <$> dealtTo p
        card <- revealNow p Attachments.adamantiumUpgrades
        settle
        attachmentPresent card `shouldReturn` False
        after <- dealtTo p
        length after `shouldBe` (before + 1)
        map toCardCode after `shouldSatisfy` elem (toCardCode Treacheries.advance)

    it "can be discarded by paying the three different resources" do
      tg <- testGame ["wolverine"]
      inGame tg do
        let p = player1 tg
        card <- revealNow p Attachments.adamantiumUpgrades
        settle
        changeForm p
        pass
        energy <- putInHand p "01088"
        mental <- putInHand p "01089"
        physical <- putInHand p "01090"
        choose "remove attachment" (isAbilityOf $ AttachmentRef $ attachmentIdOf card)
        payWith [energy, mental, physical]
        settle
        attachmentPresent card `shouldReturn` False

  describe "Hack 'n' Slash" do
    for_ [False, True] $ \boosted ->
      it (if boosted then "its boost discards randomly and counts printed resources" else "its reveal discards randomly and counts printed resources") do
        tg <- testGame ["wolverine"]
        inGame tg do
          let p = player1 tg
          clearHand p
          card <- putInHand p "01088"
          if boosted then boostNow Treacheries.hackNSlash else void (revealNow p Treacheries.hackNSlash) >> settle
          handOf p >>= (`shouldSatisfy` notElem card)
          damageOf p `shouldReturn` 2

    it "does no damage when the hand is empty" do
      tg <- testGame ["wolverine"]
      inGame tg do
        clearHand (player1 tg)
        _ <- revealNow (player1 tg) Treacheries.hackNSlash
        settle
        damageOf (player1 tg) `shouldReturn` 0
