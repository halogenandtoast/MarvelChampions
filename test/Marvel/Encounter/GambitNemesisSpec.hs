module Marvel.Encounter.GambitNemesisSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Attachment.Types qualified as Attachment
import Marvel.Minion.Cards qualified as Minions
import Marvel.Minion.Types qualified as Minion
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.Obligation.Types qualified as Obligation
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Damage
import Marvel.Status
import TestImport

settle :: GameAppT ()
settle = go (150 :: Int)
 where
  go 0 = error "Gambit encounter fixture did not settle"
  go n = openQuestion >>= \case
    Just (_, QuestionLabel "$label.yourTurn" _ _) -> pure ()
    Just (you, q) -> do
      let passing c = isNoResponse c || isNoDefender c || isLabel "$label.declineToDefend" c
      answerQuestion you (fromMaybe 0 $ headMay [i | (i,c) <- zip [0..] (choicesOf q), passing c])
      go (n-1)
    Nothing -> pure ()

attachmentIdOf :: EncounterCard -> AttachmentId
attachmentIdOf = AttachmentId . unCardId . ecCardId

attachmentPresent :: EncounterCard -> GameAppT Bool
attachmentPresent card = getsGame (HashMap.member (attachmentIdOf card) . gameAttachments)

attachShield :: IdentityId -> MinionId -> GameAppT EncounterCard
attachShield p mid = do
  card <- revealNow p Attachments.psionicShield
  settle
  target <- getsGame $ fmap (Attachment.attachmentEnemy . toAttrs) . HashMap.lookup (attachmentIdOf card) . gameAttachments
  target `shouldBe` Just (Just $ EnemyMinionId mid)
  pure card

spec :: Spec
spec = describe "Gambit nemesis and Exodus modular" do
  describe "Guild Business" do
    it "stays with Remy, contributes its hazard, and is removed by exhausting and spending energy" do
      tg <- testGame ["gambit"]
      inGame tg do
        let p = player1 tg
        card <- revealNow p Obligations.guildBusiness
        settle
        let oid = ObligationId $ unCardId $ ecCardId card
        getsGame (fmap (Obligation.obligationOwner . toAttrs) . HashMap.lookup oid . gameObligations) `shouldReturn` Just (Just p)
        getHazardCount `shouldReturn` 1
        clearHand p
        energy <- putInHand p "01088"
        choose "resolve Guild Business" (isAbilityOf $ ObligationRef oid)
        payWith [energy]
        settle
        isExhaustedNow p `shouldReturn` True
        getsGame (HashMap.member oid . gameObligations) `shouldReturn` False
        encounterDiscardCodes >>= (`shouldSatisfy` notElem "37025")

    it "does not offer its action in hero form" do
      tg <- testGame ["gambit"]
      inGame tg do
        let p = player1 tg
        card <- revealNow p Obligations.guildBusiness
        settle
        changeForm p
        pass
        _ <- putInHand p "01088"
        offers (isAbilityOf $ ObligationRef $ ObligationId $ unCardId $ ecCardId card) `shouldReturn` False

    it "gives the obligation to Gambit when another player reveals it" do
      tg <- testGame ["gambit", "spider-man"]
      inGame tg do
        card <- revealNow (player2 tg) Obligations.guildBusiness
        settle
        let oid = ObligationId $ unCardId $ ecCardId card
        getsGame (fmap (Obligation.obligationOwner . toAttrs) . HashMap.lookup oid . gameObligations) `shouldReturn` Just (Just $ player1 tg)
        resolve [IdentityMessage (player1 tg) ExhaustedIdentity]
        _ <- putInHand (player1 tg) "01088"
        offers (isAbilityOf $ ObligationRef oid) `shouldReturn` False

    it "does not allow another alter-ego to remove Remy's obligation" do
      tg <- testGame ["gambit", "spider-man"]
      inGame tg do
        card <- revealNow (player1 tg) Obligations.guildBusiness
        settle
        _ <- putInHand (player2 tg) "01088"
        endTurn
        offers (isAbilityOf $ ObligationRef $ ObligationId $ unCardId $ ecCardId card) `shouldReturn` False

  describe "Belladonna, Guild Assassin, and The Assassins Guild" do
    it "Belladonna quickstrikes a hero and enters tough" do
      tg <- testGame ["gambit"]
      inGame tg do
        let p = player1 tg
        changeForm p
        pass
        clearHand p
        card <- revealNow p Minions.belladonna
        settle
        damageOf p `shouldReturn` 3
        getsGame (fmap (Minion.minionTough . toAttrs) . HashMap.lookup (minionOf card) . gameMinions) `shouldReturn` Just True

    it "Guild Assassin quickstrikes without toughness" do
      tg <- testGame ["gambit"]
      inGame tg do
        let p = player1 tg
        changeForm p
        pass
        clearHand p
        card <- revealNow p Minions.guildAssassin
        settle
        damageOf p `shouldReturn` 2
        getsGame (fmap (Minion.minionTough . toAttrs) . HashMap.lookup (minionOf card) . gameMinions) `shouldReturn` Just False

    for_ [(Minions.belladonna, 2), (Minions.guildAssassin, 1)] $ \(def, placed) ->
      it ("places its own and guild threat after " <> show (toCardCode def) <> " defeats an ally") do
        tg <- testGame ["gambit"]
        inGame tg do
          let p = player1 tg
          scheme <- revealNow p Schemes.theAssassinsGuild
          settle
          mid <- addMinion p def
          ally <- putIntoPlay p "01067"
          when (placed == 1) $ resolve [AllyMessage (allyIdOf ally) $ AllyDamaged GameRef $ toDamage 1 FromAbility]
          changeForm p
          pass
          before <- mainSchemeThreat
          runNow [MinionMessage mid $ MinionAttacks p]
          choose "defend with Tigra" (\case AllyDefend aid _ -> aid == allyIdOf ally; _ -> False)
          settle
          mainSchemeThreat `shouldReturn` (before + placed)
          sideSchemeThreat (sideSchemeOf scheme) `shouldReturn` 6

    it "does not place defeat threat for a surviving defender" do
      tg <- testGame ["gambit"]
      inGame tg do
        let p = player1 tg
        scheme <- revealNow p Schemes.theAssassinsGuild
        settle
        mid <- addMinion p Minions.guildAssassin
        ally <- putIntoPlay p "01020"
        changeForm p
        pass
        before <- mainSchemeThreat
        runNow [MinionMessage mid $ MinionAttacks p]
        choose "defend with Hellcat" (\case AllyDefend aid _ -> aid == allyIdOf ally; _ -> False)
        settle
        mainSchemeThreat `shouldReturn` before
        sideSchemeThreat (sideSchemeOf scheme) `shouldReturn` 4

    it "places threat after an Assassin defeats an identity" do
      tg <- testGame ["gambit", "spider-man"]
      inGame tg do
        let p = player1 tg
        scheme <- revealNow p Schemes.theAssassinsGuild
        settle
        mid <- addMinion p Minions.guildAssassin
        changeForm p
        pass
        clearHand p
        setDamage p 8
        before <- mainSchemeThreat
        runNow [MinionMessage mid $ MinionAttacks p]
        settle
        mainSchemeThreat `shouldReturn` (before + 1)
        sideSchemeThreat (sideSchemeOf scheme) `shouldReturn` 6

    it "does not add guild threat after a non-Assassin defeats a character" do
      tg <- testGame ["gambit"]
      inGame tg do
        let p = player1 tg
        scheme <- revealNow p Schemes.theAssassinsGuild
        settle
        mid <- addMinion p Minions.shocker
        ally <- putIntoPlay p "01067"
        resolve [AllyMessage (allyIdOf ally) $ AllyDamaged GameRef $ toDamage 1 FromAbility]
        changeForm p
        pass
        runNow [MinionMessage mid $ MinionAttacks p]
        choose "defend with Tigra" (\case AllyDefend aid _ -> aid == allyIdOf ally; _ -> False)
        settle
        sideSchemeThreat (sideSchemeOf scheme) `shouldReturn` 4

    it "counts an Assassin defeating another minion through Moondragon" do
      tg <- testGame ["gambit"]
      inGame tg do
        let p = player1 tg
        scheme <- revealNow p Schemes.theAssassinsGuild
        settle
        assassin <- addMinion p Minions.guildAssassin
        target <- addMinion p Minions.shocker
        resolve [MinionMessage target $ MinionDamaged GameRef $ toDamage 1 FromAbility]
        moon <- putIntoPlay p "19013"
        before <- mainSchemeThreat
        choose "use Moondragon" (\c -> isAbilityOfCard moon c && case c of UseAbility a -> abilityIndex a == 1; _ -> False)
        choose "Assassin attacks" (isTargetChoice assassin)
        choose "attack Shocker" (isTargetChoice target)
        settle
        minionDamageOf target `shouldReturn` Nothing
        mainSchemeThreat `shouldReturn` (before + 1)
        sideSchemeThreat (sideSchemeOf scheme) `shouldReturn` 6

    for_ [False, True] $ \lethal ->
      it (if lethal then "Staff can defeat Moondragon's attacker and cancel its damage" else "Staff interrupts an enemy attacking another enemy") do
        tg <- testGame ["gambit"]
        inGame tg do
          let p = player1 tg
          mid <- addMinion p Minions.shocker
          when lethal $ resolve [MinionMessage mid $ MinionDamaged GameRef $ toDamage 2 FromAbility]
          staff <- putIntoPlay p "37004"
          moon <- putIntoPlay p "19013"
          changeForm p
          pass
          clearHand p
          before <- villainDamageNow
          choose "use Moondragon" (\c -> isAbilityOfCard moon c && case c of UseAbility a -> abilityIndex a == 1; _ -> False)
          choose "Shocker attacks" (isTargetChoice mid)
          chooseVillain
          choose "use Gambit's Staff" (isAbilityOfCard staff)
          settle
          if lethal then do
            minionDamageOf mid `shouldReturn` Nothing
            villainDamageNow `shouldReturn` before
          else do
            minionDamageOf mid `shouldReturn` Just 1
            villainDamageNow `shouldReturn` (before + 2)

  describe "Assassination Attempt" do
    it "makes every Assassin attack the revealer, including in alter-ego and when engaged elsewhere" do
      tg <- testGame ["gambit", "spider-man"]
      inGame tg do
        let p = player1 tg
        _ <- addMinion p Minions.guildAssassin
        _ <- addMinion (player2 tg) Minions.belladonna
        _ <- revealNow p Treacheries.assassinationAttempt
        settle
        damageOf p `shouldReturn` 5
        damageOf (player2 tg) `shouldReturn` 0

    for_ [False, True] $ \inDiscard ->
      it (if inDiscard then "reveals an Assassin found in the discard" else "reveals an Assassin found in the deck") do
        tg <- testGame ["gambit"]
        inGame tg do
          let p = player1 tg
          _ <- if inDiscard then setEncounterDeck [Treacheries.advance] [Minions.guildAssassin]
               else setEncounterDeck [Minions.guildAssassin, Treacheries.advance] []
          _ <- revealNow p Treacheries.assassinationAttempt
          settle
          getsGame (length . filter ((== "37028") . toCardCode . getCardDef) . HashMap.elems . gameMinions) `shouldReturn` 1
          damageOf p `shouldReturn` 0

  describe "Exodus and Herald of Avalon" do
    for_ [False, True] $ \inDiscard ->
      it (if inDiscard then "Exodus attaches a Shield from discard without revealing it" else "Exodus attaches a Shield from deck without revealing it") do
        tg <- testGame ["gambit"]
        inGame tg do
          let p = player1 tg
          _ <- if inDiscard then setEncounterDeck [Treacheries.advance] [Attachments.psionicShield]
               else setEncounterDeck [Attachments.psionicShield, Treacheries.advance] []
          mid <- addMinion p Minions.exodus
          shields <- getsGame $ filter ((== "37034") . toCardCode . getCardDef) . HashMap.elems . gameAttachments
          length shields `shouldBe` 1
          Attachment.attachmentEnemy (toAttrs $ testHead shields) `shouldBe` Just (EnemyMinionId mid)
          dealtTo p `shouldReturn` []

    it "still enters when no Shield can be found" do
      tg <- testGame ["gambit"]
      inGame tg do
        _ <- setEncounterDeck [Treacheries.advance] []
        mid <- addMinion (player1 tg) Minions.exodus
        minionDamageOf mid `shouldReturn` Just 0
        dealtTo (player1 tg) `shouldReturn` []

    it "Exodus retaliates after the hero attacks him" do
      tg <- testGame ["gambit"]
      inGame tg do
        let p = player1 tg
        _ <- setEncounterDeck [Treacheries.advance] []
        mid <- addMinion p Minions.exodus
        changeForm p
        pass
        clearHand p
        choose "basic attack" (isHeroAttack p)
        choose "attack Exodus" (isTargetChoice mid)
        settle
        minionDamageOf mid `shouldReturn` Just 2
        damageOf p `shouldReturn` 1

    it "Herald uses six threat and its hazard, then makes the defeating player reveal Exodus" do
      tg <- testGame ["gambit", "spider-man"]
      inGame tg do
        let p = player1 tg
            other = player2 tg
        _ <- setEncounterDeck [Minions.exodus, Treacheries.advance] []
        scheme <- revealNow p Schemes.heraldOfAvalon
        settle
        sideSchemeThreat (sideSchemeOf scheme) `shouldReturn` 6
        getHazardCount `shouldReturn` 1
        resolve [SideSchemeMessage (sideSchemeOf scheme) $ SideSchemeThreatRemoved (IdentityRef other) 6]
        exodus <- getsGame $ filter ((== "37032") . toCardCode . getCardDef) . HashMap.elems . gameMinions
        length exodus `shouldBe` 1
        Minion.minionEngagedIdentity (toAttrs $ testHead exodus) `shouldBe` other

  describe "Psionic Shield" do
    it "adds one attack and scheme while attached" do
      tg <- testGame ["gambit"]
      inGame tg do
        let p = player1 tg
        mid <- addMinion p Minions.guildAssassin
        _ <- attachShield p mid
        before <- mainSchemeThreat
        runNow [MinionMessage mid $ MinionSchemesAgainst p]
        settle
        mainSchemeThreat `shouldReturn` (before + 2)
        changeForm p
        pass
        runNow [MinionMessage mid $ MinionAttacks p]
        settle
        damageOf p `shouldReturn` 3

    for_ [("discard", RemoveFromPlay), ("return", ReturnToHand), ("remove", RemoveFromGame), ("victory", AddToVictoryDisplay)] $ \(name, leave) ->
      it ("prevents " <> name <> " and heals the same minion without losing statuses") do
        tg <- testGame ["gambit"]
        inGame tg do
          let p = player1 tg
          mid <- addMinion p Minions.shocker
          shield <- attachShield p mid
          resolve [MinionMessage mid $ MinionDamaged GameRef $ toDamage 1 FromAbility, GiveStatus (MinionRef mid) ConfusedStatus]
          resolve [leave $ MinionRef mid]
          minionDamageOf mid `shouldReturn` Just 0
          getsGame (fmap (Minion.minionConfused . toAttrs) . HashMap.lookup mid . gameMinions) `shouldReturn` Just True
          attachmentPresent shield `shouldReturn` False

    it "prevents overkill and does not trigger a new Quickstrike" do
      tg <- testGame ["gambit"]
      inGame tg do
        let p = player1 tg
        mid <- addMinion p Minions.guildAssassin
        shield <- attachShield p mid
        changeForm p
        pass
        before <- villainDamageNow
        resolve [MinionMessage mid $ MinionDamaged (IdentityRef p) $ withOverkill $ toDamage 6 $ FromPlayerAttack p]
        minionDamageOf mid `shouldReturn` Just 0
        villainDamageNow `shouldReturn` before
        damageOf p `shouldReturn` 0
        attachmentPresent shield `shouldReturn` False

    it "leaves Shield in place when Biomechanical Upgrades handles defeat first" do
      tg <- testGame ["gambit"]
      inGame tg do
        let p = player1 tg
        mid <- addMinion p Minions.shocker
        shield <- attachShield p mid
        _ <- revealNow p Attachments.biomechanicalUpgrades
        settle
        resolve [MinionMessage mid $ MinionDamaged (IdentityRef p) $ toDamage 6 $ FromPlayerAttack p]
        minionDamageOf mid `shouldReturn` Just 0
        attachmentPresent shield `shouldReturn` True
        encounterDiscardCodes >>= (`shouldSatisfy` elem "01185")

    it "retains other attachments when Shield replaces defeat" do
      tg <- testGame ["gambit"]
      inGame tg do
        let p = player1 tg
        mid <- addMinion p Minions.shocker
        enhancement <- revealNow p Attachments.geneticallyEnhanced
        settle
        shield <- attachShield p mid
        resolve [MinionMessage mid $ MinionDamaged (IdentityRef p) $ toDamage 6 $ FromPlayerAttack p]
        minionDamageOf mid `shouldReturn` Just 0
        attachmentPresent enhancement `shouldReturn` True
        attachmentPresent shield `shouldReturn` False

    it "uses only one of two Shields when replacing one departure" do
      tg <- testGame ["gambit"]
      inGame tg do
        let p = player1 tg
        mid <- addMinion p Minions.shocker
        first <- attachShield p mid
        second <- attachShield p mid
        resolve [RemoveFromPlay $ MinionRef mid]
        present <- traverse attachmentPresent [first,second]
        length (filter id present) `shouldBe` 1

    it "prevents Beetle's complete shuffle departure, keeping his card out of the deck" do
      tg <- testGame ["gambit"]
      inGame tg do
        let p = player1 tg
        mid <- addMinion p Minions.beetle
        shield <- attachShield p mid
        clearHand p
        before <- encounterDeckSize
        resolve [MinionMessage mid $ MinionDamaged (IdentityRef p) $ toDamage 4 $ FromPlayerAttack p]
        minionDamageOf mid `shouldReturn` Just 0
        encounterDeckSize `shouldReturn` before
        attachmentPresent shield `shouldReturn` False
        getEncounterDeck >>= (`shouldSatisfy` all ((/= unMinionId mid) . unCardId . ecCardId))

    it "replaces Magik's entire shuffle instead of duplicating a minion in play and deck" do
      tg <- testGame ["gambit"]
      inGame tg do
        let p = player1 tg
        mid <- addMinion p Minions.shocker
        shield <- attachShield p mid
        changeForm p
        pass
        clearHand p
        magik <- putInHand p "32042"
        energy <- putInHand p "01088"
        physical <- putInHand p "01090"
        mental <- putInHand p "01089"
        playWith magik [energy, physical]
        choose "use Magik" (isAbilityOfCard magik)
        payWith [mental]
        choose "shuffle Shocker" (isTargetChoice mid)
        settle
        minionDamageOf mid `shouldReturn` Just 0
        attachmentPresent shield `shouldReturn` False
        getEncounterDeck >>= (`shouldSatisfy` all ((/= unMinionId mid) . unCardId . ecCardId))

    it "surges if there is no minion to attach to" do
      tg <- testGame ["gambit"]
      inGame tg do
        let p = player1 tg
        _ <- setEncounterDeck [Treacheries.advance, Treacheries.assault] []
        before <- length <$> dealtTo p
        _ <- revealNow p Attachments.psionicShield
        settle
        (length <$> dealtTo p) `shouldReturn` (before + 1)

  describe "Acolyte Frenzy" do
    it "makes an engaged Acolyte scheme against an alter-ego, including its villainous boost" do
      tg <- testGame ["gambit"]
      inGame tg do
        let p = player1 tg
        _ <- addMinion p Minions.exodus
        _ <- setEncounterDeck [Treacheries.gangUp, Treacheries.advance] []
        before <- mainSchemeThreat
        _ <- revealNow p Treacheries.acolyteFrenzy
        settle
        mainSchemeThreat `shouldReturn` (before + 2)
        damageOf p `shouldReturn` 0

    it "makes an engaged Acolyte attack a hero" do
      tg <- testGame ["gambit"]
      inGame tg do
        let p = player1 tg
        _ <- addMinion p Minions.exodus
        _ <- setEncounterDeck [Treacheries.advance, Treacheries.advance] []
        changeForm p
        pass
        clearHand p
        _ <- revealNow p Treacheries.acolyteFrenzy
        settle
        damageOf p `shouldReturn` 1

    it "surges when the only Acolyte is engaged with someone else" do
      tg <- testGame ["gambit", "spider-man"]
      inGame tg do
        let p = player1 tg
        _ <- addMinion (player2 tg) Minions.exodus
        _ <- setEncounterDeck [Treacheries.advance, Treacheries.assault] []
        before <- length <$> dealtTo p
        _ <- revealNow p Treacheries.acolyteFrenzy
        settle
        (length <$> dealtTo p) `shouldReturn` (before + 1)
        damageOf p `shouldReturn` 0

    it "its boost stuns and confuses the attacked player" do
      tg <- testGame ["gambit"]
      inGame tg do
        vid <- villainIdNow
        card <- encounterCard Treacheries.acolyteFrenzy
        runNow [RevealBoostCard card $ EnemyVillainId vid]
        settle
        isStunned (player1 tg) `shouldReturn` True
        isConfused (player1 tg) `shouldReturn` True
