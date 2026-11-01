module Marvel.Encounter.AngelNemesisSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Attachment.Types qualified as Attachment
import Marvel.Message.Lifted qualified as L
import Marvel.Minion.Cards qualified as Minions
import Marvel.Minion.Runner qualified as Minion
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.Obligation.Types qualified as Obligation
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Treachery.Cards qualified as Treacheries
import TestImport

settle :: GameAppT ()
settle = go (200 :: Int)
 where
  go 0 = error "Angel encounter fixture did not settle"
  go n = openQuestion >>= \case
    Just (_, QuestionLabel "$label.yourTurn" _ _) -> pure ()
    Just (you, q) -> do
      let passing c = isNoResponse c || isNoDefender c || isLabel "$label.declineToDefend" c
      answerQuestion you (fromMaybe 0 $ headMay [i | (i,c) <- zip [0..] (choicesOf q), passing c])
      go (n-1)
    Nothing -> pure ()

advanceTo :: (Choice -> Bool) -> GameAppT ()
advanceTo wanted = go (100 :: Int)
 where
  go 0 = error "Angel encounter fixture did not reach its choice"
  go n = whenM (not <$> offers wanted) $ openQuestion >>= \case
    Just (you, q) -> do
      let passing c = isNoResponse c || isNoDefender c || isLabel "$label.declineToDefend" c
      answerQuestion you (fromMaybe 0 $ headMay [i | (i,c) <- zip [0..] (choicesOf q), passing c])
      go (n-1)
    Nothing -> error "Angel encounter fixture ended before the expected choice"

perform :: L.QueueT GameAppT () -> GameAppT ()
perform action = L.runQueueT (L.capture action) >>= runNow

asAngel :: IdentityId -> GameAppT ()
asAngel p = changeFormTo p A >> settle >> clearHand p

obligationOf :: EncounterCard -> ObligationId
obligationOf = ObligationId . unCardId . ecCardId

attachmentOf :: EncounterCard -> AttachmentId
attachmentOf = AttachmentId . unCardId . ecCardId

attachmentPresent :: EncounterCard -> GameAppT Bool
attachmentPresent card = getsGame (HashMap.member (attachmentOf card) . gameAttachments)

attachEnemy :: EncounterCard -> GameAppT (Maybe EnemyId)
attachEnemy card = getsGame (maybe Nothing (Attachment.attachmentEnemy . toAttrs) . HashMap.lookup (attachmentOf card) . gameAttachments)

allyReady :: AllyId -> GameAppT Bool
allyReady aid = getsGame (maybe False (not . allyExhausted . toAttrs) . HashMap.lookup aid . gameAllies)

harpoonWithNoRevealDamage :: IdentityId -> GameAppT MinionId
harpoonWithNoRevealDamage p = do
  _ <- stackDeck p ["42014"]
  addMinion p Minions.harpoon

spec :: Spec
spec = describe "Angel nemesis" do
  describe "Apocalyptic Influence" do
    it "goes to Warren even when another player reveals it, and changes him to Archangel" do
      tg <- testGame ["angel", "spider-man"]
      let p = player1 tg
          q = player2 tg
      inGame tg do
        before <- mainSchemeThreatNow
        card <- revealNow q Obligations.apocalypticInfluence
        settle
        (.playerIdentitySide) <$> playerAttrs p `shouldReturn` C
        (.playerIdentitySide) <$> playerAttrs q `shouldReturn` B
        owner <- getsGame (fmap (Obligation.obligationOwner . toAttrs) . HashMap.lookup (obligationOf card) . gameObligations)
        owner `shouldBe` Just (Just p)
        mainSchemeThreatNow `shouldReturn` before
        obligationsInPlay `shouldReturn` 1

    it "places two threat instead of changing form when Warren is already Archangel" do
      tg <- testGame ["angel"]
      let p = player1 tg
      inGame tg do
        changeFormTo p C
        settle
        before <- mainSchemeThreatNow
        _ <- revealNow p Obligations.apocalypticInfluence
        settle
        mainSchemeThreatNow `shouldReturn` before + 2
        (.playerIdentitySide) <$> playerAttrs p `shouldReturn` C
        obligationsInPlay `shouldReturn` 1

    it "offers its discard action only in alter-ego, deals the first player the cost, and discards itself" do
      tg <- testGame ["angel", "spider-man"]
      let p = player1 tg
          q = player2 tg
      inGame tg do
        card <- revealNow p Obligations.apocalypticInfluence
        settle
        offers (isAbilityOf $ ObligationRef $ obligationOf card) `shouldReturn` False
        runNow [IdentityMessage p $ ChangedToForm B, PassFirstPlayer]
        settle
        _ <- setEncounterDeck [Treacheries.advance, Treacheries.advance, Treacheries.advance] []
        beforeP <- length <$> dealtTo p
        beforeQ <- length <$> dealtTo q
        choose "discard Apocalyptic Influence" (isAbilityOf $ ObligationRef $ obligationOf card)
        choose "deal the first player the encounter card cost" (isTargetChoice q)
        payWith []
        settle
        obligationsInPlay `shouldReturn` 0
        length <$> dealtTo p `shouldReturn` beforeP
        length <$> dealtTo q `shouldReturn` beforeQ + 1
        encounterDiscardNow >>= (`shouldSatisfy` elem card)

  describe "Harpoon" do
    it "discards through non-events and deals indirect damage equal to the event's printed cost" do
      tg <- testGame ["angel"]
      let p = player1 tg
      inGame tg do
        cards <- stackDeck p ["01088", "42020", "42003", "42014"]
        card <- revealNow p Minions.harpoon
        settle
        damageOf p `shouldReturn` 3
        discarded <- discardOf p
        discarded `shouldSatisfy` elem (cards !! 0)
        discarded `shouldSatisfy` elem (cards !! 1)
        discarded `shouldSatisfy` elem (cards !! 2)
        discarded `shouldSatisfy` notElem (cards !! 3)
        minionDamageOf (minionOf card) `shouldReturn` Just 0

    for_ [(B, 1), (A, 2), (C, 2)] $ \(side, amount) ->
      it ("adds attack only while attacking an Aerial identity (side " <> show side <> ")") do
        tg <- testGame ["angel"]
        let p = player1 tg
        inGame tg do
          when (side /= B) $ changeFormTo p side >> settle
          mid <- harpoonWithNoRevealDamage p
          runNow [MinionMessage mid $ MinionAttacks p]
          settle
          damageOf p `shouldReturn` amount
          attrs <- getsGame (maybe (error "Harpoon missing") toAttrs . HashMap.lookup mid . gameMinions)
          Minion.getModifiedAttack attrs `shouldReturn` 1

    it "adds attack against an Aerial ally defender" do
      tg <- testGame ["angel"]
      let p = player1 tg
      inGame tg do
        asAngel p
        ally <- putIntoPlay p "42012"
        mid <- harpoonWithNoRevealDamage p
        runNow [MinionMessage mid $ MinionAttacks p]
        let defend = \case AllyDefend aid _ -> aid == allyIdOf ally; _ -> False
        advanceTo defend
        choose "Siryn defends" defend
        settle
        allyDamage <$> allyIn "42012" `shouldReturn` 2
        damageOf p `shouldReturn` 0

    it "discards the entire finite deck without dealing damage when no event is present" do
      tg <- testGame ["angel"]
      let p = player1 tg
      inGame tg do
        cards <- stackDeck p ["01088", "42020"]
        withGame_ $ entitiesL . playersL . ix p %~ \(PlayerIdentity a) ->
          PlayerIdentity a {playerIdentityDeck = Deck cards}
        _ <- revealNow p Minions.harpoon
        settle
        damageOf p `shouldReturn` 0
        rebuilt <- unDeck . (.playerIdentityDeck) <$> playerAttrs p
        for_ cards $ \card -> rebuilt `shouldSatisfy` elem card

    it "checks the actual defending ally rather than the originally attacked Aerial hero" do
      tg <- testGame ["angel"]
      let p = player1 tg
      inGame tg do
        asAngel p
        ally <- putIntoPlay p "01084"
        mid <- harpoonWithNoRevealDamage p
        runNow [MinionMessage mid $ MinionAttacks p]
        let defend = \case AllyDefend aid _ -> aid == allyIdOf ally; _ -> False
        advanceTo defend
        choose "Nick Fury defends" defend
        settle
        allyDamage <$> allyIn "01084" `shouldReturn` 1
        damageOf p `shouldReturn` 0

  describe "Hook, Line, and Sinker" do
    it "starts with two threat per player" do
      tg <- testGame ["angel", "spider-man"]
      inGame tg do
        card <- revealNow (player1 tg) Schemes.hookLineAndSinker
        settle
        sideSchemeThreat (sideSchemeOf card) `shouldReturn` 4

    it "makes a Brute attack indirect and exhausts the character actually damaged" do
      tg <- testGame ["angel"]
      let p = player1 tg
      inGame tg do
        asAngel p
        ally <- putIntoPlay p "01084"
        mid <- harpoonWithNoRevealDamage p
        _ <- revealNow p Schemes.hookLineAndSinker
        settle
        runNow [MinionMessage mid $ MinionAttacks p]
        advanceTo (isTargetChoice p)
        choose "assign first point to Angel" (isTargetChoice p)
        choose "assign second point to Angel" (isTargetChoice p)
        settle
        damageOf p `shouldReturn` 2
        isExhaustedNow p `shouldReturn` True
        allyReady (allyIdOf ally) `shouldReturn` True
        allyDamage <$> allyIn "01084" `shouldReturn` 0

    it "also exhausts recipients of indirect damage from unrelated card abilities" do
      tg <- testGame ["angel"]
      let p = player1 tg
      inGame tg do
        ally <- putIntoPlay p "01084"
        _ <- revealNow p Schemes.hookLineAndSinker
        settle
        perform (L.dealIndirectDamage p p 2)
        choose "one indirect damage to Warren" (isTargetChoice p)
        choose "one indirect damage to Nick Fury" (isTargetChoice $ allyIdOf ally)
        settle
        damageOf p `shouldReturn` 1
        allyDamage <$> allyIn "01084" `shouldReturn` 1
        isExhaustedNow p `shouldReturn` True
        allyReady (allyIdOf ally) `shouldReturn` False

    it "does not exhaust a tough character that prevented all assigned indirect damage" do
      tg <- testGame ["angel"]
      let p = player1 tg
      inGame tg do
        _ <- revealNow p Schemes.hookLineAndSinker
        settle
        perform (L.giveStatus p ToughStatus)
        settle
        perform (L.dealIndirectDamage p p 2)
        settle
        damageOf p `shouldReturn` 0
        isExhaustedNow p `shouldReturn` False

    it "leaves non-Brute attacks as direct damage and does not exhaust their recipient" do
      tg <- testGame ["angel"]
      let p = player1 tg
      inGame tg do
        asAngel p
        _ <- putIntoPlay p "01084"
        mid <- addMinion p Minions.weaponsRunner
        _ <- revealNow p Schemes.hookLineAndSinker
        settle
        runNow [MinionMessage mid $ MinionAttacks p]
        settle
        damageOf p `shouldReturn` 1
        isExhaustedNow p `shouldReturn` False

  describe "Harpoon's Harpoon" do
    it "attaches to an existing Harpoon, adds one attack, and deals two indirect after his attack" do
      tg <- testGame ["angel"]
      let p = player1 tg
      inGame tg do
        asAngel p
        mid <- harpoonWithNoRevealDamage p
        card <- revealNow p Attachments.harpoonsHarpoon
        settle
        attachEnemy card `shouldReturn` Just (EnemyMinionId mid)
        runNow [MinionMessage mid $ MinionAttacks p]
        settle
        damageOf p `shouldReturn` 5

    for_ [False, True] $ \fromDiscard ->
      it ("finds and reveals Harpoon from the " <> if fromDiscard then "discard pile" else "encounter deck") do
        tg <- testGame ["angel"]
        let p = player1 tg
        inGame tg do
          _ <- stackDeck p ["42005"]
          _ <- if fromDiscard
            then setEncounterDeck [Treacheries.advance] [Minions.harpoon]
            else setEncounterDeck [Minions.harpoon, Treacheries.advance] []
          card <- revealNow p Attachments.harpoonsHarpoon
          settle
          mid <- getsGame (headMay . HashMap.keys . gameMinions)
          attachEnemy card `shouldReturn` (EnemyMinionId <$> mid)
          mid `shouldSatisfy` isJust
          damageOf p `shouldReturn` 2

    it "surges and discards itself when Harpoon cannot be found" do
      tg <- testGame ["angel"]
      let p = player1 tg
      inGame tg do
        _ <- setEncounterDeck [Treacheries.advance, Treacheries.advance] []
        before <- length <$> dealtTo p
        card <- revealNow p Attachments.harpoonsHarpoon
        settle
        attachmentPresent card `shouldReturn` False
        length <$> dealtTo p `shouldReturn` before + 1
        encounterDiscardNow >>= (`shouldSatisfy` elem card)

    it "still deals its indirect damage to the attacked player when an ally defends" do
      tg <- testGame ["angel"]
      let p = player1 tg
      inGame tg do
        asAngel p
        ally <- putIntoPlay p "01084"
        mid <- harpoonWithNoRevealDamage p
        _ <- revealNow p Attachments.harpoonsHarpoon
        settle
        runNow [MinionMessage mid $ MinionAttacks p]
        let defend = \case AllyDefend aid _ -> aid == allyIdOf ally; _ -> False
        advanceTo defend
        choose "Nick Fury defends" defend
        advanceTo (isTargetChoice p)
        choose "assign first harpoon damage to Angel" (isTargetChoice p)
        choose "assign second harpoon damage to Angel" (isTargetChoice p)
        settle
        allyDamage <$> allyIn "01084" `shouldReturn` 2
        damageOf p `shouldReturn` 2

    it "deals its after-attack damage to another player's hero who defended" do
      tg <- testGame ["angel", "spider-man"]
      let p = player1 tg
          q = player2 tg
      inGame tg do
        asAngel p
        runNow [IdentityMessage q $ ChangedToForm A]
        settle
        clearHand q
        mid <- harpoonWithNoRevealDamage p
        _ <- revealNow p Attachments.harpoonsHarpoon
        settle
        runNow [MinionMessage mid $ MinionAttacks p]
        advanceTo (isTargetChoice q)
        choose "ask Spider-Man to defend" (isTargetChoice q)
        choose "Spider-Man defends" (isTargetChoice q)
        settle
        damageOf p `shouldReturn` 0
        damageOf q `shouldReturn` 2

    it "deals its after-attack damage to the controller of another player's defending ally" do
      tg <- testGame ["angel", "spider-man"]
      let p = player1 tg
          q = player2 tg
      inGame tg do
        asAngel p
        ally <- putIntoPlay q "01084"
        mid <- harpoonWithNoRevealDamage p
        _ <- revealNow p Attachments.harpoonsHarpoon
        settle
        runNow [MinionMessage mid $ MinionAttacks p]
        advanceTo (isTargetChoice q)
        choose "ask the other player to defend" (isTargetChoice q)
        let defend = \case AllyDefend aid _ -> aid == allyIdOf ally; _ -> False
        choose "Nick Fury defends for the other player" defend
        advanceTo (isTargetChoice q)
        choose "assign first indirect damage to Peter" (isTargetChoice q)
        choose "assign second indirect damage to Peter" (isTargetChoice q)
        settle
        allyDamage <$> allyIn "01084" `shouldReturn` 2
        damageOf p `shouldReturn` 0
        damageOf q `shouldReturn` 2

  describe "Spear Shot" do
    it "uses printed event cost and does not surge for a non-Aerial event" do
      tg <- testGame ["angel"]
      let p = player1 tg
      inGame tg do
        cards <- stackDeck p ["01088", "42016", "42014"]
        _ <- setEncounterDeck [Treacheries.advance] []
        before <- length <$> dealtTo p
        _ <- revealNow p Treacheries.spearShot
        settle
        damageOf p `shouldReturn` 1
        length <$> dealtTo p `shouldReturn` before
        discarded <- discardOf p
        discarded `shouldSatisfy` elem (cards !! 0)
        discarded `shouldSatisfy` elem (cards !! 1)
        discarded `shouldSatisfy` notElem (cards !! 2)

    it "surges after dealing damage for an Aerial event" do
      tg <- testGame ["angel"]
      let p = player1 tg
      inGame tg do
        _ <- stackDeck p ["42003"]
        _ <- setEncounterDeck [Treacheries.advance] []
        before <- length <$> dealtTo p
        _ <- revealNow p Treacheries.spearShot
        settle
        damageOf p `shouldReturn` 3
        length <$> dealtTo p `shouldReturn` before + 1

    it "surges for a zero-cost Aerial event even though it deals no damage" do
      tg <- testGame ["angel"]
      let p = player1 tg
      inGame tg do
        _ <- stackDeck p ["42014"]
        _ <- setEncounterDeck [Treacheries.advance] []
        before <- length <$> dealtTo p
        _ <- revealNow p Treacheries.spearShot
        settle
        damageOf p `shouldReturn` 0
        length <$> dealtTo p `shouldReturn` before + 1

    it "neither damages nor surges if its finite deck contains no event" do
      tg <- testGame ["angel"]
      let p = player1 tg
      inGame tg do
        cards <- stackDeck p ["01088", "42020"]
        withGame_ $ entitiesL . playersL . ix p %~ \(PlayerIdentity a) ->
          PlayerIdentity a {playerIdentityDeck = Deck cards}
        _ <- setEncounterDeck [Treacheries.advance, Treacheries.advance] []
        before <- length <$> dealtTo p
        _ <- revealNow p Treacheries.spearShot
        settle
        damageOf p `shouldReturn` 0
        -- Emptying the player deck deals one card; Spear Shot adds no surge.
        length <$> dealtTo p `shouldReturn` before + 1

    it "deals two indirect damage as a boost ability" do
      tg <- testGame ["angel"]
      let p = player1 tg
      inGame tg do
        card <- encounterCard Treacheries.spearShot
        vid <- villainIdNow
        runNow [RevealBoostCard card (EnemyVillainId vid)]
        settle
        damageOf p `shouldReturn` 2
