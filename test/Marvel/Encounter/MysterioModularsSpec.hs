module Marvel.Encounter.MysterioModularsSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Attachment.Types qualified as Attachment
import Marvel.Card.PlayerCard.Types (playerZoneEncounterCard)
import Marvel.Matchers hiding (ExhaustedIdentity, ExhaustedAlly)
import Marvel.Minion.Cards qualified as Minions
import Marvel.Minion.Runner qualified as Minion
import Marvel.Query
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Villain.Types qualified as Villain
import TestImport

attachmentAttrs :: CardCode -> GameAppT [Attrs Attachment.Attachment]
attachmentAttrs code = getsGame (filter ((== code) . toCardCode . Attachment.attachmentCardDef) . map toAttrs . HashMap.elems . gameAttachments)

spec :: Spec
spec = describe "Mysterio modular sets" do
  describe "Induced Panic" do
    it "blocks printed hero responses while retaining basic powers" do
      tg <- testGame ["quicksilver"]
      let p = player1 tg
      inGame tg do
        _ <- revealNow p Attachments.smInducedPanic
        pass
        changeForm p
        pass
        clearHand p
        offers (isHeroAttack p) `shouldReturn` True
        choose "basic attack" (isHeroAttack p)
        chooseVillain
        offers (isNamedAbility "Super Speed") `shouldReturn` False
        pass
        villainDamageNow `shouldReturn` 1
        isExhaustedNow p `shouldReturn` True

    it "blocks Miles's explicitly instructed Special while allowing Web-Shot's main effect" do
      tg <- testGame ["spider-man-miles-morales"]
      let p = player1 tg
      inGame tg do
        _ <- revealNow p Attachments.smInducedPanic
        pass
        changeForm p
        pass
        clearHand p
        shot <- putInHand p "27034"
        energy <- putInHand p "01088"
        playWith shot [energy]
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 4
        Villain.villainStunned . toAttrs <$> getVillain `shouldReturn` False
        within <- putIntoPlay p "27037"
        choose "basic attack" (isHeroAttack p)
        chooseVillain
        offers (isAbilityOfCard within) `shouldReturn` False

    it "discards one random identity-specific hand card as its alter-ego removal cost" do
      tg <- testGame ["spider-man-miles-morales"]
      let p = player1 tg
      inGame tg do
        clearHand p
        first <- putInHand p "27031"
        second <- putInHand p "27034"
        aspect <- putInHand p "27042"
        panic <- revealNow p Attachments.smInducedPanic
        pass
        choose "remove Induced Panic" (isAbilityOf $ AttachmentId $ unCardId $ ecCardId panic)
        pass
        hand <- handOf p
        hand `shouldSatisfy` elem aspect
        length (filter (`elem` [first,second]) hand) `shouldBe` 1
        attachmentAttrs "27153" `shouldReturn` []

    it "cannot be removed without an identity-specific hand card" do
      tg <- testGame ["spider-man-miles-morales"]
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- putInHand p "27042"
        panic <- revealNow p Attachments.smInducedPanic
        pass
        offers (isAbilityOf $ AttachmentId $ unCardId $ ecCardId panic) `shouldReturn` False

  describe "Evil Doppelganger" do
    it "adds SCH and ATK for identity-specific cards in its engaged player's hand only" do
      tg <- testGame ["spider-man-miles-morales", "ghost-spider"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        clearHand p
        clearHand other
        signature <- putInHand p "27031"
        _ <- putInHand p "27034"
        _ <- putInHand p "27042"
        _ <- putInHand other "27002"
        mid <- addMinion p Minions.smEvilDoppelganger
        attrs <- getsGame (toAttrs . fromMaybe (error "missing Doppelganger") . HashMap.lookup mid . gameMinions)
        Minion.getModifiedAttack attrs `shouldReturn` 3
        Minion.getModifiedScheme attrs `shouldReturn` 3
        resolve [DiscardedCard $ PlayerCard signature]
        Minion.getModifiedAttack attrs `shouldReturn` 2

    it "draws three cards and discards three at random as a boost" do
      tg <- testGame ["spider-man-miles-morales"]
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- putInHand p "27031"
        _ <- putInHand p "27034"
        _ <- stackDeck p ["27032","27033","27039"]
        _ <- setEncounterDeck [Minions.smEvilDoppelganger, Minions.hydraSoldier] []
        before <- length <$> discardOf p
        vid <- villainIdNow
        resolve [VillainMessage vid (VillainSchemesAgainst p)]
        length <$> handOf p `shouldReturn` 2
        length <$> discardOf p `shouldReturn` before + 3

  describe "Evil Doppelganger during player elimination" do
    it "keeps its base statistics after the final player is removed without querying that player's vanished hand" do
      tg <- testGame ["spider-man-miles-morales"]
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- putInHand p "27031"
        mid <- addMinion p Minions.smEvilDoppelganger
        before <- getsGame (toAttrs . fromMaybe (error "missing doppelganger") . HashMap.lookup mid . gameMinions)
        Minion.getModifiedAttack before `shouldReturn` 2
        runNow [IdentityMessage p (IdentityDamaged GameRef $ toDamage 9 FromAbility)]
        pass
        getsGame gameState `shouldReturn` Finished Lost
        getsGame (HashMap.member p . gamePlayers) `shouldReturn` False
        after <- getsGame (toAttrs . fromMaybe (error "missing doppelganger") . HashMap.lookup mid . gameMinions)
        Minion.getModifiedAttack after `shouldReturn` 1
        Minion.getModifiedScheme after `shouldReturn` 1

    it "switches its hand-dependent bonus to the surviving player after its engaged player is eliminated" do
      tg <- testGame ["spider-man-miles-morales", "ghost-spider"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        clearHand p
        clearHand other
        _ <- putInHand p "27031"
        _ <- putInHand other "27002"
        _ <- putInHand other "27003"
        mid <- addMinion p Minions.smEvilDoppelganger
        runNow [IdentityMessage p (IdentityDamaged GameRef $ toDamage 9 FromAbility)]
        pass
        getsGame (HashMap.member p . gamePlayers) `shouldReturn` False
        after <- getsGame (toAttrs . fromMaybe (error "missing doppelganger") . HashMap.lookup mid . gameMinions)
        minionEngagedIdentity after `shouldBe` other
        Minion.getModifiedAttack after `shouldReturn` 3
        Minion.getModifiedScheme after `shouldReturn` 3

  describe "Fool's Paradise and Weakness from Within" do
    it "gives every identity two hand size and loses the bonus when defeated into the victory display" do
      tg <- testGame ["spider-man-miles-morales", "ghost-spider"]
      let p = player1 tg
      inGame tg do
        paradise <- revealNow p Schemes.smFoolSParadise
        pass
        for_ tg.seats \player -> handSizeOf player `shouldReturn` 8
        let sid = SideSchemeId $ unCardId $ ecCardId paradise
        resolve [SideSchemeMessage sid (SideSchemeThwarted (IdentityRef p) 12)]
        for_ tg.seats \player -> handSizeOf player `shouldReturn` 6
        getVictoryDisplay >>= (`shouldSatisfy` elem paradise)

    it "places one additional threat per card in the revealing player's hand" do
      tg <- testGame ["spider-man-miles-morales"]
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- putInHand p "27031"
        _ <- putInHand p "27042"
        scheme <- revealNow p Schemes.smWeaknessFromWithin
        pass
        sideSchemeThreat (SideSchemeId $ unCardId $ ecCardId scheme) `shouldReturn` 3

  describe "Deepest Fears" do
    for_ [False,True] \hasSignature ->
      it ("discards a number equal to hand size and " <> if hasSignature then "places threat for a discarded signature card" else "deals damage when none was discarded") do
        tg <- testGame ["spider-man-miles-morales"]
        let p = player1 tg
        inGame tg do
          clearHand p
          _ <- putInHand p "27031"
          _ <- putInHand p "27034"
          cards <- stackDeck p [if hasSignature then "27039" else "27042", "01088", "27043"]
          before <- mainSchemeThreatNow
          _ <- revealNow p Treacheries.smDeepestFears
          pass
          discarded <- discardOf p
          take 2 cards `shouldSatisfy` all (`elem` discarded)
          mainSchemeThreatNow `shouldReturn` (before + if hasSignature then 1 else 0)
          damageOf p `shouldReturn` (if hasSignature then 0 else 1)

    it "takes one damage with an empty hand without discarding any deck cards" do
      tg <- testGame ["spider-man-miles-morales"]
      let p = player1 tg
      inGame tg do
        clearHand p
        before <- unDeck . playerIdentityDeck <$> playerAttrs p
        _ <- revealNow p Treacheries.smDeepestFears
        pass
        damageOf p `shouldReturn` 1
        unDeck . playerIdentityDeck <$> playerAttrs p `shouldReturn` before

  describe "Delusion of Collusion" do
    it "prevents readying controlled allies and Persona supports, leaving other supports and players unaffected" do
      tg <- testGame ["spider-man-miles-morales", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        ally <- putIntoPlay p "01068"
        persona <- putIntoPlay p "27035"
        location <- putIntoPlay p "01064"
        others <- putIntoPlay other "27007"
        _ <- revealNow p Attachments.smDelusionOfCollusion
        pass
        resolve [AllyMessage (allyIdOf ally) ExhaustedAlly
          , SupportMessage (supportIdOf persona) ExhaustedSupport
          , SupportMessage (supportIdOf location) ExhaustedSupport
          , SupportMessage (supportIdOf others) ExhaustedSupport
          , AllyMessage (allyIdOf ally) ReadiedAlly
          , SupportMessage (supportIdOf persona) ReadiedSupport
          , SupportMessage (supportIdOf location) ReadiedSupport
          , SupportMessage (supportIdOf others) ReadiedSupport]
        allyExhausted <$> allyIn "01068" `shouldReturn` True
        map supportExhausted <$> supportsWith "27035" `shouldReturn` [True]
        map supportExhausted <$> supportsWith "01064" `shouldReturn` [False]
        map supportExhausted <$> supportsWith "27007" `shouldReturn` [False]

    for_ [False,True] \discardAlly ->
      it ("can be removed by discarding a controlled " <> if discardAlly then "ally" else "Persona support") do
        tg <- testGame ["spider-man-miles-morales", "spider-man"]
        let p = player1 tg
            other = player2 tg
        inGame tg do
          ally <- putIntoPlay p "01068"
          persona <- putIntoPlay p "27035"
          wrong <- putIntoPlay p "01064"
          others <- putIntoPlay other "27007"
          card <- revealNow p Attachments.smDelusionOfCollusion
          pass
          choose "remove Delusion" (isAbilityOf $ AttachmentId $ unCardId $ ecCardId card)
          offers (isTargetChoice $ supportIdOf wrong) `shouldReturn` False
          offers (isTargetChoice $ supportIdOf others) `shouldReturn` False
          if discardAlly
            then choose "discard the ally" (isTargetChoice $ allyIdOf ally)
            else choose "discard the Persona support" (isTargetChoice $ supportIdOf persona)
          pass
          attachmentAttrs "27170" `shouldReturn` []
          discardOf p >>= (`shouldSatisfy` elem (if discardAlly then ally else persona))

    it "keeps its delayed boost effect after Mysterio moves the physical Illusion card to the player's discard" do
      tg <- testGameAgainst "27084" ["spider-man-miles-morales"]
      let p = player1 tg
      inGame tg do
        clearHand p
        ally <- putIntoPlay p "01068"
        resolve [AllyMessage (allyIdOf ally) (AllyDamaged GameRef $ toDamage 2 FromAbility)]
        ([boost,_],_) <- setEncounterDeck [Attachments.smDelusionOfCollusion, Minions.hydraSoldier] []
        vid <- villainIdNow
        start [VillainMessage vid (VillainAttacks p)]
        choose "Vision defends" \case AllyDefend aid _ -> aid == allyIdOf ally; _ -> False
        resolve []
        damageOf p `shouldReturn` 4
        mapMaybe playerZoneEncounterCard <$> discardOf p >>= (`shouldSatisfy` elem boost)
        encounterDiscardNow >>= (`shouldSatisfy` notElem boost)

  describe "Manipulated Mind" do
    it "converts the cheapest controlled ally, preserving traits and printed powers, then reverts it when removed" do
      tg <- testGame ["spider-man-miles-morales", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        _ <- putIntoPlay p "01068"
        cheapest <- putIntoPlay p "27048"
        _ <- putIntoPlay other "01067"
        card <- revealNow p Attachments.smManipulatedMind
        pass
        let mid = MinionId $ unAllyId $ allyIdOf cheapest
        attrs <- getsGame (toAttrs . fromMaybe (error "missing converted ally") . HashMap.lookup mid . gameMinions)
        minionEngagedIdentity attrs `shouldBe` p
        Minion.getModifiedScheme attrs `shouldReturn` 1
        Minion.getModifiedAttack attrs `shouldReturn` 2
        alliesWith "27048" `shouldReturn` []
        runNow [RemoveFromPlay $ AttachmentRef $ AttachmentId $ unCardId $ ecCardId card]
        pass
        alliesWith "27048" >>= (`shouldSatisfy` ((== 1) . length))

    it "surges rather than taking another player's ally when you control none" do
      tg <- testGame ["spider-man-miles-morales", "spider-man"]
      let p = player1 tg
      inGame tg do
        _ <- putIntoPlay (player2 tg) "01068"
        _ <- stackEncounterDeck [Schemes.bombScare]
        _ <- revealNow p Attachments.smManipulatedMind
        pass
        attachmentAttrs "27171" `shouldReturn` []
        alliesWith "01068" >>= (`shouldSatisfy` ((== 1) . length))
        any ((== "01109") . toCardCode) <$> dealtTo p `shouldReturn` True

  describe "Old Grudge and Analysis Paralysis" do
    for_ ["set aside", "deck", "discard"] \zone ->
      it ("reveals the nemesis minion from " <> zone <> " and grants one hit point") do
        tg <- testGame ["spider-man-miles-morales"]
        let p = player1 tg
        inGame tg do
          [nemesis] <- getNemesisMinionsOutOfPlay p
          case zone of
            "deck" -> resolve [ShuffleIntoEncounterDeck [nemesis]]
            "discard" -> resolve [DiscardedCard $ EncounterCard nemesis]
            _ -> pure ()
          _ <- revealNow p Attachments.smOldGrudge
          pass
          attrs <- getsGame (toAttrs . fromMaybe (error "missing Prowler") . HashMap.lookup (minionOf nemesis) . gameMinions)
          Minion.getModifiedHitPoints attrs `shouldReturn` 6
          minionTough attrs `shouldBe` True

    it "reveals the nemesis side scheme and adds its actual threat to Analysis Paralysis" do
      tg <- testGame ["spider-man-miles-morales"]
      let p = player1 tg
      inGame tg do
        card <- revealNow p Schemes.smAnalysisParalysis
        pass
        sideSchemeThreat (SideSchemeId $ unCardId $ ecCardId card) `shouldReturn` 5
        schemes <- getsGame (map (toCardCode . getCardDef) . HashMap.elems . gameSideSchemes)
        schemes `shouldSatisfy` elem "27057"

    it "Old Grudge's boost damages every controlled character, not another player's" do
      tg <- testGame ["spider-man-miles-morales", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        ally <- putIntoPlay p "01068"
        _ <- putIntoPlay other "01075"
        _ <- setEncounterDeck [Attachments.smOldGrudge, Minions.hydraSoldier] []
        vid <- villainIdNow
        resolve [VillainMessage vid (VillainSchemesAgainst p)]
        damageOf p `shouldReturn` 1
        damageOf other `shouldReturn` 0
        allyDamage <$> allyIn "01068" `shouldReturn` 1
        allyDamage <$> allyIn "01075" `shouldReturn` 0
