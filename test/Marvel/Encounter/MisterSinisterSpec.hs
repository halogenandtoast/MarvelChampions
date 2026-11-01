module Marvel.Encounter.MisterSinisterSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as T
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Attachment.Types qualified as Attachment
import Marvel.EncounterSet qualified as Set
import Marvel.Matchers hiding (ExhaustedAlly)
import Marvel.Minion.Cards qualified as Minions
import Marvel.Modifier
import Marvel.NextEvolution.Sinister (superpowers)
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Query
import Marvel.Trait hiding (Finished)
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Villain.Types qualified as Villain
import TestImport

sinisterGame :: Difficulty -> IO TestGame
sinisterGame difficulty = do
  let setup = ScenarioSetup "40136" [Set.NastyBoys] difficulty
  (app, ran, logged) <- dealtGameLogged setup [starter "spider-man"] 1
  seats <- runGameApp app (getsGame gameSeats)
  let tg = TestGame app ran logged seats
  inGame tg toFirstTurn
  pure tg

attachmentIdOf :: EncounterCard -> AttachmentId
attachmentIdOf = AttachmentId . unCardId . ecCardId

attachmentsWithCode :: CardCode -> GameAppT [Attrs Attachment.Attachment]
attachmentsWithCode code = getsGame $ map toAttrs . filter ((== code) . toCardCode . getCardDef) . HashMap.elems . gameAttachments

settle :: GameAppT ()
settle = go (200 :: Int)
 where
  go 0 = error "Mister Sinister fixture did not settle"
  go n = openQuestion >>= \case
    Just (_, QuestionLabel "$label.yourTurn" _ _) -> pure ()
    Just (you, _) -> answerQuestion you 0 >> go (n - 1)
    Nothing -> pure ()

mainCode :: GameAppT CardCode
mainCode = getsGame $ toCardCode . getCardDef . snd . testHead . HashMap.toList . gameMainSchemes

advanceCurrent :: GameAppT ()
advanceCurrent = mainSchemeIdNow >>= \sid -> resolve [AdvanceMainScheme sid]

spec :: Spec
spec = describe "Mister Sinister" do
  it "starts on one random stage 2 with one power attached and Hope in play" do
    tg <- sinisterGame Normal
    inGame tg do
      code <- mainCode
      code `shouldSatisfy` (`elem` ["40140", "40141", "40142"])
      vid <- villainIdNow
      superpowers vid `shouldReturn` 1
      alliesWith "40130" >>= \allies -> length allies `shouldBe` 1
      mainSchemeThreatNow `shouldReturn` 1

  it "resolves the first stage 2 before Mister Sinister II in expert setup" do
    tg <- sinisterGame Expert
    inGame tg do
      toCardCode . getCardDef <$> getVillain `shouldReturn` "40137"
      vid <- villainIdNow
      superpowers vid `shouldReturn` 1
      mainSchemeThreatNow `shouldReturn` 3

  it "uses two different stage 2s and then Sinister Ends" do
    tg <- sinisterGame Normal
    inGame tg do
      first <- mainCode
      advanceCurrent
      second <- mainCode
      second `shouldSatisfy` (/= first)
      second `shouldSatisfy` (`elem` ["40140", "40141", "40142"])
      vid <- villainIdNow
      superpowers vid `shouldReturn` 2
      advanceCurrent
      mainCode `shouldReturn` "40143"
      mainSchemeThreatNow `shouldReturn` 0

  it "loses when Sinister Ends completes" do
    tg <- sinisterGame Normal
    inGame tg do
      advanceCurrent
      advanceCurrent
      advanceCurrent
      getsGame gameState `shouldReturn` Finished Lost

  it "Sinister Ends redirects Mister Sinister's attack to Hope Summers" do
    tg <- sinisterGame Normal
    inGame tg do
      let p = player1 tg
      advanceCurrent
      advanceCurrent
      mainCode `shouldReturn` "40143"
      hope <- testHead <$> alliesWith "40130"
      let aid = allyId hope
      resolve [GiveStatus (AllyRef aid) ToughStatus]
      before <- damageOf p
      vid <- villainIdNow
      resolve [VillainMessage vid $ VillainAttacksWithoutBoost p]
      damageOf p `shouldReturn` before
      remaining <- alliesWith "40130"
      remaining `shouldSatisfy` notNull
      map allyTough remaining `shouldBe` [False]

  it "places stage-dependent threat only for a newly placed status" do
    tg <- sinisterGame Normal
    inGame tg do
      vid <- villainIdNow
      before <- mainSchemeThreatNow
      resolve [GiveStatus (VillainRef vid) ToughStatus]
      mainSchemeThreatNow `shouldReturn` before + 1
      resolve [GiveStatus (VillainRef vid) ToughStatus]
      mainSchemeThreatNow `shouldReturn` before + 1

  it "Sinister Soldier gains SCH and ATK from attached powers" do
    tg <- sinisterGame Normal
    inGame tg do
      let p = player1 tg
      soldier <- addMinion p Minions.ne40145
      mods <- getModifiers (MinionRef soldier)
      mods `shouldSatisfy` \xs -> AttackModifier 1 `elem` xs && SchemeModifier 1 `elem` xs
      advanceCurrent
      mods' <- getModifiers (MinionRef soldier)
      mods' `shouldSatisfy` \xs -> AttackModifier 2 `elem` xs && SchemeModifier 2 `elem` xs

  it "Teleported Away prevents damage and converts an attack into a scheme" do
    tg <- sinisterGame Normal
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      card <- encounterCard $ Schemes.ne40146
      resolve [RevealEncounterCard p card]
      before <- mainSchemeThreatNow
      vid <- villainIdNow
      resolve [VillainMessage vid $ VillainDamaged (IdentityRef p) (toDamage 3 $ FromPlayerAttack p)]
      villainDamageNow `shouldReturn` 0
      -- The replacement scheme draws a boost even though the replaced attack
      -- was requested without one. Keep it below the stage's completion threshold,
      -- leaving a second card so the encounter deck is not exhausted empty.
      _ <- setEncounterDeck [Treacheries.cable40035, Treacheries.cable40036] []
      villain <- getVillain
      amount <- Villain.getModifiedScheme (toAttrs villain)
      selectAny (SideSchemeWithId $ sideSchemeOf card) `shouldReturn` True
      resolve [VillainMessage vid $ VillainAttacksWithoutBoost p]
      mainSchemeThreatNow `shouldReturn` before + amount

  describe "Flight" do
    it "grants Aerial, overkill and its printed ATK bonus" do
      tg <- testGame ["spider-man"]
      inGame tg do
        let p = player1 tg
        _ <- revealNow p Attachments.ne40151
        settle
        vid <- villainIdNow
        selectAny (VillainWithId vid <> VillainWithTrait Aerial) `shouldReturn` True
        getModifiers (VillainRef vid) >>= \mods -> mods `shouldSatisfy` elem (AttackModifier 1)
        villain <- getVillain
        Villain.getModifiedAttack (toAttrs villain) `shouldReturn` 3

    it "Aerial Bombardment can be removed by exhausting a hero and paying mental" do
      tg <- testGame ["spider-man"]
      inGame tg do
        let p = player1 tg
        changeForm p
        pass
        clearHand p
        card <- revealNow p Attachments.ne40152
        settle
        mental <- putInHand p "01089"
        choose "remove Aerial Bombardment" (isAbilityOf $ attachmentIdOf card)
        payWith [mental]
        pass
        attachmentsWithCode "40152" `shouldReturn` []
        isExhaustedNow p `shouldReturn` True

    it "Out of Reach prevents ordinary and non-attack damage" do
      tg <- testGame ["spider-man"]
      inGame tg do
        let p = player1 tg
        _ <- revealNow p Attachments.ne40153
        settle
        vid <- villainIdNow
        resolve [VillainMessage vid $ VillainDamaged (IdentityRef p) (toDamage 2 FromAbility)]
        resolve [VillainMessage vid $ VillainDamaged (IdentityRef p) (toDamage 2 $ FromPlayerAttack p)]
        villainDamageNow `shouldReturn` 0

    it "Out of Reach permits a ranged attack" do
      tg <- testGame ["spider-man"]
      inGame tg do
        let p = player1 tg
        _ <- revealNow p Attachments.ne40153
        settle
        vid <- villainIdNow
        resolve [VillainMessage vid $ VillainDamaged (IdentityRef p) (withRanged $ toDamage 2 $ FromPlayerAttack p)]
        villainDamageNow `shouldReturn` 2

    it "High Ground is a treachery and strips all friendly tough before indirect damage" do
      cdCardType Treacheries.ne40154 `shouldBe` TreacheryType
      tg <- testGame ["spider-man"]
      inGame tg do
        let p = player1 tg
        resolve [GiveStatus (IdentityRef p) ToughStatus]
        before <- damageOf p
        card <- encounterCard Treacheries.ne40154
        resolve [RevealEncounterCard p card]
        playerIdentityTough . toAttrs <$> getPlayer p `shouldReturn` False
        damageOf p `shouldReturn` before + 4

  describe "Super Strength" do
    it "grants Brute and steady with a printed ATK bonus" do
      tg <- testGame ["spider-man"]
      inGame tg do
        _ <- revealNow (player1 tg) Attachments.ne40155
        settle
        vid <- villainIdNow
        selectAny (VillainWithId vid <> VillainWithTrait Brute) `shouldReturn` True
        resolve [GiveStatus (VillainRef vid) StunnedStatus]
        attrs <- toAttrs <$> getVillain
        Villain.isStunned attrs `shouldReturn` False
        resolve [GiveStatus (VillainRef vid) StunnedStatus]
        attrs' <- toAttrs <$> getVillain
        Villain.isStunned attrs' `shouldReturn` True

    it "Impervious reduces each attack by one and can be discarded with physical resources" do
      tg <- testGame ["spider-man"]
      inGame tg do
        let p = player1 tg
        changeForm p
        pass
        clearHand p
        card <- revealNow p Attachments.ne40156
        settle
        vid <- villainIdNow
        resolve [VillainMessage vid $ VillainDamaged (IdentityRef p) (toDamage 3 $ FromPlayerAttack p)]
        villainDamageNow `shouldReturn` 2
        pay <- putInHand p "01090"
        choose "remove Impervious" (isAbilityOf $ attachmentIdOf card)
        payWith [pay]
        pass
        attachmentsWithCode "40156" `shouldReturn` []
        Villain.villainIsTough <$> getVillain `shouldReturn` True

    it "Thrown Object leaves after an attack even if it dealt no damage" do
      tg <- testGame ["spider-man"]
      inGame tg do
        let p = player1 tg
        _ <- revealNow p Attachments.ne40157
        settle
        resolve [GiveStatus (IdentityRef p) ToughStatus]
        vid <- villainIdNow
        resolve [VillainMessage vid $ VillainAttacksWithoutBoost p]
        attachmentsWithCode "40157" `shouldReturn` []

    it "I'll Take That surges when there is no upgrade to discard" do
      tg <- testGame ["spider-man"]
      inGame tg do
        let p = player1 tg
        top <- testHead <$> stackEncounterDeck [Treacheries.imTough]
        card <- encounterCard Treacheries.ne40158
        resolve [RevealEncounterCard p card]
        playerIdentityEncounterCards . toAttrs <$> getPlayer p `shouldReturn` [top]

  describe "Telepathy" do
    it "grants Psionic and retaliate with its printed SCH bonus" do
      tg <- testGame ["spider-man"]
      inGame tg do
        _ <- revealNow (player1 tg) Attachments.ne40159
        settle
        vid <- villainIdNow
        selectAny (VillainWithId vid <> VillainWithTrait Psionic) `shouldReturn` True
        villain <- getVillain
        Villain.getModifiedScheme (toAttrs villain) `shouldReturn` 2

    it "Manufactured Drama exhausts supports and prevents readying until discarded" do
      tg <- testGame ["spider-man"]
      inGame tg do
        let p = player1 tg
        support <- putIntoPlay p "40007"
        card <- encounterCard Obligations.ne40160
        resolve [RevealEncounterCard p card]
        mods <- getModifiers (SupportRef $ supportOf support)
        mods `shouldSatisfy` elem CannotReady
        resolve [SupportMessage (supportOf support) ReadiedSupport]
        getsGame (maybe False (supportExhausted . toAttrs) . HashMap.lookup (supportOf support) . gameSupports) `shouldReturn` True

    it "Sowing Discord exhausts allies and prevents readying" do
      tg <- testGame ["spider-man"]
      inGame tg do
        let p = player1 tg
        ally <- putIntoPlay p "01083"
        card <- encounterCard Obligations.ne40161
        resolve [RevealEncounterCard p card]
        mods <- getModifiers (AllyRef $ allyIdOf ally)
        mods `shouldSatisfy` elem CannotReady
        resolve [AllyMessage (allyIdOf ally) ReadiedAlly]
        allyIn "01083" >>= \a -> allyExhausted a `shouldBe` True

    it "only Sowing Discord's owner can pay its alter-ego action to discard it" do
      tg <- testGame ["spider-man", "captain-marvel"]
      inGame tg do
        let first = player1 tg
            owner = player2 tg
        ally <- putIntoPlay owner "01083"
        card <- encounterCard Obligations.ne40161
        resolve [RevealEncounterCard owner card]
        let oid = ObligationId $ unCardId $ ecCardId card
        clearHand first
        clearHand owner
        genius <- putInHand owner "01089"
        -- The first player's turn cannot use another player's obligation.
        offers (isAbilityOf $ ObligationRef oid) `shouldReturn` False
        endTurn
        getActivePlayerId `shouldReturn` owner
        offers (isAbilityOf $ ObligationRef oid) `shouldReturn` True
        choose "owner pays Sowing Discord" (isAbilityOf $ ObligationRef oid)
        payWith [genius]
        pass
        getsGame (HashMap.member oid . gameObligations) `shouldReturn` False
        map toCardCode <$> discardOf owner >>= \codes -> codes `shouldSatisfy` elem "01089"
        getModifiers (AllyRef $ allyIdOf ally) >>= \mods -> mods `shouldSatisfy` notElem CannotReady
        resolve [AllyMessage (allyIdOf ally) ReadiedAlly]
        allyIn "01083" >>= \attrs -> allyExhausted attrs `shouldBe` False

    it "Manufactured Drama's alter-ego action exhausts its owner and discards per support" do
      tg <- testGame ["spider-man"]
      inGame tg do
        let owner = player1 tg
        first <- putIntoPlay owner "40007"
        second <- putIntoPlay owner "01091"
        card <- encounterCard Obligations.ne40160
        resolve [RevealEncounterCard owner card]
        let oid = ObligationId $ unCardId $ ecCardId card
        clearHand owner
        [top, next, remaining] <- stackDeck owner ["01088", "01089", "01090"]
        choose "pay Manufactured Drama" (isAbilityOf $ ObligationRef oid)
        pass
        getsGame (HashMap.member oid . gameObligations) `shouldReturn` False
        isExhaustedNow owner `shouldReturn` True
        discarded <- discardOf owner
        discarded `shouldSatisfy` (\cards -> top `elem` cards && next `elem` cards && remaining `notElem` cards)
        resolve [SupportMessage (supportOf first) ReadiedSupport, SupportMessage (supportOf second) ReadiedSupport]
        for_ [supportOf first, supportOf second] $ \sid ->
          getsGame (fmap (supportExhausted . toAttrs) . HashMap.lookup sid . gameSupports) `shouldReturn` Just False

    it "One Step Ahead discards one random hand card without Aerial" do
      tg <- testGame ["spider-man"]
      inGame tg do
        let p = player1 tg
        clearHand p
        _ <- putInHand p "01088"
        _ <- putInHand p "01089"
        card <- encounterCard Treacheries.ne40162
        resolve [RevealEncounterCard p card]
        length <$> handOf p `shouldReturn` 1

  describe "central enemy damage windows" do
    it "simultaneous damage removes each enemy's tough without reaching its force field" do
      tg <- testGame ["spider-man"]
      inGame tg do
        let p = player1 tg
        soldier <- addMinion p Minions.hydraSoldier
        _ <- revealNow p Attachments.cable40034
        settle
        minionField <- encounterCard Attachments.cable40034
        vid <- villainIdNow
        resolve [AttachEncounterCard minionField (EnemyMinionId soldier)]
        resolve [GiveStatus (VillainRef vid) ToughStatus, GiveStatus (MinionRef soldier) ToughStatus]
        liftIO $ writeIORef tg.ran []
        resolve [Simultaneously
          [ VillainMessage vid $ VillainDamaged (IdentityRef p) (toDamage 2 FromRetaliate)
          , MinionMessage soldier $ MinionDamaged (IdentityRef p) (toDamage 2 FromRetaliate)
          ]]
        villainDamageNow `shouldReturn` 0
        Villain.villainIsTough <$> getVillain `shouldReturn` False
        getsGame (fmap (minionDamage . toAttrs) . HashMap.lookup soldier . gameMinions) `shouldReturn` Just 0
        getsGame (fmap (minionTough . toAttrs) . HashMap.lookup soldier . gameMinions) `shouldReturn` Just False
        length <$> attachmentsWithCode "40034" `shouldReturn` 2
        history <- liftIO $ readIORef tg.ran
        let takenWindows = filter (\line -> "CheckWindows " `T.isPrefixOf` line && "EnemyWouldBeDamaged" `T.isInfixOf` line) history
        length takenWindows `shouldBe` 2
        takenWindows `shouldSatisfy` all (T.isInfixOf "damageAmount = 0")

    it "all simultaneous damage lands before the first enemy's when-defeated effect" do
      tg <- testGame ["spider-man"]
      inGame tg do
        let p = player1 tg
        first <- addMinion p Minions.hydraSoldier
        second <- addMinion p Minions.hydraSoldier
        waiting <- openQuestion
        withGame_ $ questionL .~ mempty
        pushAll $ [Simultaneously
          [ MinionMessage first $ MinionDamaged (IdentityRef p) (toDamage 4 FromRetaliate)
          , MinionMessage second $ MinionDamaged (IdentityRef p) (toDamage 4 FromRetaliate)
          ]] <> [Ask you question | Just (you, question) <- [waiting]]
        runGame
        -- The explicit when-defeated question pauses before Hydra Soldier deals
        -- its encounter card. Both enemies already have their lethal damage.
        asked >>= \case
          Just (_, choices) -> choices `shouldSatisfy` any (isTargetChoice first)
          Nothing -> error "expected Hydra Soldier's when-defeated question"
        getsGame (fmap (minionDamage . toAttrs) . HashMap.lookup first . gameMinions) `shouldReturn` Just 4
        getsGame (fmap (minionDamage . toAttrs) . HashMap.lookup second . gameMinions) `shouldReturn` Just 4
        settle
        getsGame (HashMap.member first . gameMinions) `shouldReturn` False
        getsGame (HashMap.member second . gameMinions) `shouldReturn` False

    it "consumes tough before evaluating the next simultaneous taken-damage interrupt" do
      tg <- testGame ["spider-man"]
      inGame tg do
        let p = player1 tg
        _ <- revealNow p Attachments.cable40034
        settle
        vid <- villainIdNow
        resolve [GiveStatus (VillainRef vid) ToughStatus]
        resolve [Simultaneously
          [ VillainMessage vid $ VillainDamaged (IdentityRef p) (toDamage 1 FromRetaliate)
          , VillainMessage vid $ VillainDamaged (IdentityRef p) (toDamage 2 FromRetaliate)
          ]]
        villainDamageNow `shouldReturn` 0
        Villain.villainIsTough <$> getVillain `shouldReturn` False
        attachmentsWithCode "40034" `shouldReturn` []

    it "Sinister's Disguise redirects player damage before the villain's tough is consumed" do
      tg <- testGame ["spider-man"]
      inGame tg do
        let p = player1 tg
        clearHand p
        _ <- revealNow p Attachments.ne40144
        settle
        vid <- villainIdNow
        resolve [GiveStatus (VillainRef vid) ToughStatus]
        before <- damageOf p
        resolve [VillainMessage vid $ VillainDamaged (IdentityRef p) (toDamage 2 FromAbility)]
        villainDamageNow `shouldReturn` 0
        Villain.villainIsTough <$> getVillain `shouldReturn` True
        damageOf p `shouldReturn` before + 2
        attachmentsWithCode "40144" `shouldReturn` []

    it "Telekinetic Force Field prevents direct retaliate messages" do
      tg <- testGame ["spider-man"]
      inGame tg do
        let p = player1 tg
        _ <- revealNow p Attachments.cable40034
        settle
        vid <- villainIdNow
        resolve [VillainMessage vid $ VillainDamaged (IdentityRef p) (toDamage 1 FromRetaliate)]
        villainDamageNow `shouldReturn` 0
        length <$> attachmentsWithCode "40034" `shouldReturn` 1
        resolve [VillainMessage vid $ VillainDamaged (IdentityRef p) (toDamage 2 FromRetaliate)]
        villainDamageNow `shouldReturn` 0
        attachmentsWithCode "40034" `shouldReturn` []

    it "a discarded force field does not protect the second simultaneous damage" do
      tg <- testGame ["spider-man"]
      inGame tg do
        let p = player1 tg
        _ <- revealNow p Attachments.cable40034
        settle
        vid <- villainIdNow
        resolve [Simultaneously
          [ VillainMessage vid $ VillainDamaged (IdentityRef p) (toDamage 2 FromRetaliate)
          , VillainMessage vid $ VillainDamaged (IdentityRef p) (toDamage 1 FromRetaliate)
          ]]
        villainDamageNow `shouldReturn` 1
        attachmentsWithCode "40034" `shouldReturn` []

    it "legacy insteadOf armor still intercepts a wrapped retaliate instance" do
      tg <- testGame ["spider-man"]
      inGame tg do
        let p = player1 tg
        _ <- revealNow p Attachments.armoredRhinoSuit
        settle
        vid <- villainIdNow
        resolve [VillainMessage vid $ VillainDamaged (IdentityRef p) (toDamage 2 FromRetaliate)]
        villainDamageNow `shouldReturn` 0
