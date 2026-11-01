module Marvel.Encounter.DeadpoolNemesisSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Attachment.Types qualified as Attachment
import Marvel.Minion.Cards qualified as Minions
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Modifier
import Marvel.Matchers (identityMatches)
import Marvel.Matchers.Types
import Marvel.Phase
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import TestImport

settle :: GameAppT ()
settle = go (100 :: Int)
 where
  go 0 = error "Deadpool nemesis fixture did not settle"
  go n = openQuestion >>= \case
    Just (_, QuestionLabel "$label.yourTurn" _ _) -> pure ()
    Just (_, QuestionLabel "$label.didYouTalkThisPhase" _ _) -> pure ()
    Just (you, q) -> do
      let passing c = isNoResponse c || isNoDefender c || isLabel "$label.declineToDefend" c
      answerQuestion you (fromMaybe 0 $ headMay [i | (i,c) <- zip [0..] (choicesOf q), passing c])
      go (n - 1)
    Nothing -> pure ()

attachmentOf :: EncounterCard -> AttachmentId
attachmentOf = AttachmentId . unCardId . ecCardId

boostNow :: CardDef -> GameAppT ()
boostNow def = do
  enemy <- EnemyVillainId <$> villainIdNow
  card <- encounterCard def
  runNow [RevealBoostCard card enemy]
  settle

spec :: Spec
spec = describe "Deadpool nemesis" do
  describe "The Merc with the Mouth" do
    it "exhausts its owner's allies and prevents those allies from readying" do
      tg <- testGame ["deadpool", "spider-man"]
      inGame tg do
        let you = player1 tg
            other = player2 tg
        mine <- putIntoPlay you "01084"
        theirs <- putIntoPlay other "01076"
        _ <- revealNow you Obligations.theMercWithTheMouth
        settle
        resolve [AllyMessage (allyIdOf mine) ReadiedAlly]
        getModifiers (AllyRef $ allyIdOf mine) >>= (`shouldSatisfy` elem CannotReady)
        getModifiers (AllyRef $ allyIdOf theirs) >>= (`shouldSatisfy` notElem CannotReady)
        allies <- alliesWith "01084"
        [allyExhausted a | a <- allies, allyId a == allyIdOf mine] `shouldBe` [True]
        theirsAttrs <- alliesWith "01076"
        map allyExhausted theirsAttrs `shouldBe` [False]

    it "blocks other players' card abilities only during its owner's turn" do
      tg <- testGame ["deadpool", "spider-man"]
      inGame tg do
        let you = player1 tg
            other = player2 tg
        _ <- revealNow you Obligations.theMercWithTheMouth
        settle
        canResolvePlayerCardAbilities other `shouldReturn` False
        canResolvePlayerCardAbilities you `shouldReturn` True
        resolve [SetActivePlayer other]
        canResolvePlayerCardAbilities other `shouldReturn` True

    for_ [True, False] $ \talked ->
      it (if talked then "keeps the obligation after a report of talking" else "discards it after a report of silence") do
        tg <- testGame ["deadpool"]
        inGame tg do
          let you = player1 tg
          _ <- revealNow you Obligations.theMercWithTheMouth
          settle
          runNow [CheckWindows [W.Window W.After $ W.PhaseEnded PlayerPhase]]
          settle
          choose "report talking" (isLabel $ if talked then "$label.talkedThisPhase" else "$label.didNotTalkThisPhase")
          settle
          obligationsInPlay `shouldReturn` (if talked then 1 else 0)

  describe "Butler" do
    it "places its scheme threat on Involuntary Procedures when present" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        scheme <- revealNow you Schemes.involuntaryProcedures
        settle
        minion <- addMinion you Minions.butler
        before <- mainSchemeThreat
        resolve [MinionMessage minion MinionSchemes]
        sideSchemeThreat (sideSchemeOf scheme) `shouldReturn` 8
        mainSchemeThreat `shouldReturn` before

    it "schemes on the main scheme if Involuntary Procedures is absent" do
      tg <- testGame ["deadpool"]
      inGame tg do
        minion <- addMinion (player1 tg) Minions.butler
        before <- mainSchemeThreat
        resolve [MinionMessage minion MinionSchemes]
        mainSchemeThreat `shouldReturn` before + 2

    it "confuses the attacked player as a boost" do
      tg <- testGame ["deadpool"]
      inGame tg do
        boostNow Minions.butler
        isConfused (player1 tg) `shouldReturn` True

  describe "Involuntary Procedures" do
    it "places one threat per positive damage instance rather than per damage point" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        changeForm you
        scheme <- revealNow you Schemes.involuntaryProcedures
        settle
        runNow [IdentityMessage you $ IdentityDamaged GameRef (toDamage 3 FromAbility)]
        settle
        sideSchemeThreat (sideSchemeOf scheme) `shouldReturn` 7
        runNow [IdentityMessage you $ IdentityDamaged GameRef (toDamage 1 FromAbility)]
        settle
        sideSchemeThreat (sideSchemeOf scheme) `shouldReturn` 8

    it "does not trigger after Wade Wilson or another identity takes damage" do
      tg <- testGame ["deadpool", "spider-man"]
      inGame tg do
        let you = player1 tg
        scheme <- revealNow you Schemes.involuntaryProcedures
        settle
        runNow [IdentityMessage you $ IdentityDamaged GameRef (toDamage 1 FromAbility)]
        settle
        runNow [IdentityMessage (player2 tg) $ IdentityDamaged GameRef (toDamage 1 FromAbility)]
        settle
        sideSchemeThreat (sideSchemeOf scheme) `shouldReturn` 6

    it "removes itself from the game at ten after its damage-triggered placement" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        changeForm you
        scheme <- revealNow you Schemes.involuntaryProcedures
        settle
        resolve [SideSchemeMessage (sideSchemeOf scheme) $ SideSchemePlaceThreat 3]
        runNow [IdentityMessage you $ IdentityDamaged GameRef (toDamage 1 FromAbility)]
        settle
        getsGame (HashMap.member (sideSchemeOf scheme) . gameSideSchemes) `shouldReturn` False
        encounterDiscardCodes >>= (`shouldSatisfy` notElem "44034")

    it "stays in play when another effect raises its threat to ten" do
      tg <- testGame ["deadpool"]
      inGame tg do
        scheme <- revealNow (player1 tg) Schemes.involuntaryProcedures
        settle
        resolve [SideSchemeMessage (sideSchemeOf scheme) $ SideSchemePlaceThreat 4]
        sideSchemeThreat (sideSchemeOf scheme) `shouldReturn` 10

  describe "Tabula Rasa 16" do
    it "attaches to the identity and blanks printed abilities while retaining traits" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        changeForm you
        card <- revealNow you Attachments.tabulaRasa16
        settle
        getsGame (fmap (Attachment.attachmentIdentity . toAttrs) . HashMap.lookup (attachmentOf card) . gameAttachments) `shouldReturn` Just (Just you)
        getModifiers (IdentityRef you) >>= (`shouldSatisfy` elem Blank)
        identityMatches (IdentityWithTrait Trait.DeadpoolCorps) you `shouldReturn` True
        offers (isAbilityOf $ attachmentOf card) `shouldReturn` False

    it "allows an alter ego to spend two mental resources to discard it" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        card <- revealNow you Attachments.tabulaRasa16
        settle
        first <- putInHand you "01089"
        choose "discard Tabula Rasa" (isAbilityOf $ attachmentOf card)
        payWith [first]
        settle
        getsGame (HashMap.member (attachmentOf card) . gameAttachments) `shouldReturn` False
        getModifiers (IdentityRef you) >>= (`shouldSatisfy` notElem Blank)
        encounterDiscardCodes >>= (`shouldSatisfy` elem "44035")

    it "attaches to the attacked identity when revealed as a boost" do
      tg <- testGame ["deadpool"]
      inGame tg do
        boostNow Attachments.tabulaRasa16
        getModifiers (IdentityRef $ player1 tg) >>= (`shouldSatisfy` elem Blank)

  describe "Mutated Soldier" do
    it "enters with tough and heals all damage after scheming" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        mid <- addMinion you Minions.mutatedSoldier
        attrs <- getsGame $ toAttrs . fromMaybe (error "missing soldier") . HashMap.lookup mid . gameMinions
        minionTough attrs `shouldBe` True
        resolve [RemoveStatus (MinionRef mid) ToughStatus]
        resolve [MinionMessage mid $ MinionDamaged GameRef (toDamage 3 FromAbility)]
        minionDamageOf mid `shouldReturn` Just 3
        resolve [MinionMessage mid MinionSchemes]
        minionDamageOf mid `shouldReturn` Just 0

    it "does not heal merely after taking damage" do
      tg <- testGame ["deadpool"]
      inGame tg do
        mid <- addMinion (player1 tg) Minions.mutatedSoldier
        resolve [RemoveStatus (MinionRef mid) ToughStatus]
        resolve [MinionMessage mid $ MinionDamaged GameRef (toDamage 2 FromAbility)]
        minionDamageOf mid `shouldReturn` Just 2
