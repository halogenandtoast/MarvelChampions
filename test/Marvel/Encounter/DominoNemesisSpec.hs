module Marvel.Encounter.DominoNemesisSpec (spec) where
import Data.HashMap.Strict qualified as HashMap
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Attachment.Types hiding (AttachmentId)
import Marvel.Hp
import Marvel.Domino
import Marvel.Minion.Cards qualified as Minions
import Marvel.Minion.Types
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.SideScheme.Cards qualified as Schemes
import TestImport

attachmentAbility :: EncounterCard -> Natural -> Choice -> Bool
attachmentAbility card idx = \case
  UseAbility ab -> abilitySource ab == AttachmentRef (AttachmentId $ unCardId $ ecCardId card) && abilityIndex ab == idx
  _ -> False
asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass >> clearHand p
finishForced :: GameAppT ()
finishForced = do
  pass
  openQuestion >>= \case
    Just (you,QuestionLabel "$label.resolveForced" _ _) -> answerQuestion you 0 >> finishForced
    _ -> pure ()
spec :: Spec
spec = describe "Domino nemesis" do
  it "Topaz finds Superpower Feedback in the nemesis set aside area and attaches it" do
    tg <- testGame ["domino"]
    inGame tg do
      let p = player1 tg
      _ <- revealNow p Minions.topaz
      finishForced
      attachments <- getsGame (map toAttrs . HashMap.elems . gameAttachments)
      attachments `shouldSatisfy` any (\a -> toCardCode a == "40069" && attachmentIdentity a == Just p)
  it "Prototype gains one luck counter per damage sustained, increasing its hit points" do
    tg <- testGame ["domino"]
    inGame tg do
      let p = player1 tg
      setDamage p 4
      prototype <- minionOf <$> revealNow p Minions.prototype
      finishForced
      getCountersOn prototype `shouldReturn` 4
      host <- getsGame $ toAttrs . (HashMap.! prototype) . gameMinions
      minionHitPoints host `shouldBe` HP 1
  it "Prototype's luck counters do not track later changes in identity damage" do
    tg <- testGame ["domino"]
    inGame tg do
      let p = player1 tg
      setDamage p 2
      prototype <- minionOf <$> revealNow p Minions.prototype
      finishForced
      setDamage p 5
      getCountersOn prototype `shouldReturn` 2
  it "Not My Lucky Day lets each player separately choose damage or threat" do
    tg <- testGame ["domino", "spider-man"]
    inGame tg do
      let p = player1 tg; other = player2 tg
      scheme <- sideSchemeOf <$> revealNow p Schemes.notMyLuckyDay
      choose "first takes damage" (isLabel "$label.dominoTakeDamage")
      choose "second places threat" (isLabel "$label.dominoPlaceThreat")
      finishForced
      damageOf p `shouldReturn` 1
      damageOf other `shouldReturn` 0
      sideSchemeThreat scheme `shouldReturn` 5
  it "Memories blanks Domino's wild doubling while retaining traits and is removable in alter ego" do
    tg <- testGame ["domino"]
    inGame tg do
      let p = player1 tg
      asHero p
      card <- putInHand p "40040"
      obligation <- revealNow p Obligations.memoriesofArmageddon
      finishForced
      discardedResourceCount p [PlayerCard card] `shouldReturn` 1
      offers (\case UseAbility ab -> abilitySource ab == toSource p && abilityIndex ab == 1; _ -> False) `shouldReturn` False
      runNow [IdentityEndedTurn p, EndRound]
      finishForced
      changeForm p
      finishForced
      choose "remove Memories" (\case UseAbility ab -> abilitySource ab == ObligationRef (ObligationId $ unCardId $ ecCardId obligation); _ -> False)
      finishForced
      exhaustedNow p `shouldReturn` True
  it "Superpower Feedback deals damage after a resolved identity action but not a constant" do
    tg <- testGame ["domino"]
    inGame tg do
      let p = player1 tg
      asHero p
      _ <- revealNow p Attachments.superpowerFeedback
      finishForced
      wild <- putInHand p "40040"
      discardedResourceCount p [PlayerCard wild] `shouldReturn` 2
      damageOf p `shouldReturn` 0
      choose "Domino swaps" (\case UseAbility ab -> abilitySource ab == toSource p && abilityIndex ab == 1; _ -> False)
      choose "swap hand card" (isTargetChoice wild)
      finishForced
      damageOf p `shouldReturn` 1
  it "Superpower Feedback can be discarded in alter ego with exactly one signature card" do
    tg <- testGame ["domino"]
    inGame tg do
      let p = player1 tg
      clearHand p
      feedback <- revealNow p Attachments.superpowerFeedback
      finishForced
      signature <- putInHand p "40040"
      basic <- putInHand p "01088"
      choose "remove Feedback" (attachmentAbility feedback 2)
      offers (isTargetChoice basic) `shouldReturn` False
      choose "discard signature" (isTargetChoice signature)
      finishForced
      discardOf p >>= (`shouldSatisfy` elem signature)
      handOf p >>= (`shouldSatisfy` elem basic)
      getsGame (HashMap.member (AttachmentId $ unCardId $ ecCardId feedback) . gameAttachments) `shouldReturn` False
  it "Superpower Feedback does not answer a basic power game function" do
    tg <- testGame ["domino"]
    inGame tg do
      let p = player1 tg
      asHero p
      _ <- revealNow p Attachments.superpowerFeedback
      finishForced
      choose "basic attack" (\case UseAbility ab -> abilitySource ab == toSource p && abilityIndex ab == 301; _ -> False)
      chooseVillain
      finishForced
      damageOf p `shouldReturn` 0
  it "Superpower Feedback is not triggered by a basic card's ability" do
    tg <- testGame ["domino"]
    inGame tg do
      let p = player1 tg
      asHero p
      _ <- revealNow p Attachments.superpowerFeedback
      finishForced
      bear <- putIntoPlay p "40056"
      _ <- stackDeck p ["01088"]
      choose "Atlas Bear looks" (isAbilityOfCard bear)
      choose "look at own deck" (isTargetChoice p)
      choose "finish looking" (isLabel "$label.done")
      finishForced
      damageOf p `shouldReturn` 0
