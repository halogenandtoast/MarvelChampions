module Marvel.Rules.DeadpoolMechanicsSpec (spec) where

import Marvel.Ability (passesCardCriteria)
import Marvel.ActiveCost.Types
import Marvel.Cost
    ( Cost(MainSchemeAccelerationTokenCost, ExhaustAnyIdentityCost) )
import Marvel.Criteria (Criteria (NoCardsPlayedThisPhase))
import Marvel.Message.Lifted qualified as Lifted
import Marvel.Payment.Types
import Marvel.Upgrade.Types qualified as Upgrade
import TestImport

spec :: Spec
spec = describe "Deadpool shared mechanics" do
  it "exhausts a chosen other identity and records whose identity paid" do
    tg <- testGame ["spider-man", "captain-marvel"]
    inGame tg do
      let you = player1 tg
          other = player2 tg
      acid <- getRandom
      withGame_ $ activeCostL .~ [ActiveCost acid you ForTreachery ExhaustAnyIdentityCost NoPayment Nothing [] Nothing]
      runNow [PayCost acid ExhaustAnyIdentityCost]
      choose "exhaust the other player" (isTargetChoice other)
      playerIdentityExhausted . toAttrs <$> getPlayer other `shouldReturn` True
      playerIdentityExhausted . toAttrs <$> getPlayer you `shouldReturn` False
      receipt <- getCurrentPayment
      [who | ExhaustedIdentityPayment who <- payments receipt] `shouldBe` [other]

  it "places one physical main-scheme acceleration token as a cost" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      acid <- getRandom
      withGame_ $ activeCostL .~ [ActiveCost acid you ForTreachery MainSchemeAccelerationTokenCost NoPayment Nothing [] Nothing]
      runNow [PayCost acid MainSchemeAccelerationTokenCost]
      getMainSchemeTokenCount `shouldReturn` 1
      getEncounterIconCount `shouldReturn` 0

  it "checks card-play history for this player and this phase" do
    tg <- testGame ["spider-man", "captain-marvel"]
    inGame tg do
      let you = player1 tg
          other = player2 tg
      card <- putInHand you "01005"
      passesCardCriteria you card NoCardsPlayedThisPhase `shouldReturn` True
      withGame_ $ playedThisPhaseL .~ [(other, getCardDef card)]
      passesCardCriteria you card NoCardsPlayedThisPhase `shouldReturn` True
      withGame_ $ playedThisPhaseL .~ [(you, getCardDef card)]
      passesCardCriteria you card NoCardsPlayedThisPhase `shouldReturn` False

  it "stores a prior-game outcome in the identity's saved state" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      getPreviousGameWon you `shouldReturn` Nothing
      runNow [CardSpecific (IdentityRef you) "previousGameWon" (toJSON False)]
      getPreviousGameWon you `shouldReturn` Just False
      player <- getPlayer you
      case fromJSON (toJSON $ toAttrs player) of
        Success (loaded :: Attrs PlayerIdentity) -> playerIdentityPreviousGameWon loaded `shouldBe` Just False
        Error err -> liftIO $ expectationFailure err

  it "moves damage into one saved board slot and cannot fill that slot twice" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      card <- putIntoPlay you "01007"
      let uid = upgradeIdOf card
      setDamage you 3
      configure <- Lifted.runQueueT $ Lifted.capture $ Lifted.configureResourceSlots uid [Energy, Mental, Physical]
      runNow configure
      movement <- Lifted.runQueueT $ Lifted.capture $ Lifted.moveDamageToUpgradeSlot uid (IdentityCharacter you) 1
      runNow movement
      damageOf you `shouldReturn` 2
      map Upgrade.upgradeOccupiedSlots <$> upgradesWith "01007" `shouldReturn` [[1]]
      runNow movement
      damageOf you `shouldReturn` 2
      map Upgrade.upgradeResourceSlots <$> upgradesWith "01007" `shouldReturn` [[Energy, Mental, Physical]]

  it "keeps a facedown held upgrade card unavailable for play" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      host <- putIntoPlay you "01007"
      held <- putInHand you "01093"
      messages <- Lifted.runQueueT $ Lifted.capture $ Lifted.holdFacedownCard (upgradeIdOf host) held
      runNow messages
      getHeldPlayableCards you `shouldReturn` []
      map Upgrade.upgradeHeldCards <$> upgradesWith "01007" `shouldReturn` [[held]]
