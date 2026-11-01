module Marvel.Rules.DeadpoolDamageCostSpec (spec) where

import Marvel.ActiveCost.Types
import Marvel.Cost qualified as Cost
import Marvel.Message.Lifted qualified as Lifted
import Marvel.Payment.Types
import TestImport

startDamageContinuation :: IdentityId -> [Message] -> GameAppT ()
startDamageContinuation you continuation = do
  messages <- Lifted.runQueueT $ Lifted.capture $
    Lifted.payCostThen you (IdentityRef you) Cost.UpToRemainingHitPointsDamageCost continuation
  runNow messages

spec :: Spec
spec = describe "Deadpool variable damage costs" do
  it "offers amounts from one through all remaining hit points, without a zero option" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      clearHand you
      hp <- hitPointsOf you
      setDamage you (hp - 3)
      startDamageContinuation you []
      for_ [1, 2, 3] \n -> offers (isLabel $ countLabel "takeDamage" n) `shouldReturn` True
      offers (isLabel $ countLabel "takeDamage" 0) `shouldReturn` False
      offers (isLabel $ countLabel "takeDamage" 4) `shouldReturn` False

  it "records the chosen amount as damage actually paid" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      clearHand you
      acid <- getRandom
      let cost = Cost.UpToRemainingHitPointsDamageCost
      withGame_ $ activeCostL .~ [ActiveCost acid you ForTreachery cost NoPayment Nothing [] Nothing]
      runNow [PayCost acid cost]
      choose "take two damage" (isLabel $ countLabel "takeDamage" 2)
      damageOf you `shouldReturn` 2
      receipt <- getCurrentPayment
      [n | DamagePayment n <- payments receipt] `shouldBe` [2]
      [n | PendingDamageCostPayment n _ <- payments receipt] `shouldBe` []

  it "finishes taking damage before running the paid continuation" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      clearHand you
      startDamageContinuation you [IdentityMessage you (IdentityHealed 1)]
      choose "take two damage" (isLabel $ countLabel "takeDamage" 2)
      damageOf you `shouldReturn` 1

  it "aborts the continuation if an interrupt prevents any of the damage cost" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      clearHand you
      setDamage you 2
      stance <- putIntoPlay you "08032"
      startDamageContinuation you [IdentityMessage you (IdentityHealed 2)]
      choose "take four damage" (isLabel $ countLabel "takeDamage" 4)
      choose "prevent three damage" (isAbilityOfCard stance)
      pass
      damageOf you `shouldReturn` 3

  it "allows the last hit point to be paid and resolves defeat replacement before the continuation" do
    tg <- testGame ["captain-america"]
    inGame tg do
      let you = player1 tg
      changeForm you
      clearHand you
      helmet <- putIntoPlay you "03008"
      hp <- hitPointsOf you
      setDamage you (hp - 1)
      startDamageContinuation you [IdentityMessage you (IdentityHealed 1)]
      choose "pay the last hit point" (isLabel $ countLabel "takeDamage" 1)
      choose "replace defeat with one hit point" (isAbilityOfCard helmet)
      pass
      damageOf you `shouldReturn` (hp - 2)

  it "cannot start without remaining hit points or through a tough status card" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
          affordable = Cost.canAffordCost you (IdentityRef you) Cost.UpToRemainingHitPointsDamageCost
      affordable `shouldReturn` True
      resolve [IdentityMessage you IdentityBecomeTough]
      affordable `shouldReturn` False
      resolve [IdentityMessage you $ IdentityDamaged GameRef $ toDamage 1 FromAbility]
      affordable `shouldReturn` True
      hp <- hitPointsOf you
      setDamage you hp
      affordable `shouldReturn` False
