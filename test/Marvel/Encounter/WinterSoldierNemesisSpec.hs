module Marvel.Encounter.WinterSoldierNemesisSpec (spec) where

import TestImport
import Marvel.Ability qualified as Ability
import Marvel.FalconWinter.Cards qualified as Cards
import Marvel.Matchers
import Marvel.Modifier
import Marvel.Minion.Cards qualified as Minions
import Marvel.Query

settle :: GameAppT ()
settle = do
  pass
  q <- asked
  case q >>= (headMay . filter forced . snd) of
    Just c -> choose "mandatory" (== c) >> settle
    Nothing -> pure ()
 where
  forced = \case
    UseAbility a -> abilityType a `elem` [Ability.ForcedResponse, Ability.ForcedInterrupt, Ability.WhenRevealed]
    _ -> False

spec :: Spec
spec = describe "Winter Soldier nemesis and Whiteout" do
  it "Hydra Mercenary guards the villain while engaged" do
    tg <- testGame ["winter-soldier"]
    inGame tg do
      let p = player1 tg
      changeForm p
      settle
      _ <- addMinion p Cards.fw54031
      v <- villainIdNow
      selectAny (AttackableBy (IdentityWithId p) <> EnemyWithId (EnemyVillainId v)) `shouldReturn` False

  it "Hydra Hit Squad raises Hydra attack and hit points while in play" do
    tg <- testGame ["winter-soldier"]
    inGame tg do
      let p = player1 tg
      mid <- addMinion p Cards.fw54031
      _ <- revealNow p Cards.fw54029
      settle
      getEnemyRemainingHitPoints (EnemyMinionId mid) `shouldReturn` 5
      getModifiers (MinionRef mid) >>= (`shouldSatisfy` elem (AttackModifier 1))

  it "High-Tech Armament attaches to Crossbones ahead of the villain" do
    tg <- testGame ["winter-soldier"]
    inGame tg do
      let p = player1 tg
      mid <- addMinion p Cards.fw54028
      card <- revealNow p Cards.fw54030
      settle
      selectAny (AttachmentWithId (AttachmentId $ unCardId $ ecCardId card) <> AttachmentOnEnemy (EnemyMinionId mid)) `shouldReturn` True

  it "Slippery Conditions exhausts allies entering play for any player" do
    tg <- testGame ["winter-soldier"]
    inGame tg do
      let p = player1 tg
      _ <- revealNow p Cards.fw54036
      settle
      ally <- putIntoPlay p "54003"
      settle
      allyExhausted <$> allyIn "54003" `shouldReturn` True
      selectAny (AllyWithId $ allyIdOf ally) `shouldReturn` True

  it "Encased in Ice accepts attacks but is not an ordinary minion" do
    tg <- testGame ["winter-soldier"]
    inGame tg do
      let p = player1 tg
      changeForm p
      settle
      card <- revealNow p Cards.fw54035
      settle
      let aid = AttachmentId $ unCardId $ ecCardId card
      selectAny (AttackableBy (IdentityWithId p) <> EnemyWithId (EnemyAttachmentId aid)) `shouldReturn` True
      selectAny (MinionWithId $ MinionId $ unCardId $ ecCardId card) `shouldReturn` False
      v <- villainIdNow
      selectAny (AttackableBy (IdentityWithId p) <> EnemyWithId (EnemyVillainId v)) `shouldReturn` False

  it "Encased in Ice moves only damage beyond three to its host and discards" do
    tg <- testGame ["winter-soldier"]
    inGame tg do
      let p = player1 tg
      changeForm p
      settle
      clearHand p
      card <- revealNow p Cards.fw54035
      settle
      let aid = AttachmentId $ unCardId $ ecCardId card
      punch <- putInHand p "54005"
      resources <- traverse (putInHand p) ["01088", "01086"]
      playWith punch resources
      choose "attack the ice" (isTargetChoice aid)
      settle
      damageOf p `shouldReturn` 4
      selectAny (AttachmentWithId aid) `shouldReturn` False

  it "tough prevents excess damage moved from Encased in Ice" do
    tg <- testGame ["winter-soldier"]
    inGame tg do
      let p = player1 tg
      changeForm p
      settle
      clearHand p
      resolve [IdentityMessage p IdentityBecomeTough]
      card <- revealNow p Cards.fw54035
      settle
      let aid = AttachmentId $ unCardId $ ecCardId card
      punch <- putInHand p "54005"
      resources <- traverse (putInHand p) ["01088", "01086"]
      playWith punch resources
      choose "attack the ice" (isTargetChoice aid)
      settle
      damageOf p `shouldReturn` 0
      selectAny (AttachmentWithId aid) `shouldReturn` False

  it "Red Room Programming can be removed by exhausting Bucky in alter-ego" do
    tg <- testGame ["winter-soldier"]
    inGame tg do
      let p = player1 tg
      card <- revealNow p Cards.fw54027
      choose "exhaust Bucky" (isLabel "$label.exhaustRemoveObligation")
      settle
      isExhaustedNow p `shouldReturn` True
      obligationIsInPlay (ObligationId $ unCardId $ ecCardId card) `shouldReturn` False

  it "Blizzard attaches Encased in Ice to the character he attacked" do
    tg <- testGame ["winter-soldier"]
    inGame tg do
      let p = player1 tg
      changeForm p
      settle
      clearHand p
      mid <- addMinion p Cards.fw54034
      (cards, _) <- setEncounterDeck [Minions.hydraSoldier, Cards.fw54035, Minions.hydraSoldier, Minions.hydraSoldier] []
      runNow [MinionMessage mid $ MinionAttacks p]
      settle
      let aid = AttachmentId $ unCardId $ ecCardId $ testHead $ testTail cards
      actual <- getAttachmentIdentity aid
      actual `shouldBe` Just p

  it "Whiteout activates Blizzard already in play" do
    tg <- testGame ["winter-soldier"]
    inGame tg do
      let p = player1 tg
      _ <- addMinion p Cards.fw54034
      _ <- setEncounterDeck [Minions.hydraSoldier, Minions.hydraSoldier, Minions.hydraSoldier] []
      _ <- revealNow p Cards.fw54037
      settle
      n <- mainSchemeThreatNow
      n `shouldBe` 2

  it "Crossbones places two main-scheme threat after his attack defeats an ally" do
    tg <- testGame ["winter-soldier"]
    inGame tg do
      let p = player1 tg
      mid <- addMinion p Cards.fw54028
      ally <- putIntoPlay p "01083"
      resolve [AllyMessage (allyIdOf ally) $ AllySetDamage 2]
      changeForm p
      settle
      clearHand p
      runNow [MinionMessage mid $ MinionAttacksAlly p $ allyIdOf ally]
      settle
      mainSchemeThreatNow `shouldReturn` 2
