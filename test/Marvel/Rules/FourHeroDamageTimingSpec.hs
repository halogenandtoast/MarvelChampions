module Marvel.Rules.FourHeroDamageTimingSpec (spec) where

import Marvel.Ability qualified as Ability
import Marvel.Keyword (Keyword(Piercing))
import Marvel.Message.Lifted qualified as L
import Marvel.Minion.Cards qualified as Minions
import Marvel.Modifier
import Marvel.Matchers
import TestImport

settle :: GameAppT ()
settle = do
  pass
  question <- asked
  case question >>= (headMay . filter forced . snd) of
    Just choice -> choose "mandatory ability" (== choice) >> settle
    Nothing -> pure ()
 where
  forced = \case
    UseAbility ab -> abilityType ab `elem` [Ability.ForcedInterrupt, Ability.ForcedResponse, Ability.WhenRevealed]
    _ -> False

asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> settle >> clearHand p

wallDamage :: GameAppT [Natural]
wallDamage = map supportUses <$> supportsWith "46008"

spec :: Spec
spec = describe "Four hero damage timing" do
  it "Iceman's Ice Wall receives no damage when tough prevents the enemy attack first" do
    tg <- testGame ["iceman"]
    inGame tg do
      let p = player1 tg
      asHero p
      _ <- putIntoPlay p "46008"
      resolve [IdentityMessage p IdentityBecomeTough]
      mid <- addMinion p Minions.laufey
      runNow [MinionMessage mid $ MinionAttacks p]
      settle
      damageOf p `shouldReturn` 0
      wallDamage `shouldReturn` [0]
      playerIdentityTough <$> playerAttrs p `shouldReturn` False

  it "Iceman's Ice Wall still receives piercing damage after tough is discarded" do
    tg <- testGame ["iceman"]
    inGame tg do
      let p = player1 tg
      asHero p
      _ <- putIntoPlay p "46008"
      resolve [IdentityMessage p IdentityBecomeTough]
      mid <- addMinion p Minions.laufey
      runNow [MinionMessage mid $ MinionAttacksWithChange p 0 [Piercing]]
      settle
      damageOf p `shouldReturn` 0
      wallDamage `shouldReturn` [4]
      playerIdentityTough <$> playerAttrs p `shouldReturn` False

  it "Iceman's Ice Wall receives no damage when its recipient cannot take damage" do
    tg <- testGame ["iceman"]
    inGame tg do
      let p = player1 tg
      asHero p
      _ <- putIntoPlay p "46008"
      (_, messages) <- L.captureQueueT $ L.lastingModifiers p (IdentityEntity $ IdentityWithId p) [CannotTakeDamage] True
      runNow messages
      mid <- addMinion p Minions.laufey
      runNow [MinionMessage mid $ MinionAttacks p]
      settle
      damageOf p `shouldReturn` 0
      wallDamage `shouldReturn` [0]

  it "Nightcrawler's Riposte remains eligible after unrelated damage between post-attack choices" do
    tg <- testGame ["nightcrawler"]
    inGame tg do
      let p = player1 tg
      asHero p
      _ <- putIntoPlay p "48004"
      mid <- addMinion p Minions.tigerShark
      riposte <- putInHand p "48018"
      resource <- putInHand p "01086"
      runNow [MinionMessage mid $ MinionAttacks p]
      choose "defend the attack" isDefend
      playWith riposte [resource]
      damageOf p `shouldReturn` 0
      -- Retaliate and Riposte are already eligible. A nested unrelated damage
      -- effect cannot retroactively make the completed attack deal damage.
      runNow $ damageIdentity p GameRef $ toDamage 2 FromAbility
      pass
      choose "retaliate first" (isLabel "$label.resolveRetaliate")
      pass
      damageOf p `shouldReturn` 2
      minionDamageOf mid `shouldReturn` Just 4

  it "Magneto's Magnetic Bubble receives no damage when tough prevents damage first" do
    tg <- testGame ["magneto"]
    inGame tg do
      let p = player1 tg
      asHero p
      _ <- putIntoPlay p "49006"
      resolve [IdentityMessage p IdentityBecomeTough]
      mid <- addMinion p Minions.laufey
      runNow [MinionMessage mid $ MinionAttacks p]
      settle
      damageOf p `shouldReturn` 0
      map upgradeUses <$> upgradesWith "49006" `shouldReturn` [0]
      playerIdentityTough <$> playerAttrs p `shouldReturn` False

  it "Magneto's Magnetic Bubble receives no damage when its recipient cannot take damage" do
    tg <- testGame ["magneto"]
    inGame tg do
      let p = player1 tg
      asHero p
      _ <- putIntoPlay p "49006"
      (_, messages) <- L.captureQueueT $ L.lastingModifiers p (IdentityEntity $ IdentityWithId p) [CannotTakeDamage] True
      runNow messages
      mid <- addMinion p Minions.laufey
      runNow [MinionMessage mid $ MinionAttacks p]
      settle
      damageOf p `shouldReturn` 0
      map upgradeUses <$> upgradesWith "49006" `shouldReturn` [0]

  it "Iceman's Ice Wall absorbs pending damage inside a simultaneous-attack context wrapper" do
    tg <- testGame ["iceman"]
    inGame tg do
      let p = player1 tg
      asHero p
      _ <- putIntoPlay p "46008"
      mid <- addMinion p Minions.laufey
      let damage = damageIdentity p (MinionRef mid) (toDamage 4 FromAttack)
          wrapped = map (\case
            message@(IdentityMessage _ (IdentityDamaged _ _)) -> WithSimultaneousAttackContext 0 message
            message -> message) damage
      runNow wrapped
      settle
      damageOf p `shouldReturn` 0
      wallDamage `shouldReturn` [4]
