module Marvel.Hero.MagnetoSignatureSpec (spec) where

import Marvel.Minion.Cards qualified as Minions
import Marvel.Matchers
import Marvel.Query
import Marvel.Villain.Types (villainStunned)
import TestImport

asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass >> clearHand p

playPaid :: IdentityId -> CardCode -> [CardCode] -> GameAppT ()
playPaid p code payments = do
  card <- putInHand p code
  resources <- traverse (putInHand p) payments
  playWith card resources

playWrapped :: IdentityId -> MinionId -> GameAppT ()
playWrapped p enemy = do
  card <- putInHand p "49007"
  resource <- putInHand p "01088"
  playOn card enemy [resource]

spec :: Spec
spec = describe "Magneto signature" do
  it "Wrapped in Metal blanks a non-Elite minion and prevents its activation" do
    tg <- testGame ["magneto"]
    inGame tg do
      let p = player1 tg
      asHero p
      enemy <- addMinion p Minions.hydraRegular
      playWrapped p enemy
      pass
      runNow [MinionMessage enemy $ MinionAttacks p]
      pass
      damageOf p `shouldReturn` 0

  it "Wrapped in Metal cannot target an Elite minion" do
    tg <- testGame ["magneto"]
    inGame tg do
      let p = player1 tg
      asHero p
      _ <- addMinion p Minions.laufey
      card <- putInHand p "49007"
      _ <- putInHand p "01088"
      offers (isPlay card) `shouldReturn` False

  it "Electromagnetic Blast removes three threat" do
    tg <- testGame ["magneto"]
    inGame tg do
      let p = player1 tg
      asHero p
      placeMainThreat 5
      before <- mainSchemeThreatNow
      playPaid p "49008" ["01088"]
      chooseMainScheme
      pass
      mainSchemeThreatNow `shouldReturn` before - 3

  it "Metal Shards deals seven damage and does not grant tough without defeating the enemy" do
    tg <- testGame ["magneto"]
    inGame tg do
      let p = player1 tg
      asHero p
      playPaid p "49009" ["01088", "01086"]
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 7
      playerIdentityTough <$> playerAttrs p `shouldReturn` False

  it "Metal Shards grants tough after defeating a minion" do
    tg <- testGame ["magneto"]
    inGame tg do
      let p = player1 tg
      asHero p
      enemy <- addMinion p Minions.hydraRegular
      playPaid p "49009" ["01088", "01086"]
      choose "defeat minion" (isTargetChoice enemy)
      pass
      playerIdentityTough <$> playerAttrs p `shouldReturn` True

  it "Magnetic Missile discards a wrapped minion then deals five damage and stuns" do
    tg <- testGame ["magneto"]
    inGame tg do
      let p = player1 tg
      asHero p
      enemy <- addMinion p Minions.hydraRegular
      playWrapped p enemy
      pass
      playPaid p "49010" ["01086"]
      choose "discard wrapped minion" (isTargetChoice enemy)
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 5
      villainStunned . toAttrs <$> getVillain `shouldReturn` True
      selectAny (MinionWithId enemy) `shouldReturn` False

  it "Magnetic Missile is unavailable without a wrapped minion" do
    tg <- testGame ["magneto"]
    inGame tg do
      let p = player1 tg
      asHero p
      card <- putInHand p "49010"
      _ <- putInHand p "01086"
      offers (isPlay card) `shouldReturn` False

  it "Master of Magnetism pays two resources for a Magnetic event" do
    tg <- testGame ["magneto"]
    inGame tg do
      let p = player1 tg
      asHero p
      placeMainThreat 5
      before <- mainSchemeThreatNow
      playPaid p "49008" ["49011"]
      chooseMainScheme
      pass
      mainSchemeThreatNow `shouldReturn` before - 3

  it "Wrapped in Metal removes the attached minion's printed Guard keyword" do
    tg <- testGame ["magneto"]
    inGame tg do
      let p = player1 tg
      asHero p
      enemy <- addMinion p Minions.hydraMercenary
      playWrapped p enemy
      pass
      choose "attack" (isHeroAttack p)
      villain <- villainIdNow
      offers (isTargetChoice villain) `shouldReturn` True
