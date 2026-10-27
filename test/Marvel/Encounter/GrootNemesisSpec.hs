module Marvel.Encounter.GrootNemesisSpec (spec) where

import Marvel.EncounterSet (EncounterSet (GrootNemesis), getNemesisSet)
import Marvel.Minion.Cards qualified as Minions
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Treachery.Cards qualified as Treacheries
import TestImport

asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass

-- | The minion attacks the player, undefended.
attackedBy :: MinionId -> IdentityId -> GameAppT ()
attackedBy m p = do
  start [MinionMessage m (MinionAttacks p)]
  choose "no defender" isNoDefender
  chooseIf isAnyAbility
  pass

spec :: Spec
spec = describe "Groot nemesis" do
  it "is Groot's nemesis set" do
    getNemesisSet "16001a" `shouldBe` GrootNemesis :: IO ()

  describe "Blazing Inferno (16026)" do
    it "enters with 3 threat a player" do
      tg <- testGame ["spider-man", "spider-man"]
      let p = player1 tg
      inGame tg do
        scheme <- sideSchemeOf <$> revealNow p SideSchemes.blazingInferno
        pass
        sideSchemeThreat scheme `shouldReturn` 6

    it "deals 2 indirect damage to each player as the villain phase begins" do
      tg <- testGame ["spider-man", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        _ <- revealNow p SideSchemes.blazingInferno
        pass
        asHero p
        endTurn
        asHero other
        endTurn
        -- before the villain's attack is answered
        damageOf p `shouldReturn` 2
        damageOf other `shouldReturn` 2

  describe "Furnax (16027)" do
    it "is a Leviathon, as printed" do
      (map show (toList (cdTraits Minions.furnax)) `shouldBe` ["Leviathon"]) :: IO ()

    it "after he attacks, deals 2 indirect damage to each player" do
      tg <- testGame ["spider-man", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        asHero p
        m <- addMinion p Minions.furnax
        m `attackedBy` p
        damageOf p `shouldReturn` 4
        damageOf other `shouldReturn` 2

    it "after he schemes, deals 2 indirect damage to each player" do
      tg <- testGame ["spider-man", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        m <- addMinion p Minions.furnax
        before <- mainSchemeThreatNow
        resolve [MinionMessage m MinionSchemes]
        mainSchemeThreatNow `shouldReturn` (before + 2)
        damageOf p `shouldReturn` 2
        damageOf other `shouldReturn` 2

  describe "Fan the Flames (16028)" do
    it "deals 2 indirect damage to the player revealing it" do
      tg <- testGame ["spider-man", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        _ <- revealNow p Treacheries.fanTheFlames
        pass
        damageOf p `shouldReturn` 2
        damageOf other `shouldReturn` 0

    it "1 more with Blazing Inferno in play" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        _ <- revealNow p SideSchemes.blazingInferno
        pass
        _ <- revealNow p Treacheries.fanTheFlames
        pass
        damageOf p `shouldReturn` 3

    it "and 1 more with Furnax in play" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        _ <- revealNow p SideSchemes.blazingInferno
        pass
        _ <- addMinion p Minions.furnax
        _ <- revealNow p Treacheries.fanTheFlames
        pass
        damageOf p `shouldReturn` 4
