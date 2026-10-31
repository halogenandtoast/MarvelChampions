module Marvel.Hero.StormSignatureSpec (spec) where

import Data.List (find)
import Marvel.Minion.Cards qualified as Minions
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Phase
import TestImport

asStorm :: IdentityId -> GameAppT ()
asStorm p = changeForm p >> pass >> clearHand p

weather :: IdentityId -> CardCode -> GameAppT ()
weather p code = do
  cards <- fromMaybe [] . playerIdentityWeatherDeck <$> playerAttrs p
  for_ (find ((== code) . toCardCode) cards) \card ->
    resolve [IdentityMessage p $ ChooseWeatherSupport card False]

payEvent :: IdentityId -> CardCode -> [CardCode] -> GameAppT ()
payEvent p code resources = do
  c <- putInHand p code
  payment <- traverse (putInHand p) resources
  playWith c payment

isCardChoice :: CardCode -> Choice -> Bool
isCardChoice code = \case
  CardLabel c _ -> c == code
  _ -> False

spec :: Spec
spec = describe "Storm signature events" do
  it "Weather Goddess swaps to a different Weather and resolves Clear Skies" do
    tg <- testGame ["storm"]
    inGame tg do
      let p = player1 tg
      asStorm p
      weather p "36004"
      payEvent p "36009" []
      choose "Clear Skies" (isCardChoice "36002")
      (length <$> handOf p) `shouldReturn` 1
      supportsWith "36002" >>= (`shouldSatisfy` (not . null))
      (length <$> supportsWith "36004") `shouldReturn` 0

  it "Weather Goddess can swap even after Weather Control was used" do
    tg <- testGame ["storm"]
    inGame tg do
      let p = player1 tg
      asStorm p
      weather p "36003"
      choose "Weather Control" (isNamedAbility "$label.weatherControl")
      choose "Clear Skies" (isCardChoice "36002")
      clearHand p
      payEvent p "36009" []
      choose "Thunderstorm" (isCardChoice "36004")
      vid <- villainIdNow
      choose "Special damage" (isTargetChoice vid)
      villainDamageNow `shouldReturn` 2

  it "Torrential Rain removes three threat divided among schemes" do
    tg <- testGame ["storm"]
    inGame tg do
      let p = player1 tg
      asStorm p
      weather p "36002"
      placeMainThreat 6
      before <- mainSchemeThreatNow
      payEvent p "36010" ["01089"]
      pass
      mainSchemeThreatNow `shouldReturn` before - 3

  it "Torrential Rain also resolves Hurricane's separate Special" do
    tg <- testGame ["storm"]
    inGame tg do
      let p = player1 tg
      asStorm p
      weather p "36003"
      placeMainThreat 6
      before <- mainSchemeThreatNow
      payEvent p "36010" ["01089"]
      chooseMainScheme
      mainSchemeThreatNow `shouldReturn` before - 5

  it "Lightning Bolt deals eight damage without Thunderstorm" do
    tg <- testGame ["storm"]
    inGame tg do
      let p = player1 tg
      asStorm p
      weather p "36002"
      payEvent p "36011" ["01088", "01089"]
      vid <- villainIdNow
      choose "attack target" (isTargetChoice vid)
      villainDamageNow `shouldReturn` 8

  it "Lightning Bolt resolves Thunderstorm after its eight damage" do
    tg <- testGame ["storm"]
    inGame tg do
      let p = player1 tg
      asStorm p
      weather p "36004"
      payEvent p "36011" ["01088", "01089"]
      vid <- villainIdNow
      choose "attack target" (isTargetChoice vid)
      choose "Special target" (isTargetChoice vid)
      villainDamageNow `shouldReturn` 10

  it "Blast of Wind damages the villain and the selected player's minions, then resolves Weather" do
    tg <- testGame ["storm", "spider-man"]
    inGame tg do
      let p = player1 tg
          other = player2 tg
      asStorm p
      weather p "36002"
      mine <- addMinion p Minions.hydraMercenary
      theirs <- addMinion other Minions.hydraMercenary
      payEvent p "36013" ["01088", "01089"]
      choose "other player" (isTargetChoice other)
      villainDamageNow `shouldReturn` 3
      minionDamageOf mine `shouldReturn` Just 0
      minionDamageOf theirs `shouldReturn` Nothing
      (length <$> handOf p) `shouldReturn` 1

  it "Flash Freeze reduces the villain's ATK for its attack on Storm" do
    tg <- testGame ["storm"]
    inGame tg do
      let p = player1 tg
      asStorm p
      weather p "36002"
      card <- putInHand p "36012"
      payment <- putInHand p "01088"
      _ <- stackEncounterDeck [Treacheries.assault]
      vid <- villainIdNow
      start [VillainMessage vid $ VillainAttacks p]
      playWith card [payment]
      pass
      damageOf p `shouldReturn` 0
      later <- addMinion p Minions.hydraMercenary
      resolve [MinionMessage later $ MinionAttacks p]
      damageOf p `shouldReturn` 0
      resolve [EndPhase VillainPhase]
      resolve [MinionMessage later $ MinionAttacks p]
      damageOf p `shouldReturn` 1

  it "Flash Freeze does not reduce an ally defender's damage in a later minion attack" do
    tg <- testGame ["storm"]
    inGame tg do
      let p = player1 tg
      asStorm p
      weather p "36002"
      ally <- putIntoPlay p "36017"
      card <- putInHand p "36012"
      payment <- putInHand p "01088"
      _ <- stackEncounterDeck [Treacheries.assault]
      vid <- villainIdNow
      start [VillainMessage vid $ VillainAttacks p]
      playWith card [payment]
      pass
      mid <- addMinion p Minions.hydraMercenary
      start [MinionMessage mid $ MinionAttacks p]
      choose "Pixie defends" \case
        AllyDefend aid _ -> aid == AllyId (unCardId $ pcCardId ally)
        _ -> False
      pass
      allies <- alliesWith "36017"
      map allyDamage allies `shouldBe` [1]

  it "Flash Freeze resolves Blizzard's Special when it is in play" do
    tg <- testGame ["storm"]
    inGame tg do
      let p = player1 tg
      asStorm p
      weather p "36005"
      mid <- addMinion p Minions.hydraMercenary
      card <- putInHand p "36012"
      payment <- putInHand p "01088"
      _ <- stackEncounterDeck [Treacheries.assault]
      vid <- villainIdNow
      start [VillainMessage vid $ VillainAttacks p]
      playWith card [payment]
      choose "blank the minion" (isTargetChoice mid)
      isBlanked mid `shouldReturn` True

  it "Torrential Rain divides removal between a side scheme and the main scheme" do
    tg <- testGame ["storm"]
    inGame tg do
      let p = player1 tg
      asStorm p
      weather p "36002"
      side <- encounterCard SideSchemes.breakinAndTakin
      resolve [RevealEncounterCard p side]
      placeMainThreat 5
      mainBefore <- mainSchemeThreatNow
      sideBefore <- sideSchemeThreat (sideSchemeOf side)
      payEvent p "36010" ["01089"]
      choose "one from side scheme" (isTargetChoice $ sideSchemeOf side)
      chooseMainScheme
      chooseMainScheme
      sideSchemeThreat (sideSchemeOf side) `shouldReturn` sideBefore - 1
      mainSchemeThreatNow `shouldReturn` mainBefore - 2

  it "the signature actions cannot be played in alter ego" do
    tg <- testGame ["storm"]
    inGame tg do
      let p = player1 tg
      clearHand p
      cards <- traverse (putInHand p) ["36009", "36010", "36011", "36013"]
      _ <- putInHand p "01088"
      _ <- putInHand p "01089"
      for_ cards \card -> offers (isPlay card) `shouldReturn` False
