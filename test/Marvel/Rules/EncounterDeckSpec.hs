module Marvel.Rules.EncounterDeckSpec (spec) where

import Marvel.Minion.Cards qualified as Minions
import Marvel.Treachery.Cards qualified as Treacheries
import TestImport

codesOf :: [EncounterCard] -> [CardCode]
codesOf = map toCardCode

finished :: GameAppT Bool
finished = getsGame \g -> case gameState g of
  Finished _ -> True
  _ -> False

spec :: Spec
spec = describe "an encounter deck that empties" do
  it "is made again from the discard pile at once, with an acceleration token, however it empties" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      _ <- setEncounterDeck [Minions.hydraMercenary] [Treacheries.advance]
      before <- accelerationTokensNow
      resolve [SearchForAndPutIntoPlayEncounterCard p (CardWithType MinionType)]
      codesOf <$> scenarioEncounterDeckNow `shouldReturn` [toCardCode Treacheries.advance]
      encounterDiscardNow `shouldReturn` []
      accelerationTokensNow `shouldReturn` before + 1

  it "a search that finds nothing asks nothing" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      _ <- setEncounterDeck [Treacheries.advance] [Treacheries.advance]
      resolve [SearchForAndPutIntoPlayEncounterCard p (CardWithType MinionType)]
      length <$> scenarioEncounterDeckNow `shouldReturn` 1
      finished `shouldReturn` False

  it "goes on dealing from the new deck" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      _ <- setEncounterDeck [Treacheries.advance] [Minions.hydraMercenary, Minions.hydraMercenary]
      dealt <- length . (.playerIdentityEncounterCards) <$> playerAttrs p
      runNow [DealEncounterCard p, DealEncounterCard p]
      length . (.playerIdentityEncounterCards) <$> playerAttrs p `shouldReturn` dealt + 2
      codesOf <$> scenarioEncounterDeckNow `shouldReturn` [toCardCode Minions.hydraMercenary]

  it "a discard effect it empties partway is fulfilled, and does not go on into the new deck" do
    tg <- testGame ["thor"]
    let p = player1 tg
    inGame tg do
      changeForm p
      clearHand p
      _ <- setEncounterDeck [Treacheries.advance, Treacheries.advance] [Minions.hydraMercenary]
      defender <- putInHand p "06003"
      choose "play Defender of the Nine Realms" (isPlay defender)
      pass
      (.playerIdentityMinions) <$> playerAttrs p `shouldReturn` mempty
      length <$> scenarioEncounterDeckNow `shouldReturn` 3

  it "Loki's discard turns up a treachery from a deck it empties" do
    tg <- testGame ["thor"]
    let p = player1 tg
    inGame tg do
      loki <- addMinion p Minions.loki
      pass
      _ <- setEncounterDeck [Treacheries.advance] [Minions.hydraMercenary]
      before <- accelerationTokensNow
      resolve [MinionMessage loki (MinionDamaged GameRef (toDamage 5 FromAbility))]
      minionDamageOf loki `shouldReturn` Just 0
      length <$> scenarioEncounterDeckNow `shouldReturn` 2
      accelerationTokensNow `shouldReturn` before + 1

  it "with no discard pile to make it from, the players lose" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      _ <- setEncounterDeck [Minions.hydraMercenary] []
      resolve [SearchForAndPutIntoPlayEncounterCard p (CardWithType MinionType)]
      finished `shouldReturn` True
