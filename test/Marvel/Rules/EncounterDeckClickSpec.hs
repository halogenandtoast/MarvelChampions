module Marvel.Rules.EncounterDeckClickSpec (spec) where

import TestImport
import Marvel.Minion.Cards qualified as Minions

spec :: Spec
spec = describe "Resolving dealt encounter cards" do
  it "deals automatically and waits for a click before revealing each card" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
          dealt = length . playerIdentityEncounterCards . toAttrs <$> getPlayer p
      before <- dealt
      _ <- stackEncounterDeck [Minions.hydraMercenary, Minions.hydraMercenary]
      runNow [DealEncounterCard p, DealEncounterCard p]
      dealt `shouldReturn` (before + 2)
      runNow [IdentityMessage p RevealEncounterCards]
      dealt `shouldReturn` (before + 2)
      offers (isLabel "$label.revealDealtEncounterCard") `shouldReturn` True
      choose "reveal the encounter card" (isLabel "$label.revealDealtEncounterCard")
      dealt `shouldReturn` (before + 1)
      offers (isLabel "$label.revealDealtEncounterCard") `shouldReturn` True
      choose "reveal the next encounter card" (isLabel "$label.revealDealtEncounterCard")
      dealt `shouldReturn` before

  it "deals to every player before revealing in player order" do
    tg <- testGame ["spider-man", "captain-marvel"]
    inGame tg do
      let first = player1 tg
          second = player2 tg
          dealt p = length . playerIdentityEncounterCards . toAttrs <$> getPlayer p
      _ <- stackEncounterDeck [Minions.hydraMercenary, Minions.hydraMercenary]
      runNow [DealEncounterCard first, DealEncounterCard second, RevealDealtEncounterCards]
      dealt first `shouldReturn` 1
      dealt second `shouldReturn` 1
      (fmap fst <$> openQuestion) `shouldReturn` Just first
      choose "first player's encounter" (isLabel "$label.revealDealtEncounterCard")
      dealt first `shouldReturn` 0
      dealt second `shouldReturn` 1
      (fmap fst <$> openQuestion) `shouldReturn` Just second
      choose "second player's encounter" (isLabel "$label.revealDealtEncounterCard")
      dealt second `shouldReturn` 0
