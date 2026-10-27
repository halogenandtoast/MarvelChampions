module Marvel.Rules.UniqueSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Minion.Cards qualified as Minions
import TestImport

inPlay :: EncounterCard -> GameAppT Bool
inPlay card = getsGame (HashMap.member (minionOf card) . gameMinions)

-- RR Unique Icon: "A non-villain card in an out-of-play state that matches a card in play
-- cannot enter play. ... A non-villain encounter card, it is discarded and any effects of
-- it entering play are ignored. If it was being revealed, any effects of it being revealed
-- are ignored and the player revealing it is dealt a facedown encounter card."
spec :: Spec
spec = describe "unique encounter cards" do
  it "a revealed minion matching a unique card in play is discarded, its player dealt a facedown card" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      first <- revealNow p Minions.sandman
      pass
      [next] <- stackEncounterDeck [Minions.hydraMercenary]
      second <- revealNow p Minions.sandman
      pass
      inPlay first `shouldReturn` True
      inPlay second `shouldReturn` False
      map ecCardId <$> encounterDiscard `shouldReturn` [ecCardId second]
      dealtTo p `shouldReturn` [next]

  it "one put into play is discarded, with no card dealt" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      _ <- revealNow p Minions.sandman
      pass
      card <- encounterCard Minions.sandman
      runNow [PutEncounterCardIntoPlay p card]
      pass
      inPlay card `shouldReturn` False
      dealtTo p `shouldReturn` []

  it "a unique minion with no match enters play" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      card <- revealNow p Minions.sandman
      pass
      inPlay card `shouldReturn` True
