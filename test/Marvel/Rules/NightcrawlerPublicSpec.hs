module Marvel.Rules.NightcrawlerPublicSpec (spec) where

import Data.Aeson.KeyMap qualified as KeyMap
import Data.List (isInfixOf)
import Marvel.Game.Public (publicGameFor)
import Marvel.Treachery.Cards qualified as Treacheries
import TestImport

spec :: Spec
spec = describe "Nightcrawler public Gambit boost field" do
  it "exposes the numeric icons and star while hiding the tucked card's identity" do
    tg <- testGame ["nightcrawler"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      clearHand p
      cards <- stackEncounterDeck [Treacheries.repairSequence]
      gambit <- putInHand p "48021"
      energy <- putInHand p "01088"
      resource <- putInHand p "01086"
      playWith gambit [energy,resource]
      choose "Gambit's entry response" (isAbilityOfCard gambit)
      choose "tuck the card with one icon and a star" (isTargetChoice $ testHead cards)
      pass
      view <- liftIO . publicGameFor (const False) =<< getGame
      let allies = [ally | Object game <- [view], Just (Array players) <- [KeyMap.lookup "players" game],
            Object player <- toList players, Just (Array allAllies) <- [KeyMap.lookup "allies" player],
            Object ally <- toList allAllies, KeyMap.lookup "code" ally == Just (String "48021")]
      length allies `shouldBe` 1
      let ally = testHead allies
      KeyMap.lookup "tuckedBoostIcons" ally `shouldBe` Just (toJSON (1 :: Int))
      KeyMap.lookup "tuckedBoostStar" ally `shouldBe` Just (Bool True)
      show (Object ally) `shouldSatisfy` (not . isInfixOf "01146")
