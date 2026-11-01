module Marvel.Rules.HeroPackPublicSpec (spec) where

import Data.Aeson.KeyMap qualified as KeyMap
import Marvel.Game.Public (publicGame)
import TestImport

cardObjects :: CardCode -> Value -> [Object]
cardObjects code = \case
  Object obj -> [obj | KeyMap.lookup "code" obj == Just (toJSON code)] <> concatMap (cardObjects code) (KeyMap.elems obj)
  Array items -> concatMap (cardObjects code) (toList items)
  _ -> []

spec :: Spec
spec = describe "Hero pack public counters" do
  it "shows Magnetic Bubble's absorbed damage as damage" do
    tg <- testGame ["magneto"]
    inGame tg do
      let p = player1 tg
      clearHand p
      bubble <- putIntoPlay p "49006"
      resolve [UpgradeMessage (upgradeIdOf bubble) $ AddUpgradeUses 4]
      view <- liftIO . publicGame =<< getGame
      let cards = filter (KeyMap.member "damage") $ cardObjects "49006" view
      cards `shouldSatisfy` notNull
      for_ cards $ \card -> do
        KeyMap.lookup "damage" card `shouldBe` Just (toJSON (4 :: Natural))
        KeyMap.lookup "uses" card `shouldBe` Just (toJSON (0 :: Natural))

  it "shows Ice Wall's absorbed damage as damage" do
    tg <- testGame ["iceman"]
    inGame tg do
      let p = player1 tg
      clearHand p
      wall <- putIntoPlay p "46008"
      resolve [SupportMessage (supportIdOf wall) $ AddSupportUses 3]
      view <- liftIO . publicGame =<< getGame
      let cards = filter (KeyMap.member "damage") $ cardObjects "46008" view
      cards `shouldSatisfy` notNull
      for_ cards $ \card -> do
        KeyMap.lookup "damage" card `shouldBe` Just (toJSON (3 :: Natural))
        KeyMap.lookup "uses" card `shouldBe` Just (toJSON (0 :: Natural))
