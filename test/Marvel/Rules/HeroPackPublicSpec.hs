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
  it "shows Ironheart's inactive identity cards after setup and each level up" do
    tg <- testGame ["ironheart"]
    inGame tg do
      let p = player1 tg
          check expected = do
            view <- liftIO . publicGame =<< getGame
            let codes = case view of
                  Object obj -> case KeyMap.lookup "setAsideCards" obj of
                    Just (Array cards) -> [code | Object card <- toList cards, Just code <- [KeyMap.lookup "code" card]]
                    _ -> []
                  _ -> []
            filter (`elem` map toJSON (["29001a", "29002a", "29003a"] :: [CardCode])) codes
              `shouldBe` map toJSON (expected :: [CardCode])
      check ["29002a", "29003a"]
      resolve [IdentityMessage p $ SwapIdentityVersion "29002a" "29002b"]
      check ["29001a", "29003a"]
      resolve [IdentityMessage p $ SwapIdentityVersion "29003a" "29003b"]
      check ["29001a", "29002a"]

  it "names the alternative payments for Riri's single Child Prodigy action" do
    tg <- testGame ["ironheart"]
    inGame tg do
      let p = player1 tg
      clearHand p
      resolve [IdentityMessage p $ SwapIdentityVersion "29002a" "29002b"]
      _ <- putInHand p "01091"
      _ <- putInHand p "01090"
      resolve [IdentityMessage p PlayerTurnOption]
      view <- liftIO . publicGame =<< getGame
      let payments = collectPaymentLabels view
      payments `shouldMatchList` [String "$label.childProdigyMentalPayment", String "$label.childProdigyTwoResourcesPayment"]

  it "shows Stroke of Genius's response on the spent resource card" do
    tg <- testGame ["ironheart"]
    inGame tg do
      let p = player1 tg
      clearHand p
      ronnie <- putInHand p "29010"
      genius <- putInHand p "29009"
      choose "play" (isPlay ronnie)
      choose "pay Stroke" (isPayWith genius)
      view <- liftIO . publicGame =<< getGame
      let responses = findResponses view
      responses `shouldSatisfy` notNull
      for_ responses $ \response -> do
        KeyMap.lookup "kind" response `shouldBe` Just (String "ability")
        KeyMap.lookup "target" response `shouldBe` Just (toJSON $ pcCardId genius)
        KeyMap.lookup "abilityType" response `shouldBe` Just (String "Response")

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

findResponses :: Value -> [Object]
findResponses = \case
  Object obj -> [obj | KeyMap.lookup "label" obj == Just (String "$label.strokeOfGenius")] <> concatMap findResponses (KeyMap.elems obj)
  Array items -> concatMap findResponses (toList items)
  _ -> []

collectPaymentLabels :: Value -> [Value]
collectPaymentLabels = \case
  Object obj -> [v | Just v <- [KeyMap.lookup "paymentLabel" obj], v /= Null] <> concatMap collectPaymentLabels (KeyMap.elems obj)
  Array items -> concatMap collectPaymentLabels (toList items)
  _ -> []
