module Marvel.Rules.AllyHeldCardsSpec (spec) where

import Data.Aeson.KeyMap qualified as KeyMap
import Marvel.Cost.Types (Cost (DiscardAllyCost))
import Marvel.Ally.Types qualified as Ally
import Marvel.Effect.Types (Attrs (..), Effect (..), IsEffect)
import Marvel.Matchers.Types
import Marvel.Modifier (Modifier (Blank))
import TestImport

newtype BlankAlly = BlankAlly (Attrs Effect)
  deriving anyclass (IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance RunMessage BlankAlly where
  runMessage msg (BlankAlly a) = BlankAlly <$> runMessage msg a

blankAlly :: AllyId -> GameAppT EffectId
blankAlly aid = do
  eid <- getRandom
  withGame_ $ entitiesL . effectsL . at eid ?~ Effect (BlankAlly EffectAttrs
    { effectId = eid
    , effectCardCode = "01039"
    , effectSource = GameRef
    , effectMatcher = AllyEntity (AllyWithId aid)
    , effectModifiers = [Blank]
    , effectEnds = Nothing
    })
  pure eid

spec :: Spec
spec = describe "events attached facedown to allies" do
  it "can be played as in hand and leave the ally when played" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      changeForm p
      ally <- putIntoPlay p "01068"
      clearHand p
      [card] <- stackDiscard p ["01005"]
      resolve [RemoveFromGame $ toRef card, AttachPlayableEventToAlly (allyIdOf ally) card]
      discardOf p >>= (`shouldSatisfy` notElem card)
      getAllyHeldCards p `shouldReturn` [card]
      a <- putInHand p "01088"
      b <- putInHand p "01089"
      playWith card [a, b]
      chooseVillain
      pass
      getAllyHeldCards p `shouldReturn` []
      villainDamageNow `shouldReturn` 8
      discardOf p >>= (`shouldSatisfy` elem card)

  it "blanking suppresses permission without discarding the attached event" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      changeForm p
      ally <- putIntoPlay p "01068"
      clearHand p
      [card] <- stackDiscard p ["01005"]
      resolve [RemoveFromGame $ toRef card, AttachPlayableEventToAlly (allyIdOf ally) card]
      _ <- putInHand p "01088"
      _ <- putInHand p "01089"
      eid <- blankAlly (allyIdOf ally)
      refreshTurn
      offers (isPlay card) `shouldReturn` False
      getAllyHeldCards p `shouldReturn` []
      attrs <- allyIn "01068"
      Ally.allyHeldCards attrs `shouldBe` [card]
      withGame_ $ entitiesL . effectsL . at eid .~ Nothing
      refreshTurn
      offers (isPlay card) `shouldReturn` True

  it "discards held events when the ally leaves play" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      ally <- putIntoPlay p "01068"
      [card] <- stackDiscard p ["01005"]
      resolve [RemoveFromGame $ toRef card, AttachPlayableEventToAlly (allyIdOf ally) card]
      resolve [RemoveFromPlay $ toRef $ allyIdOf ally]
      discardOf p >>= (`shouldSatisfy` (\cs -> card `elem` cs && ally `elem` cs))
      getAllyHeldCards p `shouldReturn` []

  it "discards held events when the ally returns to hand" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      ally <- putIntoPlay p "01068"
      [card] <- stackDiscard p ["01005"]
      resolve [RemoveFromGame $ toRef card, AttachPlayableEventToAlly (allyIdOf ally) card]
      resolve [ReturnToHand $ toRef $ allyIdOf ally]
      discardOf p >>= (`shouldSatisfy` elem card)
      handOf p >>= (`shouldSatisfy` elem ally)

  it "reserves an event before its holder is discarded to pay for it" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      changeForm p
      ally <- putIntoPlay p "01068"
      clearHand p
      [original] <- stackDiscard p ["01005"]
      let card = original {pcCardDef = (getCardDef original)
            { cdCost = Just 0, cdAbilityCost = DiscardAllyCost AnyAlly }}
      resolve [RemoveFromGame $ toRef original, AttachPlayableEventToAlly (allyIdOf ally) card]
      playWith card []
      chooseVillain
      pass
      cards <- discardOf p
      length (filter ((== pcCardId card) . pcCardId) cards) `shouldBe` 1
      getAllyHeldCards p `shouldReturn` []
      villainDamageNow `shouldReturn` 8

  it "returns a reserved event to its holder when payment is aborted" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      changeForm p
      ally <- putIntoPlay p "01068"
      clearHand p
      [card] <- stackDiscard p ["01005"]
      resolve [RemoveFromGame $ toRef card, AttachPlayableEventToAlly (allyIdOf ally) card]
      _ <- putInHand p "01088"
      _ <- putInHand p "01089"
      choose "begin paying for held event" (isPlay card)
      getAllyHeldCards p `shouldReturn` []
      Just acid <- getActiveCost
      runNow [AbortPayment acid]
      getAllyHeldCards p `shouldReturn` [card]
      handOf p >>= (`shouldSatisfy` notElem card)
      discardOf p >>= (`shouldSatisfy` notElem card)

  it "discards an aborted reserved event once if its holder has left play" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      changeForm p
      ally <- putIntoPlay p "01068"
      clearHand p
      [card] <- stackDiscard p ["01005"]
      resolve [RemoveFromGame $ toRef card, AttachPlayableEventToAlly (allyIdOf ally) card]
      _ <- putInHand p "01088"
      _ <- putInHand p "01089"
      choose "begin paying" (isPlay card)
      Just acid <- getActiveCost
      runNow [RemoveFromPlay $ toRef $ allyIdOf ally, AbortPayment acid]
      cards <- discardOf p
      length (filter ((== pcCardId card) . pcCardId) cards) `shouldBe` 1
      handOf p >>= (`shouldSatisfy` notElem card)

  it "loads legacy allies with no held events" do
    tg <- testGame ["spider-man"]
    inGame tg do
      _ <- putIntoPlay (player1 tg) "01068"
      attrs <- allyIn "01068"
      let old = case toJSON attrs of
            Object o -> Object (KeyMap.delete "allyHeldCards" o)
            v -> v
      case fromJSON old of
        Success (a :: Attrs Ally) -> Ally.allyHeldCards a `shouldBe` []
        Error e -> liftIO $ expectationFailure e
