module Marvel.Rules.SupportHeldEventsSpec (spec) where

import Data.Aeson.KeyMap qualified as KeyMap
import Marvel.Cost.Types (Cost (DiscardCost))
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Support.Types qualified as Support
import Marvel.Ability.Types (HasAbilities)
import Marvel.Modifier (HasModifiersFor (..))
import Marvel.Matchers.Types
import Marvel.Modifier (Modifier (Blank))
import TestImport

newtype StoredSupport = StoredSupport (Attrs Support)
  deriving anyclass (Support.IsSupport, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON)
instance RunMessage StoredSupport where
  runMessage msg (StoredSupport attrs) = StoredSupport <$> runMessage msg attrs

attachEvent :: SupportId -> PlayerCard -> [Message]
attachEvent sid card = [RemoveFromGame (toRef card), SupportMessage sid (AttachPlayableEvent card)]

spec :: Spec
spec = describe "events attached facedown to supports" do
  it "can be played as in hand and leave the holder when played" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      changeForm p
      holder <- putIntoPlay p "01015"
      clearHand p
      [card] <- stackDiscard p ["01005"]
      resolve $ attachEvent (supportIdOf holder) card
      discardOf p >>= (`shouldSatisfy` notElem card)
      getSupportHeldCards p `shouldReturn` [card]
      a <- putInHand p "01088"
      b <- putInHand p "01089"
      playWith card [a, b]
      chooseVillain
      pass
      getSupportHeldCards p `shouldReturn` []
      villainDamageNow `shouldReturn` 8
      discardOf p >>= (`shouldSatisfy` elem card)

  it "blanking suppresses permission without discarding the attached event" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      changeForm p
      holder <- putIntoPlay p "01015"
      clearHand p
      [card] <- stackDiscard p ["01005"]
      resolve $ attachEvent (supportIdOf holder) card
      _ <- putInHand p "01088"
      _ <- putInHand p "01089"
      _ <- revealNow p SideSchemes.smPanicInTheStreets
      pass
      refreshTurn
      offers (isPlay card) `shouldReturn` False
      getSupportHeldCards p `shouldReturn` []
      attrs <- getsGame (toAttrs . fromMaybe (error "missing support") . lookup (supportIdOf holder) . gameSupports)
      Support.supportHeldCards attrs `shouldBe` [card]


  it "discards held events when the holder leaves play" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      holder <- putIntoPlay p "01015"
      [card] <- stackDiscard p ["01005"]
      resolve $ attachEvent (supportIdOf holder) card
      resolve [RemoveFromPlay $ toRef $ supportIdOf holder]
      discardOf p >>= (`shouldSatisfy` (\cs -> card `elem` cs && holder `elem` cs))
      getSupportHeldCards p `shouldReturn` []

  it "discards held events when the holder returns to hand" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      holder <- putIntoPlay p "01015"
      [card] <- stackDiscard p ["01005"]
      resolve $ attachEvent (supportIdOf holder) card
      resolve [ReturnToHand $ toRef $ supportIdOf holder]
      discardOf p >>= (`shouldSatisfy` elem card)
      handOf p >>= (`shouldSatisfy` elem holder)

  it "reserves an event before its holder is discarded to pay for it" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      changeForm p
      holder <- putIntoPlay p "01015"
      clearHand p
      [original] <- stackDiscard p ["01005"]
      let card = original {pcCardDef = (getCardDef original)
            { cdCost = Just 0, cdAbilityCost = DiscardCost (SupportRef $ supportIdOf holder) }}
      resolve $ attachEvent (supportIdOf holder) card
      playWith card []
      chooseVillain
      pass
      cards <- discardOf p
      length (filter ((== pcCardId card) . pcCardId) cards) `shouldBe` 1
      getSupportHeldCards p `shouldReturn` []
      villainDamageNow `shouldReturn` 8

  it "returns a reserved event to its holder when payment is aborted" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      changeForm p
      holder <- putIntoPlay p "01015"
      clearHand p
      [card] <- stackDiscard p ["01005"]
      resolve $ attachEvent (supportIdOf holder) card
      _ <- putInHand p "01088"
      _ <- putInHand p "01089"
      choose "begin paying for held event" (isPlay card)
      getSupportHeldCards p `shouldReturn` []
      Just acid <- getActiveCost
      runNow [AbortPayment acid]
      getSupportHeldCards p `shouldReturn` [card]
      handOf p >>= (`shouldSatisfy` notElem card)
      discardOf p >>= (`shouldSatisfy` notElem card)

  it "discards an aborted reserved event once if its holder has left play" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      changeForm p
      holder <- putIntoPlay p "01015"
      clearHand p
      [card] <- stackDiscard p ["01005"]
      resolve $ attachEvent (supportIdOf holder) card
      _ <- putInHand p "01088"
      _ <- putInHand p "01089"
      choose "begin paying" (isPlay card)
      Just acid <- getActiveCost
      runNow [RemoveFromPlay $ toRef $ supportIdOf holder, AbortPayment acid]
      cards <- discardOf p
      length (filter ((== pcCardId card) . pcCardId) cards) `shouldBe` 1
      handOf p >>= (`shouldSatisfy` notElem card)

  it "does not grant permission to play cards merely stored on a support" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      changeForm p
      holder <- putIntoPlay p "01015"
      clearHand p
      [card] <- stackDiscard p ["01005"]
      resolve [RemoveFromGame $ toRef card]
      attrs <- getsGame (toAttrs . fromMaybe (error "missing support") . lookup (supportIdOf holder) . gameSupports)
      withGame_ $ entitiesL . supportsL . at (supportIdOf holder) ?~ Support.Support (StoredSupport (attrs {supportHeldCards = [card]}))
      getSupportHeldCards p `shouldReturn` []

  it "loads legacy supports without granting permission to play held cards" do
    tg <- testGame ["spider-man"]
    inGame tg do
      holder <- putIntoPlay (player1 tg) "01015"
      attrs <- getsGame (toAttrs . fromMaybe (error "missing support") . lookup (supportIdOf holder) . gameSupports)
      let old = case toJSON attrs of
            Object o -> Object (KeyMap.delete "supportCanPlayHeldEvents" o)
            v -> v
      case fromJSON old of
        Success (a :: Attrs Support) -> Support.supportCanPlayHeldEvents a `shouldBe` False
        Error e -> liftIO $ expectationFailure e
