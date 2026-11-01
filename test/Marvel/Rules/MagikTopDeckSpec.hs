module Marvel.Rules.MagikTopDeckSpec (spec) where

import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Vector qualified as Vector
import Marvel.ActiveCost.Types
import Marvel.Effect.Types (Attrs (..), Effect (..), IsEffect)
import Marvel.Modifier
import Marvel.Cost.Types (costResources)
import Marvel.Game.Public (publicGameFor)
import Marvel.Identity (getModifiedCost, isPlayable)
import Marvel.Matchers
import Marvel.Phase
import Marvel.Query
import Marvel.Window ()
import TestImport

playerField :: Key -> Value -> Maybe Value
playerField key = \case
  Object game -> case KeyMap.lookup "players" game of
    Just (Array players) -> case headMay (Vector.toList players) of
      Just (Object player) -> KeyMap.lookup key player
      _ -> Nothing
    _ -> Nothing
  _ -> Nothing

pendingCost :: GameAppT [Maybe Resource]
pendingCost = do
  costs <- getsGame gameActiveCost
  pure $ maybe [] (costResources . activeCostCost) (headMay costs)

newtype BlankTopDeck = BlankTopDeck (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance RunMessage BlankTopDeck where
  runMessage msg (BlankTopDeck attrs) = BlankTopDeck <$> runMessage msg attrs

spec :: Spec
spec = describe "Magik top-deck play" do
  it "plays the faceup top card for one less while revealing the next card before payment" do
    tg <- testGame ["magik"]
    let p = player1 tg
    inGame tg do
      changeForm p
      clearHand p
      [top, next] <- stackDeck p ["01093", "01087"]
      payment <- putInHand p "01086"
      offers (isPlay top) `shouldReturn` True
      choose "play the faceup card" (isPlay top)
      pendingCost `shouldReturn` [Nothing]
      attrs <- toAttrs <$> getPlayer p
      headMay (unDeck $ playerIdentityDeck attrs) `shouldBe` Just next
      playerIdentityPendingTopDeckPlay attrs `shouldBe` Just top
      handOf p >>= (`shouldSatisfy` notElem top)
      payWith [payment]
      upgradesOf p >>= (`shouldSatisfy` elem (upgradeIdOf top))
      attrs' <- toAttrs <$> getPlayer p
      playerIdentityTopDeckPlayUsed attrs' `shouldBe` True
      playerIdentityPendingTopDeckPlay attrs' `shouldBe` Nothing

  it "allows only one top play per phase and renews the permission in the next phase" do
    tg <- testGame ["magik"]
    let p = player1 tg
    inGame tg do
      changeForm p
      clearHand p
      [top, next] <- stackDeck p ["12033", "01093"]
      playWith top []
      getPlayableHandCards p >>= (`shouldSatisfy` notElem next)
      runNow [BeginPhase VillainPhase]
      getPlayableHandCards p >>= (`shouldSatisfy` elem next)

  it "does not expose or play the deck top while in alter-ego form" do
    tg <- testGame ["magik"]
    let p = player1 tg
    inGame tg do
      clearHand p
      [top] <- stackDeck p ["12033"]
      offers (isPlay top) `shouldReturn` False
      view <- liftIO . publicGameFor (const True) =<< getGame
      playerField "deckTop" view `shouldBe` Just Null

  it "keeps the current top publicly visible even after its phase play has been used" do
    tg <- testGame ["magik"]
    let p = player1 tg
    inGame tg do
      changeForm p
      clearHand p
      [top, next] <- stackDeck p ["12033", "01093"]
      playWith top []
      view <- liftIO . publicGameFor (const False) =<< getGame
      case playerField "deckTop" view of
        Just (Object card) -> KeyMap.lookup "code" card `shouldBe` Just (toJSON $ toCardCode next)
        other -> liftIO $ expectationFailure $ "missing public deck top: " <> show other
      playerField "hand" view `shouldBe` Just Null

  it "does not turn the top card into a resource source or an actual hand card" do
    tg <- testGame ["magik"]
    let p = player1 tg
    inGame tg do
      changeForm p
      clearHand p
      [top] <- stackDeck p ["01088"]
      paymentSources <- getAvailablePaymentSources p
      paymentSources `shouldSatisfy` notElem top
      actualHand <- selectList (InHandOf (IdentityWithId p) $ BasicCardMatches AnyCard)
      actualHand `shouldSatisfy` notElem top
      offers (isPlay top) `shouldReturn` False

  it "returns an aborted top play to the deck instead of adding it to hand" do
    tg <- testGame ["magik"]
    let p = player1 tg
    inGame tg do
      changeForm p
      clearHand p
      [top, next] <- stackDeck p ["01093", "01087"]
      _ <- putInHand p "01086"
      choose "begin paying for top card" (isPlay top)
      Just acid <- getActiveCost
      runNow [AbortPayment acid, IdentityMessage p PlayerTurnOption]
      attrs <- toAttrs <$> getPlayer p
      take 2 (unDeck $ playerIdentityDeck attrs) `shouldBe` [top, next]
      playerIdentityTopDeckPlayUsed attrs `shouldBe` False
      playerIdentityPendingTopDeckPlay attrs `shouldBe` Nothing
      handOf p >>= (`shouldSatisfy` notElem top)
      offers (isPlay top) `shouldReturn` True

  it "does not reduce ordinary hand plays or consume the top-card permission for them" do
    tg <- testGame ["magik"]
    let p = player1 tg
    inGame tg do
      changeForm p
      clearHand p
      [top] <- stackDeck p ["01093"]
      handCard <- putInHand p "01093"
      player <- getPlayer p
      getModifiedCost player handCard Nothing `shouldReturn` 2
      payment <- putInHand p "01088"
      playWith handCard [payment]
      getPlayableHandCards p >>= (`shouldSatisfy` elem top)
      playerIdentityTopDeckPlayUsed . toAttrs <$> getPlayer p `shouldReturn` False

  it "resets a deck emptied by initiating its final card before payment" do
    tg <- testGame ["magik"]
    let p = player1 tg
    inGame tg do
      changeForm p
      clearHand p
      [top] <- stackDeck p ["01093"]
      [discarded] <- stackDiscard p ["01087"]
      withGame_ $ entitiesL . playersL . ix p %~ \(PlayerIdentity attrs) -> PlayerIdentity attrs
        {playerIdentityDeck = Deck [top], playerIdentityDiscard = Discard [discarded]}
      before <- length . playerIdentityEncounterCards . toAttrs <$> getPlayer p
      _ <- putInHand p "01086"
      choose "initiate final top card" (isPlay top)
      attrs <- toAttrs <$> getPlayer p
      unDeck (playerIdentityDeck attrs) `shouldBe` [discarded]
      length (playerIdentityEncounterCards attrs) `shouldBe` before + 1
      pendingCost `shouldReturn` [Nothing]

  it "includes a top-card play in Team-Building Exercise and stacks both reductions" do
    tg <- testGame ["magik"]
    let p = player1 tg
    inGame tg do
      changeForm p
      exercise <- putIntoPlay p "12024"
      clearHand p
      [top] <- stackDeck p ["45048"]
      choose "use Team-Building Exercise" (isAbilityOfCard exercise)
      choose "play the top X-Men ally" (isPlay top)
      pass
      alliesWith "45048" >>= (`shouldSatisfy` notNull)
      playerIdentityTopDeckPlayUsed . toAttrs <$> getPlayer p `shouldReturn` True

  it "triggers Pixie's played-from-hand response when she was played from the deck" do
    tg <- testGame ["magik"]
    let p = player1 tg
    inGame tg do
      changeForm p
      clearHand p
      [discarded] <- stackDiscard p ["45048"]
      [pixie] <- stackDeck p ["36017"]
      payment <- putInHand p "01086"
      playWith pixie [payment]
      choose "Pixie's hand-play response" (isAbilityOfCard pixie)
      choose "return the discarded X-Men ally" (isTargetChoice discarded)
      handOf p >>= (`shouldSatisfy` elem discarded)

  it "offers matching defense events from the top during the villain phase" do
    tg <- testGame ["magik"]
    let p = player1 tg
    inGame tg do
      changeForm p
      clearHand p
      [barrier] <- stackDeck p ["45040"]
      v <- villainIdNow
      withGame_ $ phaseL .~ VillainPhase
      runNow [VillainMessage v (VillainAttacks p)]
      offers (isPlay barrier) `shouldReturn` True
      choose "play the top defense" (isPlay barrier)
      pass
      playerIdentityTopDeckPlayUsed . toAttrs <$> getPlayer p `shouldReturn` True
      discardOf p >>= (`shouldSatisfy` elem barrier)

  it "does not discount a deck-play ability's generic play check" do
    tg <- testGame ["magik"]
    let p = player1 tg
    inGame tg do
      changeForm p
      clearHand p
      [top] <- stackDeck p ["01093"]
      _ <- putInHand p "01086"
      player <- getPlayer p
      getModifiedCost player top Nothing `shouldReturn` 2
      isPlayable player top `shouldReturn` False
      offers (isPlay top) `shouldReturn` True

  it "blanking Magik suppresses both the play permission and public top visibility" do
    tg <- testGame ["magik"]
    let p = player1 tg
    inGame tg do
      changeForm p
      clearHand p
      [top] <- stackDeck p ["12033"]
      offers (isPlay top) `shouldReturn` True
      eid <- getRandom
      withGame_ $ entitiesL . effectsL . at eid ?~ Effect (BlankTopDeck EffectAttrs
        {effectId = eid, effectCardCode = "45030a", effectSource = GameRef
        ,effectMatcher = IdentityEntity (IdentityWithId p), effectModifiers = [Blank], effectEnds = Nothing})
      offers (isPlay top) `shouldReturn` False
      view <- liftIO . publicGameFor (const True) =<< getGame
      playerField "deckTop" view `shouldBe` Just Null

  it "Mutant Protectors cannot put the top ally into play as if it was in hand" do
    tg <- testGame ["magik"]
    let p = player1 tg
    inGame tg do
      changeForm p
      clearHand p
      _ <- stackDeck p ["45031"]
      protectors <- putInHand p "32017"
      _ <- putInHand p "01086"
      v <- villainIdNow
      runNow [VillainMessage v (VillainAttacks p)]
      offers (isPlay protectors) `shouldReturn` False
