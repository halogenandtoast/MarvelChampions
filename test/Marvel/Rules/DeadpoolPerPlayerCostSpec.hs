module Marvel.Rules.DeadpoolPerPlayerCostSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Effect.Types (Attrs (..), Effect (..), IsEffect)
import Marvel.Identity (getModifiedCost)
import Marvel.Matchers
import Marvel.Modifier
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Event.Types qualified as Event
import TestImport

newtype PerPlayerCostDiscount = PerPlayerCostDiscount (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance RunMessage PerPlayerCostDiscount where
  runMessage msg (PerPlayerCostDiscount attrs) = PerPlayerCostDiscount <$> runMessage msg attrs

reduceCost :: IdentityId -> GameAppT ()
reduceCost you = do
  eid <- getRandom
  withGame_ $ entitiesL . effectsL . at eid ?~ Effect (PerPlayerCostDiscount EffectAttrs
    { effectId = eid, effectCardCode = "44046", effectSource = GameRef
    , effectMatcher = IdentityEntity $ IdentityWithId you
    , effectModifiers = [ResourceCostReduction 1], effectEnds = Nothing })

spec :: Spec
spec = describe "Deadpool per-player printed costs" do
  it "resolves the initial physical deck for the complete table size" do
    let setup = fromMaybe (error "no Rhino setup") $ recommendedSetup "01094"
        deck = (starter "deadpool") {cards = (starter "deadpool").cards <> [("44046", 1)]}
    (gameApp, ran, logged) <- dealtGameLogged setup [deck, starter "spider-man"] 1
    seats <- runGameApp gameApp (getsGame gameSeats)
    let tg = TestGame gameApp ran logged seats
    inGame tg do
      toFirstTurn
      attrs <- playerAttrs (player1 tg)
      let cards = unHand attrs.playerIdentityHand <> unDeck attrs.playerIdentityDeck <> unDiscard attrs.playerIdentityDiscard
      [cdCost $ getCardDef c | c <- cards, toCardCode c == "44046"] `shouldBe` [Just 6]

  it "makes War deal six damage when it discards Break Time in a two-player game" do
    tg <- testGame ["deadpool", "spider-man"]
    inGame tg do
      let you = player1 tg
      clearHand you
      changeForm you
      pass
      war <- putIntoPlay you "44058"
      _ <- setEncounterDeck [Treacheries.advance] []
      _ <- stackDeck you ["44046"]
      choose "use War" (isAbilityOfCard war)
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 6
      damageOf you `shouldReturn` 0

  it "uses the scaled printed value in pure cost matchers and gameplay catalogues" do
    tg <- testGame ["deadpool", "spider-man"]
    inGame tg do
      card <- putInHand (player1 tg) "44046"
      cardMatch (CardWithPrintedCostAtMost 3) card `shouldBe` False
      cardMatch (CardWithPrintedCostAtLeast 6) card `shouldBe` True
      cdCost <$> lookupPlayerCardInGame "44046" `shouldReturn` Just 6
      catalogue <- getPlayerCardCatalogue
      (cdCost =<< HashMap.lookup "44046" catalogue) `shouldBe` Just 6
      cdCost (lookupPlayerCard "44046") `shouldBe` Just 3

  it "keeps the starting player count after elimination, including for newly created cards" do
    tg <- testGame ["deadpool", "spider-man"]
    inGame tg do
      resolve [IdentityMessage (player2 tg) IdentityDefeated]
      getStartingPlayerCount `shouldReturn` 2
      card <- putInHand (player1 tg) "44046"
      cdCost (getCardDef card) `shouldBe` Just 6

  it "applies a one-resource reduction to the total and pays five rather than multiplying the discount" do
    tg <- testGame ["deadpool", "spider-man"]
    inGame tg do
      let you = player1 tg
      clearHand you
      card <- putInHand you "44046"
      reduceCost you
      identity <- getPlayer you
      getModifiedCost identity card Nothing `shouldReturn` 5
      confidence <- putInHand you "44025"
      energy <- putInHand you "01088"
      playWith card [confidence, energy]
      choose "resume immediately" (isLabel "$label.breakTimeResume")
      pass
      discardOf you >>= (`shouldSatisfy` elem card)

  it "preserves the resolved physical definition through entity construction, repeated resolution and save reload" do
    tg <- testGame ["deadpool", "spider-man"]
    inGame tg do
      let you = player1 tg
      card <- putInHand you "44046"
      cdCost (Event.eventCardDef $ toAttrs $ createEvent you card) `shouldBe` Just 6
      let resolved = resolvePlayerCountCost 2 (lookupPlayerCard "44046")
      resolvePlayerCountCost 2 resolved `shouldBe` resolved
      game <- getGame
      case fromJSON @Game (toJSON game) of
        Error err -> error err
        Success reloaded -> do
          let reloadedHand = maybe [] (unHand . playerIdentityHand . toAttrs) $ HashMap.lookup you $ gamePlayers reloaded
          [cdCost $ getCardDef c | c <- reloadedHand, pcCardId c == pcCardId card] `shouldBe` [Just 6]
          resolveCardCostValues 2 (toJSON reloaded) `shouldBe` toJSON reloaded

  it "resolves fresh raw cards before they enter a hand, deck, discard pile or set-aside zone" do
    tg <- testGame ["deadpool", "spider-man"]
    inGame tg do
      let you = player1 tg
      clearHand you
      cid <- getRandom
      let raw = MkPlayerCard cid (lookupPlayerCard "44046") (Just you) (Just you)
          costOf c = cdCost $ getCardDef c
      resolve [IdentityMessage you (AddToHand raw)]
      hand <- handOf you
      [costOf c | c <- hand, pcCardId c == cid] `shouldBe` [Just 6]
      resolve [IdentityMessage you (PlaceOnTopOfDeck raw)]
      deck <- unDeck . playerIdentityDeck <$> playerAttrs you
      costOf (testHead deck) `shouldBe` Just 6
      resolve [IdentityMessage you (DiscardFrom FromDeck 1 Nothing)]
      discarded <- discardOf you
      [costOf c | c <- discarded, pcCardId c == cid] `shouldBe` [Just 6]
      resolve [SetAside [PlayerCard raw]]
      aside <- getSetAsidePlayerCards
      [costOf c | c <- aside, pcCardId c == cid] `shouldBe` [Just 6]
