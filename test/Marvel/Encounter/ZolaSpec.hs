module Marvel.Encounter.ZolaSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.MainScheme.Types (Attrs (mainSchemeCounters))
import Marvel.Minion.Cards qualified as Cards
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types (Attrs (sideSchemeHeldCards), SideScheme)
import Marvel.Treachery.Cards qualified as Cards
import TestImport

zola :: IO TestGame
zola = testGameAgainst "04109" ["spider-man"]

testCountersNow :: GameAppT Natural
testCountersNow = getsGame (sum . map (mainSchemeCounters . toAttrs) . HashMap.elems . gameMainSchemes)

minionsEngagedWith :: IdentityId -> GameAppT [CardCode]
minionsEngagedWith p = getsGame (map (toCardCode . getCardDef) . filter ((== p) . minionEngagedIdentity . toAttrs) . HashMap.elems . gameMinions)

hydraPrison :: GameAppT [SideScheme]
hydraPrison = getsGame (filter ((== Cards.hydraPrison) . getCardDef) . HashMap.elems . gameSideSchemes)

spec :: Spec
spec = describe "Zola" do
  it "sets up Hydra Prison and an Ultimate Bio-Servant engaged with each player" do
    tg <- zola
    inGame tg do
      minionsEngagedWith (player1 tg) `shouldReturn` [toCardCode Cards.ultimateBioServant]
      length <$> hydraPrison `shouldReturn` 1

  it "places a test counter after step one, and at three puts a discarded-to minion into play with the first player" do
    tg <- zola
    inGame tg do
      let p = player1 tg
      _ <- stackEncounterDeck [Cards.mindRay, Cards.berserkMutate]
      resolve [AccelerateMainScheme]
      testCountersNow `shouldReturn` 1
      length <$> minionsEngagedWith p `shouldReturn` 1
      ms <- mainSchemeId
      resolve [PlaceCounters (MainSchemeRef ms) 1, AccelerateMainScheme]
      -- the third counter: Mind Ray is discarded on the way to Berserk Mutate
      testCountersNow `shouldReturn` 0
      engaged <- minionsEngagedWith p
      liftIO $ engaged `shouldSatisfy` elem (toCardCode Cards.berserkMutate)
      codes <- encounterDiscardCodes
      liftIO $ codes `shouldSatisfy` elem (toCardCode Cards.mindRay)
      liftIO $ codes `shouldSatisfy` notElem (toCardCode Cards.berserkMutate)

  it "Hydra Prison holds the hero's own ally, with threat for its cost, and gives it back when defeated" do
    tg <- zola
    inGame tg do
      let p = player1 tg
      [prison] <- hydraPrison
      let blackCat = "01002"
          held = sideSchemeHeldCards (toAttrs prison)
      liftIO $ map toCardCode held `shouldBe` [blackCat]
      -- 1, and Black Cat's cost of 2
      sideSchemeThreat (toId prison) `shouldReturn` 3
      cards <- (<>) <$> handOf p <*> ((<>) <$> discardOf p <*> (unDeck . (.playerIdentityDeck) <$> playerAttrs p))
      liftIO $ map toCardCode cards `shouldSatisfy` notElem blackCat
      resolve [SideSchemeMessage (toId prison) (SideSchemeThwarted (IdentityRef p) 3)]
      length <$> hydraPrison `shouldReturn` 0
      hand <- handOf p
      liftIO $ map toCardCode hand `shouldSatisfy` elem blackCat
      -- removed from the game, not discarded
      codes <- encounterDiscardCodes
      liftIO $ codes `shouldSatisfy` notElem (toCardCode Cards.hydraPrison)
