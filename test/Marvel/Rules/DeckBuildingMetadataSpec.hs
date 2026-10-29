module Marvel.Rules.DeckBuildingMetadataSpec (spec) where

import Data.Aeson.KeyMap qualified as KeyMap
import Test.Hspec (shouldBe)
import TestImport hiding (shouldBe)

withCards :: Text -> [(CardCode, Int)] -> DeckList
withCards hero cards = (starter hero) {cards = cards}

spec :: Spec
spec = describe "deck-building card metadata" do
  it "reads a legacy CardDef JSON object with neither new optional field" do
    let def = lookupPlayerCard "22022"
    case toJSON def of
      Object object -> fromJSON (Object $ KeyMap.delete "cdDeckLimit" $ KeyMap.delete "cdTeamUp" object)
        `shouldBe` Success def {cdDeckLimit = Nothing, cdTeamUp = Nothing}
      _ -> expectationFailure "CardDef JSON must be an object"

  it "round trips copy-limit and Team-Up metadata" do
    let def = lookupPlayerCard "22022"
    cdDeckLimit def `shouldBe` Just 1
    cdTeamUp def `shouldBe` Just ("Gamora", "Nebula")
    fromJSON (toJSON def) `shouldBe` Success def

  it "enforces max1 Daughters of Thanos and max2 Leadership Training" do
    deckBuildingError (withCards "nebula" [("22022", 1)]) `shouldBe` Nothing
    deckBuildingError (withCards "nebula" [("22022", 2)]) `shouldBe` Just "error.deckCopyLimit"
    deckBuildingError (withCards "valkyrie" [("25034", 2)]) `shouldBe` Nothing
    deckBuildingError (withCards "valkyrie" [("25034", 3)]) `shouldBe` Just "error.deckCopyLimit"

  it "enforces Vision's other printed training max2" do
    deckBuildingError (withCards "vision" [("26033", 2)]) `shouldBe` Nothing
    deckBuildingError (withCards "vision" [("26033", 3)]) `shouldBe` Just "error.deckCopyLimit"

  it "aggregates a resource and its reprint instead of granting separate limits" do
    deckBuildingError (withCards "valkyrie" [("01088", 1), ("25025", 1)]) `shouldBe` Just "error.deckCopyLimit"
    deckBuildingError (withCards "vision" [("26025", 1)]) `shouldBe` Nothing

  it "aggregates repeated entries of the same printing" do
    deckBuildingError (withCards "valkyrie" [("25034", 1), ("25034", 2)]) `shouldBe` Just "error.deckCopyLimit"

  it "uses the normal three-copy limit for an unannotated aspect/basic card" do
    deckBuildingError (withCards "spider-man" [("01086", 3)]) `shouldBe` Nothing
    deckBuildingError (withCards "spider-man" [("01086", 4)]) `shouldBe` Just "error.deckCopyLimit"

  it "annotates the packs' identity cards with their exact quantities" do
    deckBuildingError (withCards "vision" [("26007", 2)]) `shouldBe` Nothing
    deckBuildingError (withCards "vision" [("26007", 3)]) `shouldBe` Just "error.deckCopyLimit"
    deckBuildingError (withCards "valkyrie" [("25002", 2)]) `shouldBe` Just "error.deckCopyLimit"

  it "keeps an old identity-specific card without metadata backward compatible" do
    let deck = starter "adam-warlock"
    deckBuildingError deck {cards = ("21038", 1) : deck.cards} `shouldBe` Nothing

  for_ ["nebula", "gamora"] $ \hero ->
    it ("allows Daughters of Thanos for its named identity: " <> show hero) do
      deckBuildingError (withCards hero [("22022", 1)]) `shouldBe` Nothing

  it "does not allow another deck to include Daughters even if a named hero sits at the table" do
    deckBuildingError (withCards "spider-man" [("22022", 1)]) `shouldBe` Just "error.teamUpIdentity"

  for_ ["war-machine", "iron-man"] $ \hero ->
    it ("allows Two Against the World for its named identity: " <> show hero) do
      deckBuildingError (withCards hero [("23024", 1)]) `shouldBe` Nothing

  it "uses hero and alter-ego titles regardless of the supplied identity face" do
    let deck = withCards "war-machine" [("23024", 1)]
    deckBuildingError deck {hero = "23001b"} `shouldBe` Nothing
    deckBuildingError (withCards "vision" [("23024", 1)]) `shouldBe` Just "error.teamUpIdentity"

  it "annotates previously implemented Team-Ups as well as the new packs" do
    deckBuildingError (withCards "ant-man" [("12020", 1)]) `shouldBe` Nothing
    deckBuildingError (withCards "wasp" [("13020", 1)]) `shouldBe` Nothing
    deckBuildingError (withCards "vision" [("13020", 1)]) `shouldBe` Just "error.teamUpIdentity"
    deckBuildingError (withCards "quicksilver" [("14018", 1)]) `shouldBe` Nothing
    deckBuildingError (withCards "scarlet-witch" [("15018", 1)]) `shouldBe` Nothing
    deckBuildingError (withCards "rocket-raccoon" [("16048", 1)]) `shouldBe` Nothing

  it "ignores a zero-quantity entry when checking named-character restrictions" do
    deckBuildingError (withCards "vision" [("22022", 0)]) `shouldBe` Nothing

  it "preserves existing Adam Warlock and Spider-Woman error precedence" do
    let adam = starter "adam-warlock"
    deckBuildingError adam {cards = ("01054", 1) : adam.cards} `shouldBe` Just "error.avatarOfLife"
    deckBuildingError (starter "spider-woman") {aspects = ["aggression"]} `shouldBe` Just "error.unbalancedAspects"

  for_ ["nebula", "war-machine", "valkyrie", "vision"] $ \hero ->
    it ("accepts the complete printed starter deck: " <> show hero) do
      deckBuildingError (starter hero) `shouldBe` Nothing

  it "does not accept the internal Dense reverse as another deck card" do
    deckBuildingError (withCards "vision" [("26002b", 1)]) `shouldBe` Just "error.deckCopyLimit"
