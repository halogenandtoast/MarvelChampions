module Marvel.CoverageSpec (spec) where

import Data.Aeson (eitherDecodeFileStrict')
import Data.Aeson.Types (parseEither)
import Data.HashMap.Strict qualified as HashMap
import Marvel.Ally (allAllies)
import Marvel.AlterEgo (allAlterEgos)
import Marvel.Card.Code
import Marvel.Event (allEvents)
import Marvel.Hero (allHeroes)
import Marvel.Obligation (allObligations)
import Marvel.PlayerCard (allPlayerCards)
import Marvel.Support (allSupports)
import Marvel.Upgrade (allUpgrades)
import TestImport

data CoreCard = CoreCard {code :: CardCode, name :: Text, cardType :: Text}

-- | The core set's cards as MarvelCDB lists them.
coreCards :: IO [CoreCard]
coreCards = do
  json <- either error pure =<< eitherDecodeFileStrict' "frontend/public/data/cards.json"
  either error pure $ flip parseEither json $ withObject "cards" \o -> do
    cards <- o .: "cards"
    fmap catMaybes $ for cards $ withObject "card" \c -> do
      pack' <- c .: "packCode"
      if pack' == ("core" :: Text)
        then Just <$> (CoreCard <$> c .: "code" <*> c .: "name" <*> c .: "type")
        else pure Nothing

spec :: Spec
spec = describe "the core set" do
  it "has every player card implemented" do
    cards <- coreCards
    let implemented c = case c.cardType of
          "hero" -> HashMap.member c.code allHeroes
          "alter_ego" -> HashMap.member c.code allAlterEgos
          "ally" -> HashMap.member c.code allAllies
          "event" -> HashMap.member c.code allEvents
          "support" -> HashMap.member c.code allSupports
          "upgrade" -> HashMap.member c.code allUpgrades
          "resource" -> HashMap.member c.code allPlayerCards
          "obligation" -> HashMap.member c.code allObligations
          _ -> True
    [(c.code, c.name) | c <- cards, not (implemented c)] `shouldBe` []
