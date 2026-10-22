module Marvel.CoverageSpec (spec) where

import Data.Aeson (eitherDecodeFileStrict')
import Data.Aeson.Types (parseEither)
import Data.HashMap.Strict qualified as HashMap
import Marvel.Ally (allAllies)
import Marvel.Attachment (allAttachments)
import Marvel.Environment (allEnvironments)
import Marvel.MainScheme (allMainSchemes)
import Marvel.Minion (allMinions)
import Marvel.SideScheme (allSideSchemes)
import Marvel.Treachery (allTreacheries)
import Marvel.Villain (allVillains)
import Marvel.AlterEgo (allAlterEgos)
import Marvel.Card.Code
import Marvel.Event (allEvents)
import Marvel.Hero (allHeroes)
import Marvel.Obligation (allObligations)
import Marvel.PlayerCard (allPlayerCards)
import Marvel.Support (allSupports)
import Marvel.Upgrade (allUpgrades)
import TestImport

data PackCard = PackCard {code :: CardCode, name :: Text, cardType :: Text}

-- | A pack's cards as MarvelCDB lists them.
packCards :: Text -> IO [PackCard]
packCards pack = do
  json <- either error pure =<< eitherDecodeFileStrict' "frontend/public/data/cards.json"
  either error pure $ flip parseEither json $ withObject "cards" \o -> do
    cards <- o .: "cards"
    fmap catMaybes $ for cards $ withObject "card" \c -> do
      pack' <- c .: "packCode"
      if pack' == pack
        then Just <$> (PackCard <$> c .: "code" <*> c .: "name" <*> c .: "type")
        else pure Nothing

-- | The packs implemented, by MarvelCDB pack code.
packs :: [Text]
packs = ["core", "cap", "msm", "bkw", "thor", "hlk"]

spec :: Spec
spec = for_ packs \pack -> describe (show pack) do
  it "has every player card implemented" do
    cards <- packCards pack
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
  it "has every encounter card implemented" do
    cards <- packCards pack
    let implemented c = case c.cardType of
          "villain" -> HashMap.member c.code allVillains
          "minion" -> HashMap.member c.code allMinions
          "treachery" -> HashMap.member c.code allTreacheries
          "attachment" -> HashMap.member c.code allAttachments
          "side_scheme" -> HashMap.member c.code allSideSchemes
          "environment" -> HashMap.member c.code allEnvironments
          -- main schemes are kept by their base code, the sides being one card
          "main_scheme" -> HashMap.member (toBaseCardCode c.code) allMainSchemes
          _ -> True
        -- MarvelCDB's collapsed row for the three Android Efficiency variants
        collapsed = ["01144"]
    [(c.code, c.name) | c <- cards, c.code `notElem` collapsed, not (implemented c)] `shouldBe` []
