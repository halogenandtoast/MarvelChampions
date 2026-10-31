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

data PackCard = PackCard {code :: CardCode, name :: Text, cardType :: Text, setCode :: Maybe Text}

-- | A pack's cards as MarvelCDB lists them.
packCards :: Text -> IO [PackCard]
packCards pack = cardsWhere (== pack) (inScope pack)

-- | The cards of the encounter set, by MarvelCDB set code.
setCards :: Text -> IO [PackCard]
setCards set = cardsWhere (const True) (== Just set)

cardsWhere :: (Text -> Bool) -> (Maybe Text -> Bool) -> IO [PackCard]
cardsWhere inPack inSet = do
  json <- either error pure =<< eitherDecodeFileStrict' "frontend/public/data/cards.json"
  either error pure $ flip parseEither json $ withObject "cards" \o -> do
    cards <- o .: "cards"
    fmap catMaybes $ for cards $ withObject "card" \c -> do
      pack' <- c .: "packCode"
      set <- c .:? "setCode"
      if inPack pack' && inSet set
        then Just <$> (PackCard <$> c .: "code" <*> c .: "name" <*> c .: "type" <*> pure set)
        else pure Nothing

-- | The packs implemented, by MarvelCDB pack code.
packs :: [Text]
packs = ["phoenix", "cyclops", "core", "cap", "msm", "bkw", "thor", "drs", "hlk", "trors", "twc", "toafk", "ant", "wsp", "qsv", "scw", "stld", "gam", "gmw", "drax", "vnm", "mts", "nebu", "warm", "valk", "vision", "sm", "nova", "ironheart", "spiderham", "spdr", "mut_gen"]

-- | The sets of a pack implemented so far: of The Rise of Red Skull, its two heroes and
-- their nemesis sets and the aspect and basic cards (set-less), its scenarios, modular
-- and campaign sets to come.
inScope :: Text -> Maybe Text -> Bool
inScope pack set = case pack of
  "trors" -> maybe True (`elem` ["hawkeye", "hawkeye_nemesis", "spider_woman", "spider_woman_nemesis"]) set
  -- The Mad Titan's Shadow: the player side so far, its heroes, their nemesis sets and
  -- the set-less aspect and basic cards
  "mts" -> maybe True (`elem` ["spectrum", "spectrum_nemesis", "warlock", "warlock_nemesis"]) set
  _ -> True

-- | The encounter sets (scenarios' and modular) implemented ahead of the rest of their pack, by MarvelCDB
-- set code.
modularSets :: [Text]
modularSets =
  [ "the_hood"
  , "risky_business"
  , "mutagen_formula"
  , "goblin_gimmicks"
  , "a_mess_of_things"
  , "power_drain"
  , "running_interference"
  , "temporal"
  , "anachronauts"
  , "mot"
  -- The Rise of Red Skull: Crossbones's and Absorbing Man's scenarios and modular sets
  , "crossbones"
  , "exper_weapon"
  , "absorbing_man"
  , "hydra_assault"
  , "weap_master"
  , "hydra_patrol"
  -- The Galaxy's Most Wanted
  , "ship_command"
  , "power_stone"
  , "badoon_headhunter"
  -- Brotherhood of Badoon
  , "brotherhood_of_badoon"
  , "band_of_badoon"
  -- Escape the Museum
  , "escape_the_museum"
  -- Ronan the Accuser
  , "ronan"
  , "kree_militant"
  -- The Galaxy's Most Wanted: Nebula
  , "nebula"
  , "space_pirates"
  -- The Galaxy's Most Wanted campaign
  , "challenge"
  -- Infiltrate the Museum
  , "infiltrate_the_museum"
  , "galactic_artifacts"
  , "menagerie_medley"
  -- The Mad Titan's Shadow: Ebony Maw
  , "ebony_maw"
  , "black_order"
  , "armies_of_titan"
  -- The Mad Titan's Shadow: Tower Defense
  , "tower_defense"
  -- The Mad Titan's Shadow: Thanos
  , "thanos"
  , "children_of_thanos"
  , "infinity_gauntlet"
  -- The Mad Titan's Shadow: Loki
  , "loki"
  -- The Mad Titan's Shadow: Hela, and the Enchantress set
  , "hela"
  , "legions_of_hel"
  , "frost_giants"
  , "enchantress"
  -- The Mad Titan's Shadow campaign
  , "mts_campaign"
  ]

encounterImplemented :: PackCard -> Bool
encounterImplemented c = case c.cardType of
  "villain" -> HashMap.member c.code allVillains
  "minion" -> HashMap.member c.code allMinions
  "treachery" -> HashMap.member c.code allTreacheries
  "attachment" -> HashMap.member c.code allAttachments
  "side_scheme" -> HashMap.member c.code allSideSchemes
  -- a double-sided one by its sides, MarvelCDB keeping an older record of one side by
  -- the base code (Avengers Tower, 21100)
  "environment" -> any (`HashMap.member` allEnvironments) [c.code, CardCode (unCardCode c.code <> "a")]
  "obligation" -> HashMap.member c.code allObligations
  -- an encounter set's support, as Ship Command's Milano
  "support" -> HashMap.member c.code allSupports
  -- main schemes are kept by their base code, the sides being one card
  "main_scheme" -> HashMap.member (toBaseCardCode c.code) allMainSchemes
  -- a hero set's own cards
  "hero" -> HashMap.member c.code allHeroes
  "alter_ego" -> HashMap.member c.code allAlterEgos
  "event" -> HashMap.member c.code allEvents
  "upgrade" -> HashMap.member c.code allUpgrades
  "resource" -> HashMap.member c.code allPlayerCards
  -- a scenario's player card, as Hela's Odin
  "ally" -> HashMap.member c.code allAllies
  _ -> True

spec :: Spec
spec = do
  describe "modular sets" $ for_ modularSets \set -> it (show set <> " has every card implemented") do
    cards <- setCards set
    length cards `shouldSatisfy` (> 0)
    [(c.code, c.name) | c <- cards, not (encounterImplemented c)] `shouldBe` []
  packSpec

packSpec :: Spec
packSpec = for_ packs \pack -> describe (show pack) do
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
