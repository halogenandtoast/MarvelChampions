-- | A deck list as a player brings it to the table: a hero and the cards with them, by
-- MarvelCDB code, and the starter decks of the core set.
module Marvel.Decklist (
  DeckList (..),
  loadPlayer,
  checkedDeckList,
  missingCards,
  aspectsBalanced,
  sharedHeroes,
  campaignDeck,
  starterDecks,
  lookupStarterDeck,
) where

import Marvel.Prelude

import Data.HashMap.Strict qualified as HashMap
import Data.List (find, nub)
import Data.Text qualified as T
import Marvel.AlterEgo.Cards (allAlterEgosMap)
import Marvel.Campaign (Campaign (..), CampaignLog (..), campaignCards, campaignPlayer)
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Deck
import Marvel.Game (initPlayer)
import Marvel.Hero.Cards (allHeroesMap)
import Marvel.Identity (PlayerIdentity)
import Marvel.Name
import Marvel.PlayerCard

data DeckList = DeckList
  { hero :: CardCode
  -- ^ either face of the identity
  , aspects :: [Text]
  , cards :: [(CardCode, Int)]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | The cards of a deck the engine cannot play: an identity it does not know, or player
-- cards it has no definition for.
missingCards :: DeckList -> [CardCode]
missingCards d =
  [d.hero | not knownIdentity] <> nub [c | (c, _) <- d.cards, not (HashMap.member c allPlayerCards)]
 where
  knownIdentity =
    HashMap.member (toHeroCardCode d.hero) allHeroesMap
      && HashMap.member (toAlterEgoCardCode d.hero) allAlterEgosMap

-- | Jessica Drew's Double Agent: her deck is built with two aspects, as many cards of
-- each (FAQ, Jessica Drew #31B), her own identity-specific cards aside. Any other deck
-- passes.
aspectsBalanced :: DeckList -> Bool
aspectsBalanced d = case lookup (toBaseCardCode d.hero) doubleAgents of
  Nothing -> True
  Just own -> case d.aspects of
    [a, b] | a /= b -> count own a == count own b
    _ -> False
 where
  doubleAgents = HashMap.fromList [("04031", map CardCode ["04032", "04033", "04034", "04035", "04036", "04037", "04038", "04039"])]
  count own aspect =
    sum
      [ n
      | (c, n) <- d.cards
      , c `notElem` own
      , Just def <- [HashMap.lookup c allPlayerCards]
      , (T.toLower . tshow <$> cdAspect def) == Just (T.toLower aspect)
      ]

-- | The deck, if the engine can play it.
checkedDeckList :: DeckList -> Either [CardCode] DeckList
checkedDeckList d = case missingCards d of
  [] -> Right d
  missing -> Left missing

-- | A player for the deck, its cards given ids from the generator.
loadPlayer :: (MonadRandom m) => DeckList -> m (Either [CardCode] PlayerIdentity)
loadPlayer d = case checkedDeckList d of
  Left missing -> pure $ Left missing
  Right _ -> do
    cards <- for (concatMap (\(c, n) -> replicate n c) d.cards) \code -> do
      cardId <- getRandom
      pure $ MkPlayerCard cardId (lookupPlayerCard code) Nothing Nothing
    Right <$> initPlayer (toHeroCardCode d.hero) (Deck cards)

-- | The deck a player brings to a campaign's next game: the one they began it with, the
-- cards the campaign log adds to it, and without the allies removed from the campaign.
-- What the campaign adds does not count toward the deck's size.
campaignDeck :: Campaign -> DeckList -> DeckList
campaignDeck c d =
  d
    { cards =
        [(code, n) | (code, n) <- d.cards, code `notElem` c.campaignLog.alliesRemoved]
          <> [(code, 1) | Just p <- [campaignPlayer d.hero c], code <- campaignCards p]
    }

-- | The titles of heroes that more than one of the decks bring: each hero is unique.
sharedHeroes :: [DeckList] -> [Text]
sharedHeroes ds =
  nub [title | (title, n) <- HashMap.toList counts, n > (1 :: Int)]
 where
  counts = HashMap.fromListWith (+) [(heroTitle d, 1) | d <- ds]
  heroTitle d = maybe (unCardCode d.hero) (display . cdName) $ lookup (toHeroCardCode d.hero) allHeroesMap

-- | The core set's five starter decks, by the hero's name in lower case, as MarvelCDB
-- lists them (decklists 1 to 5), and the hero packs' decks.
starterDecks :: [(Text, DeckList)]
starterDecks =
  [ ("spider-man", starter "01001a" "justice" $ ["01002", "01003", "01003", "01004", "01004", "01005", "01005", "01005", "01006", "01007", "01007", "01008", "01008", "01009", "01009"] <> ["01058", "01059", "01060", "01060", "01061", "01061", "01062", "01062", "01063", "01063", "01064", "01064", "01065", "01065"])
  , ("captain-marvel", starter "01010a" "leadership" $ ["01011", "01012", "01012", "01012", "01013", "01013", "01013", "01014", "01014", "01015", "01016", "01017", "01017", "01018", "01018"] <> ["01066", "01067", "01068", "01069", "01069", "01070", "01070", "01071", "01071", "01072", "01072", "01073", "01074", "01074"])
  , ("she-hulk", starter "01019a" "aggression" $ ["01020", "01021", "01022", "01022", "01023", "01023", "01024", "01024", "01024", "01025", "01026", "01027", "01027", "01028", "01028"] <> aggression)
  , ("iron-man", starter "01029a" "aggression" $ ["01030", "01031", "01031", "01031", "01032", "01032", "01033", "01034", "01035", "01036", "01037", "01038", "01038", "01039", "01039"] <> aggression)
  , ("black-panther", starter "01040a" "protection" $ ["01041", "01042", "01043a", "01043b", "01043c", "01043d", "01043d", "01044", "01044", "01044", "01045", "01046", "01047", "01048", "01049"] <> ["01075", "01076", "01077", "01077", "01078", "01078", "01079", "01079", "01080", "01080", "01081", "01081", "01082", "01082"])
  , -- the Captain America hero pack's deck: its own basic cards, not the core set's
    ("captain-america", DeckList "03001a" ["leadership"] $ tally $ ["03002", "03003", "03003", "03004", "03004", "03004", "03005", "03005", "03006", "03006", "03007", "03008", "03009", "03010", "03010"] <> ["03011", "03012", "03013", "03014", "03015", "03015", "03015", "03016", "03016", "03017", "03017", "03017", "03018", "03018", "03019", "03019", "03019"] <> ["03020", "03021", "03022", "03023", "03024", "03025", "03025", "03025"])
  , -- the Ms. Marvel hero pack's: her cards, its Protection cards and its basic cards
    ("ms-marvel", DeckList "05001a" ["protection"] $ tally $ ["05002", "05003", "05003", "05003", "05004", "05004", "05004", "05005", "05005", "05006", "05007", "05008", "05009", "05010", "05011"] <> ["05012", "05013", "05013", "05014", "05014", "05014", "05015", "05015", "05015", "05016", "05016", "05017", "05017", "05017"] <> ["05018", "05019", "05020", "05021", "05022", "05023", "05023", "05023", "05024", "05024", "05024"])
  , -- the pack's own deck: its hero cards, then its Justice and basic cards (11 to 24)
    ("black-widow", DeckList "08001a" ["justice"] . tally $ ["08002", "08003", "08003", "08004", "08004", "08005", "08006", "08006", "08007", "08007", "08008", "08008", "08009", "08010", "08010"] <> ["08011", "08012", "08013", "08013", "08013", "08014", "08014", "08015", "08015", "08016", "08016", "08017", "08017", "08017", "08018", "08018", "08018"] <> ["08019", "08020", "08021", "08022", "08023", "08024", "08024", "08024"])
  , -- the Thor hero pack's: his cards, its Aggression cards and its basic cards
    ("thor", DeckList "06001a" ["aggression"] $ tally $ ["06002", "06003", "06003", "06003", "06004", "06005", "06005", "06005", "06006", "06006", "06007", "06008", "06008", "06009", "06010"] <> ["06011", "06012", "06013", "06013", "06014", "06014", "06014", "06015", "06015", "06015", "06016", "06016", "06017", "06018", "06018", "06018", "06019"] <> ["06020", "06021", "06021", "06021", "06022", "06023", "06024", "06025"])
  , -- the Doctor Strange hero pack's own: its Protection and basic cards, not the core set's
    ("doctor-strange", DeckList "09001a" ["protection"] $ tally $ ["09002", "09003", "09003", "09004", "09004", "09005", "09005", "09006", "09007", "09007", "09008", "09009", "09010", "09010", "09011"] <> ["09012", "09013", "09014", "09015", "09015", "09015", "09016", "09016", "09016", "09017", "09017", "09018", "09018", "09019", "09020", "09020", "09020"] <> ["09021", "09021", "09021", "09022", "09023", "09024", "09025", "09026"])
  , -- the Hulk pack's own deck: its signature cards, Aggression and basic cards
    ("hulk", DeckList "10001a" ["aggression"] (tally $ ["10002", "10002", "10003", "10003", "10004", "10004", "10005", "10005", "10006", "10006", "10007", "10007", "10008", "10009", "10010"] <> ["10011", "10012", "10013", "10014", "10014", "10014", "10015", "10015", "10015", "10016", "10016", "10016", "10017", "10017", "10018", "10018", "10018"] <> ["10019", "10019", "10019", "10020", "10021", "10022", "10023", "10024"]))
  -- The Green Goblin

  -- The Wrecking Crew

  -- The Rise of Red Skull
  , -- the Rise of Red Skull box's: his signature, Leadership and basic cards
    ("hawkeye", DeckList "04001a" ["leadership"] $ tally ["04002", "04003", "04004", "04005", "04005", "04006", "04006", "04007", "04007", "04008", "04008", "04009", "04009", "04010", "04010", "04011", "04012", "04013", "04014", "04015", "04015", "04015", "04016", "04016", "04016", "04017", "04017", "04017", "04018", "04018", "04019", "04019", "04020", "04021", "04022", "04022", "04022", "04023", "04024", "04025"])
  , -- two aspects, as many cards of each (Double Agent)
    ("spider-woman", DeckList "04031a" ["aggression", "justice"] $ tally ["04032", "04033", "04033", "04034", "04035", "04035", "04036", "04036", "04037", "04037", "04038", "04038", "04039", "04039", "04039", "04040", "04041", "04041", "04042", "04042", "04043", "04043", "04043", "04044", "04044", "04044", "04045", "04046", "04046", "04047", "04047", "04047", "04048", "04048", "04049", "04049", "04049", "04050", "04051", "04052"])

  -- The Once and Future Kang

  ]
 where
  starter code aspect cs = DeckList code [aspect] (tally (cs <> basics))
  aggression = ["01050", "01051", "01052", "01052", "01053", "01053", "01054", "01054", "01055", "01055", "01056", "01056", "01057", "01057"]
  basics = ["01083", "01084", "01085", "01086", "01087", "01088", "01089", "01090", "01091", "01092", "01093"]
  tally cs = HashMap.toList $ HashMap.fromListWith (+) [(CardCode c, 1) | c <- cs]

lookupStarterDeck :: Text -> Maybe DeckList
lookupStarterDeck k = snd <$> find ((== k) . fst) starterDecks
