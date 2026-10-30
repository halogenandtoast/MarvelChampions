{- | A deck list as a player brings it to the table: a hero and the cards with them, by
MarvelCDB code, and the starter decks of the core set.
-}
module Marvel.Decklist (
  DeckList (..),
  loadPlayer,
  checkedDeckList,
  missingCards,
  aspectsBalanced,
  deckBuildingError,
  sharedHeroes,
  campaignDeck,
  campaignDeckShortBy,
  marketCardsIn,
  campaignOnlyCardsIn,
  starterDecks,
  lookupStarterDeck,
) where

import Marvel.Prelude

import Data.HashMap.Strict qualified as HashMap
import Data.List (find, nub)
import Data.Text qualified as T
import Marvel.AlterEgo.Cards (allAlterEgosMap)
import Marvel.Aspect (Aspect (..))
import Marvel.Campaign (Campaign (..), CampaignLog (..), campaignCardsOf)
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Deck
import Marvel.EncounterSet (EncounterSet (MadTitansShadowCampaign, TheMarket))
import Marvel.Game (initPlayer)
import Marvel.Hero.Cards (allHeroesMap)
import Marvel.Identity (PlayerIdentity)
import Marvel.Keyword (Keyword (Permanent))
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

{- | The cards of a deck the engine cannot play: an identity it does not know, or player
cards it has no definition for.
-}
missingCards :: DeckList -> [CardCode]
missingCards d =
  [d.hero | not knownIdentity] <> nub [c | (c, _) <- d.cards, not (HashMap.member c allPlayerCards)]
 where
  knownIdentity =
    HashMap.member (toHeroCardCode d.hero) allHeroesMap
      && HashMap.member (toAlterEgoCardCode d.hero) allAlterEgosMap

{- | Jessica Drew's Double Agent: her deck is built with two aspects, as many cards of
each (FAQ, Jessica Drew #31B), her own identity-specific cards aside. Any other deck
passes.
-}
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

{- | Why the deck breaks its identity's deck-building rule, as an i18n key: Jessica
Drew's Double Agent ('aspectsBalanced'), or Adam Warlock's Avatar of Life — as many
cards of each of the four aspects (at least one of each), and no more than one copy
of any card not his own, two printings of a card counted as one card. A deck from
MarvelCDB names no aspects for him; the cards say them.
-}
deckBuildingError :: DeckList -> Maybe Text
deckBuildingError d
  | not (aspectsBalanced d) = Just "error.unbalancedAspects"
  | toBaseCardCode d.hero == CardCode "21031" && not avatarOfLife = Just "error.avatarOfLife"
  | not withinCopyLimits = Just "error.deckCopyLimit"
  | not teamUpIdentity = Just "error.teamUpIdentity"
  | otherwise = Nothing
 where
  defs = [(def, n) | (c, n) <- d.cards, Just def <- [HashMap.lookup c allPlayerCards]]
  ofAspect a = sum [n | (def, n) <- defs, cdAspect def == Just a]
  perAspect = map ofAspect [Aggression, Justice, Leadership, Protection]
  -- his own cards are those of his hero set: no aspect, not basic
  notHisOwn def = isJust (cdAspect def)
  copies = HashMap.fromListWith (+) [(canonicalCardCode def, n) | (def, n) <- defs, notHisOwn def]
  avatarOfLife = all (> 0) perAspect && length (nub perAspect) == 1 && all (<= 1) (HashMap.elems copies)
  -- The copies of reprints are one card. Old identity-specific definitions had
  -- no copy metadata; keep those legacy decks compatible until annotated.
  deckCopies = HashMap.fromListWith (+) [(canonicalCardCode def, n) | (def, n) <- defs, n > 0]
  copyLimits = HashMap.fromListWith min
    [ (canonicalCardCode def, fromIntegral n :: Int)
    | (def, _) <- defs
    , Just n <- [cdDeckLimit def <|> (3 <$ cdAspect def)]
    ]
  withinCopyLimits = all
    (\(code, n) -> maybe True (n <=) (HashMap.lookup code copyLimits))
    (HashMap.toList deckCopies)
  identityTitles = map (title . cdName) $ catMaybes
    [ HashMap.lookup (toHeroCardCode d.hero) allHeroesMap
    , HashMap.lookup (toAlterEgoCardCode d.hero) allAlterEgosMap
    ]
  teamUpIdentity = and
    [ a `elem` identityTitles || b `elem` identityTitles
    | (def, n) <- defs, n > 0, Just (a, b) <- [cdTeamUp def]
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

{- | The deck a player brings to a campaign's next game: the one they began it with, the
cards the campaign log adds to it (the Market cards they bought among them), and without the allies removed from the campaign.
What the campaign adds does not count toward the deck's size.
-}
campaignDeck :: Campaign -> DeckList -> DeckList
campaignDeck c d =
  d
    { cards =
        [(code, n) | (code, n) <- d.cards, code `notElem` c.campaignLog.alliesRemoved]
          <> [(code, 1) | code <- campaignCardsOf d.hero c]
    }

{- | How many cards the player must add to the deck they began the campaign with, once the
allies removed from the campaign are taken out: "If this causes your deck to fall below
the minimum number of cards, then you must add a card to your deck" (TRORS rulebook
p.12, Zola). The minimum is 40, permanent cards and what the campaign adds not counted
(RR, Deck Customization).
-}
campaignDeckShortBy :: Campaign -> DeckList -> Int
campaignDeckShortBy c d = max 0 (40 - counted)
 where
  counted =
    sum
      [ n
      | (code, n) <- d.cards
      , code `notElem` c.campaignLog.alliesRemoved
      , Just def <- [HashMap.lookup code allPlayerCards]
      , Permanent `notElem` cdKeywords def
      ]

{- | The cards of The Market in the deck: bought with units in The Galaxy's Most Wanted
campaign, they "cannot be included in a deck unless players are playing" it (rulebook
p.4), the campaign adding them to the deck it deals ('campaignDeck').
-}
marketCardsIn :: DeckList -> [CardCode]
marketCardsIn d = nub [c | (c, _) <- d.cards, Just def <- [HashMap.lookup c allPlayerCards], cdEncounterSet def == Just TheMarket]

{- | The Mad Titan's Shadow's campaign-only cards in the deck (180-193): they "cannot be
included in any deck unless playing The Mad Titan's Shadow campaign and the players were
directed to add them to a deck by the Campaign Instructions" (rulebook p.4), which the
campaign does as it deals each game, never in a deck brought to the table.
-}
campaignOnlyCardsIn :: DeckList -> [CardCode]
campaignOnlyCardsIn d = nub [c | (c, _) <- d.cards, Just def <- [HashMap.lookup c allPlayerCards], cdEncounterSet def == Just MadTitansShadowCampaign]

-- | The titles of heroes that more than one of the decks bring: each hero is unique.
sharedHeroes :: [DeckList] -> [Text]
sharedHeroes ds =
  nub [title | (title, n) <- HashMap.toList counts, n > (1 :: Int)]
 where
  counts = HashMap.fromListWith (+) [(heroTitle d, 1) | d <- ds]
  heroTitle d = maybe (unCardCode d.hero) (display . cdName) $ lookup (toHeroCardCode d.hero) allHeroesMap

{- | The core set's five starter decks, by the hero's name in lower case, as MarvelCDB
lists them (decklists 1 to 5), and the hero packs' decks.
-}
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
  , -- The Green Goblin

    -- The Wrecking Crew

    -- The Rise of Red Skull
    -- the Rise of Red Skull box's: his signature, Leadership and basic cards
    ("hawkeye", DeckList "04001a" ["leadership"] $ tally ["04002", "04003", "04004", "04005", "04005", "04006", "04006", "04007", "04007", "04008", "04008", "04009", "04009", "04010", "04010", "04011", "04012", "04013", "04014", "04015", "04015", "04015", "04016", "04016", "04016", "04017", "04017", "04017", "04018", "04018", "04019", "04019", "04020", "04021", "04022", "04022", "04022", "04023", "04024", "04025"])
  , -- two aspects, as many cards of each (Double Agent)
    ("spider-woman", DeckList "04031a" ["aggression", "justice"] $ tally ["04032", "04033", "04033", "04034", "04035", "04035", "04036", "04036", "04037", "04037", "04038", "04038", "04039", "04039", "04039", "04040", "04041", "04041", "04042", "04042", "04043", "04043", "04043", "04044", "04044", "04044", "04045", "04046", "04046", "04047", "04047", "04047", "04048", "04048", "04049", "04049", "04049", "04050", "04051", "04052"])
  , -- The Once and Future Kang

    -- Ant-Man: his signature, Leadership and basic cards
    ("ant-man", DeckList "12001a" ["leadership"] $ tally ["12002", "12003", "12003", "12004", "12005", "12005", "12006", "12006", "12007", "12007", "12007", "12008", "12009", "12009", "12010", "12011", "12012", "12013", "12014", "12015", "12015", "12015", "12016", "12016", "12016", "12017", "12017", "12017", "12018", "12018", "12018", "12019", "12019", "12020", "12021", "12022", "12023", "12024", "12024", "12024"])
  , -- Wasp: her signature, Aggression and basic cards
    ("wasp", DeckList "13001a" ["aggression"] $ tally ["13002", "13003", "13003", "13004", "13004", "13004", "13005", "13005", "13006", "13006", "13007", "13007", "13008", "13009", "13010", "13011", "13012", "13013", "13013", "13013", "13014", "13014", "13014", "13015", "13015", "13016", "13016", "13016", "13017", "13017", "13017", "13018", "13019", "13020", "13021", "13022", "13023", "13024", "13024", "13025"])
  , -- Quicksilver: his signature, Protection and basic cards
    ("quicksilver", DeckList "14001a" ["protection"] $ tally ["14002", "14003", "14003", "14003", "14003", "14004", "14004", "14005", "14005", "14006", "14007", "14008", "14009", "14010", "14011", "14012", "14012", "14012", "14013", "14014", "14014", "14014", "14015", "14015", "14015", "14016", "14016", "14017", "14017", "14017", "14018", "14019", "14020", "14021", "14022", "14022", "14022", "14023", "14023", "14023"])
  , -- Scarlet Witch: her signature, Justice and basic cards
    ("scarlet-witch", DeckList "15001a" ["justice"] $ tally ["15002", "15003", "15004", "15004", "15004", "15004", "15005", "15005", "15005", "15006", "15007", "15008", "15008", "15008", "15009", "15010", "15011", "15012", "15012", "15012", "15013", "15013", "15013", "15014", "15014", "15014", "15015", "15015", "15015", "15016", "15016", "15017", "15017", "15018", "15019", "15019", "15019", "15020", "15021", "15022"])
  , -- Groot: his signature, Protection and basic cards
    ("groot", DeckList "16001a" ["protection"] $ tally ["16002", "16002", "16003", "16003", "16004", "16004", "16005", "16005", "16005", "16006", "16007", "16008", "16009", "16010", "16011", "16012", "16013", "16013", "16013", "16014", "16014", "16014", "16015", "16015", "16016", "16016", "16016", "16017", "16017", "16017", "16018", "16018", "16019", "16020", "16021", "16022", "16023", "16024", "16024", "16024"])
  , -- Rocket Raccoon: his signature, Aggression and basic cards; Flora and Fauna (16048) is
    -- the Groot pack's card, added with it
    ("rocket-raccoon", DeckList "16029a" ["aggression"] $ tally ["16030", "16030", "16031", "16031", "16032", "16033", "16033", "16034", "16034", "16035", "16036", "16037", "16038", "16038", "16039", "16040", "16041", "16041", "16042", "16042", "16042", "16043", "16043", "16043", "16044", "16044", "16045", "16045", "16045", "16046", "16046", "16046", "16047", "16048", "16049", "16050", "16051", "16052", "16052", "16052"])
  , -- Star-Lord: his signature, Leadership and basic cards
    ("star-lord", DeckList "17001a" ["leadership"] $ tally ["17002", "17003", "17003", "17003", "17004", "17004", "17005", "17005", "17005", "17006", "17007", "17007", "17008", "17009", "17010", "17011", "17012", "17013", "17014", "17014", "17014", "17015", "17015", "17015", "17016", "17016", "17017", "17017", "17017", "17018", "17018", "17019", "17019", "17019", "17020", "17021", "17022", "17023", "17023", "17023"])
  , -- Gamora: her signature, Aggression and basic cards, and Justice and Protection events (Skilled Tactician)
    ("gamora", DeckList "18001a" ["aggression"] $ tally ["18002", "18003", "18003", "18004", "18004", "18005", "18005", "18006", "18006", "18007", "18007", "18008", "18009", "18009", "18010", "18011", "18012", "18012", "18012", "18013", "18013", "18013", "18014", "18014", "18015", "18015", "18015", "18016", "18016", "18016", "18017", "18017", "18018", "18019", "18020", "18020", "18020", "18021", "18022", "18023"])
  , -- Drax: his signature, Protection and basic cards
    ("drax", DeckList "19001a" ["protection"] $ tally ["19002", "19003", "19003", "19004", "19004", "19005", "19005", "19006", "19006", "19007", "19007", "19008", "19009", "19010", "19011", "19012", "19013", "19014", "19014", "19015", "19015", "19015", "19016", "19016", "19016", "19017", "19017", "19017", "19018", "19018", "19018", "19019", "19019", "19020", "19021", "19021", "19021", "19022", "19023", "19024"])
  , -- Venom: his signature, Justice and basic cards
    ("venom", DeckList "20001a" ["justice"] $ tally ["20002", "20002", "20003", "20003", "20004", "20005", "20005", "20005", "20006", "20006", "20007", "20008", "20009", "20010", "20010", "20011", "20012", "20012", "20012", "20013", "20013", "20013", "20014", "20014", "20015", "20015", "20015", "20016", "20017", "20018", "20019", "20020", "20020", "20020", "20021", "20021", "20021", "20022", "20022", "20022"])
  , -- Spectrum: her signature, Leadership and basic cards; 40 and the 3 Permanent energy forms
    ("spectrum", DeckList "21001a" ["leadership"] $ tally ["21002", "21003", "21004", "21005", "21006", "21006", "21007", "21007", "21007", "21008", "21008", "21008", "21009", "21009", "21009", "21010", "21010", "21010", "21011", "21012", "21013", "21014", "21015", "21015", "21015", "21016", "21016", "21016", "21017", "21017", "21017", "21018", "21018", "21018", "21019", "21020", "21021", "21022", "21022", "21022", "21023", "21024", "21025"])
  , -- Adam Warlock: his signature cards, six of each aspect and a basic card (MTS
    -- rulebook p.3)
    ("adam-warlock", DeckList "21031a" ["aggression", "justice", "leadership", "protection"] $ tally ["21032", "21033", "21034", "21035", "21036", "21036", "21037", "21037", "21038", "21038", "21038", "21039", "21039", "21040", "21040", "21041", "21042", "21043", "21044", "21045", "21046", "21047", "21048", "21049", "21050", "21051", "21052", "21053", "21054", "21055", "21056", "21057", "21058", "21059", "21060", "21061", "21062", "21063", "21064", "21065"])
  , ("nebula", DeckList "22001a" ["justice"] $ tally ["22002", "22003", "22004", "22004", "22005", "22006", "22007", "22007", "22008", "22008", "22009", "22009", "22010", "22010", "22010", "22011", "22012", "22013", "22014", "22014", "22014", "22015", "22015", "22015", "22016", "22017", "22018", "22018", "22018", "22019", "22019", "22020", "22021", "22022", "22023", "22023", "22024", "22025", "22026"])
  , ("war-machine", DeckList "23001a" ["leadership"] $ tally ["23002", "23003", "23004", "23005", "23005", "23006", "23007", "23008", "23008", "23009", "23009", "23010", "23010", "23011", "23011", "23012", "23013", "23014", "23015", "23016", "23016", "23016", "23017", "23017", "23017", "23018", "23018", "23018", "23019", "23019", "23019", "23020", "23020", "23021", "23022", "23023", "23024", "23025", "23026", "23027"])
  , ("valkyrie", DeckList "25001a" ["aggression"] $ tally ["25002", "25003", "25004", "25005", "25006", "25007", "25008", "25008", "25009", "25010", "25010", "25011", "25011", "25012", "25012", "25012", "25013", "25014", "25015", "25016", "25017", "25017", "25018", "25018", "25018", "25019", "25019", "25019", "25020", "25020", "25020", "25021", "25022", "25022", "25023", "25024", "25024", "25024", "25025", "25026", "25027"])
  , ("vision", DeckList "26001a" ["protection"] $ tally ["26002", "26003", "26004", "26005", "26006", "26007", "26007", "26008", "26008", "26008", "26009", "26009", "26010", "26010", "26011", "26012", "26013", "26014", "26015", "26016", "26016", "26016", "26017", "26017", "26018", "26018", "26018", "26019", "26019", "26019", "26020", "26020", "26021", "26022", "26023", "26024", "26024", "26024", "26025", "26026", "26027"])
  , ("ghost-spider", DeckList "27001a" ["protection"] $ tally ["27002", "27002", "27002", "27003", "27004", "27004", "27004", "27005", "27005", "27006", "27006", "27007", "27008", "27009", "27009", "27010", "27011", "27012", "27013", "27013", "27013", "27014", "27014", "27014", "27015", "27015", "27015", "27016", "27016", "27016", "27017", "27018", "27019", "27020", "27021", "27022", "27023", "27024", "27024", "27024"])
  , ("spider-man-miles-morales", DeckList "27030a" ["justice"] $ tally ["27031", "27031", "27032", "27032", "27033", "27033", "27034", "27034", "27034", "27035", "27036", "27037", "27038", "27039", "27039", "27040", "27041", "27042", "27042", "27042", "27043", "27043", "27043", "27044", "27044", "27044", "27045", "27045", "27046", "27047", "27048", "27049", "27050", "27051", "27052", "27053", "27054", "27054", "27054", "27055"])
  , ("nova", DeckList "28001a" ["aggression"] $ tally ["28002", "28003", "28003", "28004", "28004", "28004", "28005", "28005", "28005", "28006", "28006", "28007", "28007", "28008", "28009", "28010", "28011", "28011", "28012", "28012", "28012", "28013", "28013", "28013", "28014", "28014", "28014", "28015", "28015", "28016", "28016", "28016", "28017", "28017", "28017", "28018", "28019", "28019", "28019", "28020"])
  , ("spider-ham", DeckList "30001a" ["justice"] $ tally ["30002", "30003", "30003", "30004", "30005", "30006", "30006", "30007", "30007", "30007", "30008", "30009", "30009", "30010", "30011", "30012", "30013", "30014", "30014", "30014", "30015", "30015", "30016", "30016", "30016", "30017", "30017", "30017", "30018", "30018", "30018", "30019", "30019", "30019", "30020", "30021", "30022", "30022", "30022", "30023"])
  , ("ironheart", DeckList "29001a" ["leadership"] $ tally ["29004", "29005", "29005", "29006", "29006", "29006", "29007", "29007", "29008", "29009", "29009", "29010", "29011", "29012", "29013", "29014", "29015", "29016", "29017", "29017", "29017", "29018", "29018", "29018", "29019", "29019", "29019", "29020", "29020", "29020", "29021", "29021", "29022", "29023", "29024", "29025", "29026", "29027", "29027", "29027"])
  , ("spdr", DeckList "31001a" ["protection"] $ tally ["31003", "31004", "31004", "31004", "31005", "31005", "31006", "31006", "31007", "31008", "31009", "31010", "31011", "31012", "31013", "31014", "31015", "31016", "31016", "31016", "31017", "31017", "31017", "31018", "31018", "31018", "31019", "31019", "31019", "31020", "31020", "31020", "31021", "31022", "31023", "31023", "31023", "31024", "31024", "31024"])
  ]
 where
  starter code aspect cs = DeckList code [aspect] (tally (cs <> basics))
  aggression = ["01050", "01051", "01052", "01052", "01053", "01053", "01054", "01054", "01055", "01055", "01056", "01056", "01057", "01057"]
  basics = ["01083", "01084", "01085", "01086", "01087", "01088", "01089", "01090", "01091", "01092", "01093"]
  tally cs = HashMap.toList $ HashMap.fromListWith (+) [(CardCode c, 1) | c <- cs]

lookupStarterDeck :: Text -> Maybe DeckList
lookupStarterDeck k = snd <$> find ((== k) . fst) starterDecks
