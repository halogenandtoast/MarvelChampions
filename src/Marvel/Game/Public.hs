{-# LANGUAGE DuplicateRecordFields #-}

-- | The game as the client draws it: one record per thing on the table, each card by its
-- id and code with its stats as they now stand, and each choice of the open questions
-- described by what it is about. Decks are hidden information, so only their sizes go
-- out, and at a game with friends so do the hands of the players a viewer does not hold.
--
-- Stored games and exports keep the engine's own JSON; this is only ever sent.
module Marvel.Game.Public (publicGame, publicGameFor, owedFor, describeChoice, GameView (..), ChoiceView (..)) where

import Marvel.Prelude
import Control.Monad.Reader (runReaderT)
import Data.Functor.Identity (runIdentity)

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Marvel.Ability.Types (Ability (..), AbilityType)
import Marvel.Ability.Types qualified as Ability
import Marvel.ActiveCost.Types
import Marvel.Payment ()
import Data.List qualified as L
import Marvel.Cost.Types (Cost (..), costResources, printedResourceRequirements)
import Marvel.Resource.Types (Resource (..))
import Marvel.Ally.Types (Ally)
import Marvel.Area
import Marvel.Ally.Types qualified as Ally
import Marvel.AlterEgo.Runner qualified as AlterEgo
import Marvel.AlterEgo.Types qualified as AlterEgo ()
import Marvel.Attachment.Types (Attachment, Attrs (..))
import Marvel.Obligation.Types (Attrs (..), getObligationDisplayThreat, getObligationDisplayCounters)
import Marvel.Attack qualified as Attack
import Marvel.Campaign qualified as Campaign
import Marvel.Campaign (Campaign (..), CampaignMode)
import Marvel.Card
import Marvel.Deck
import Marvel.Discard
import Marvel.Hand
import Marvel.EncounterSet qualified as EncounterSet
import Marvel.Entity
import Marvel.Game
import Marvel.Hero.Heroes.DoctorStrange (InvocationDeck (..), invocationDeckOf)
import Marvel.Hero.Runner qualified as Hero
import Marvel.Hero.Types qualified as HeroTypes ()
import Marvel.Environment.Types qualified as Environment
import Marvel.Hp
import Marvel.Id
import Marvel.Identity
import Marvel.MainScheme.Types (Attrs (..), MainScheme)
import Marvel.MainScheme.Types qualified as MainScheme
import Marvel.Message (FinishedStatus (..))
import Marvel.Message qualified as Msg
import Marvel.Modifier (Modifier (InfiniteHitPoints, CrisisIcon, CanLookAtTopOfEncounterDeck))
import Marvel.Minion.Types (Minion)
import Marvel.Minion.Types qualified as Minion
import Marvel.Name
import Marvel.Phase
import Marvel.Question
import Marvel.Ref
import Marvel.Scenario.Types (Attrs (..))
import Marvel.SideScheme.Types (Attrs (..), SideScheme)
import Marvel.Stats ()
import Marvel.Support.Types (Attrs (..), Support)
import Marvel.Upgrade.Types (Attrs (..), Upgrade)
import Marvel.Villain.Types (Villain)
import Marvel.Villain.Types qualified as Villain
import Marvel.Window.Types (ThreatSource)
import Marvel.Window.Types qualified as W

data GameView = GameView
  { round :: Int
  , phase :: Phase
  , outcome :: Maybe FinishedStatus
  , activePlayer :: IdentityId
  , playerOrder :: [IdentityId]
  -- ^ turn order this round, the first player first
  , seats :: [IdentityId]
  , players :: [PlayerView]
  -- ^ in seat order
  , villains :: [VillainView]
  , mainSchemes :: [SchemeView]
  , sideSchemes :: [SchemeView]
  , minions :: [MinionView]
  , uncontrolledAllies :: [AllyView]
  , environments :: [EnvironmentView]
  , futurePastDeckCount :: Maybe Int
  , encounterDeckSize :: Int
  , encounterDeckTop :: Maybe CardView
  , setAsideModulars :: [EncounterSet.EncounterSet]
  , encounterDiscard :: [CardView]
  , victoryDisplay :: [CardView]
  -- ^ the cards in the victory display, the first added first
  , victoryCondition :: Maybe Natural
  -- ^ how many of the villain's versions in the victory display win the game (Loki)
  , collection :: [CardView]
  -- ^ The Collection (Infiltrate the Museum): its player and encounter cards, faceup,
  -- the first added first
  , focusedCards :: [CardView]
  , attack :: Maybe AttackView
  -- ^ an enemy's attack, while it is made
  , threat :: Maybe ThreatView
  -- ^ threat that would be placed, while its window is open
  , boostCards :: [BoostView]
  -- ^ face down as dealt, then face up once flipped
  , accelerationTokens :: Natural
  , accelerationIcons :: Natural
  , question :: HashMap IdentityId QuestionView
  , questionVersion :: Int
  , activeCost :: Maybe ActiveCostView
  -- ^ what is being paid for, while it is
  , areas :: [AreaView]
  -- ^ the game areas, by the ids of what is in each; a game of one area has one
  , viewerArea :: Maybe AreaId
  -- ^ the area of the first seat whose hand the viewer sees
  , campaign :: Maybe CampaignInfo
  -- ^ the campaign the game is a scenario of
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

-- | Which campaign the game is a scenario of, and which of its scenarios.
data CampaignInfo = CampaignInfo
  { name :: Maybe Text
  , mode :: CampaignMode
  , scenario :: Int
  -- ^ its place, from 1
  , scenarios :: Int
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

-- | A game area: what is in it, by id, and the decks kept in it besides the encounter
-- deck.
data AreaView = AreaView
  { id :: AreaId
  , players :: [IdentityId]
  , villains :: [VillainId]
  , activeVillain :: Maybe VillainId
  , mainSchemes :: [Text]
  , sideSchemes :: [Text]
  , environments :: [Text]
  , decks :: [DeckView]
  , accelerationTokens :: Natural
  -- ^ the acceleration tokens next to its main scheme
  , accelerationIcons :: Natural
  -- ^ the acceleration icons in it
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

-- | A deck other than the encounter deck: a villain's own (@villain@, his name), a named
-- one (@named@, its name) or the side-scheme deck (@sideScheme@).
data DeckView = DeckView {kind :: Text, name :: Maybe Text, size :: Int, discard :: [CardView]}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data AttackView = AttackView {attacker :: Text, target :: Text}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data ThreatView = ThreatView {scheme :: Text, amount :: Natural, source :: ThreatSource, threshold :: Maybe Natural}
  -- ^ a main scheme's threat that advances it
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

-- | A boost card: its code and name only once it is flipped.
data BoostView = BoostView {id :: CardId, code :: Maybe CardCode, name :: Maybe Text}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data CardView = CardView {id :: CardId, code :: CardCode, name :: Text}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data PlayerView = PlayerView
  { id :: IdentityId
  , threat :: Natural
  , name :: Text
  , form :: Text
  -- ^ @hero@ or @alterEgo@
  , code :: CardCode
  -- ^ the face showing
  , otherCode :: CardCode
  , faces :: [CardCode]
  -- ^ every side of the identity card, A then B then C (Ant-Man's fold-out third)
  , hitPoints :: Natural
  , damage :: Natural
  , exhausted :: Bool
  , stunCards :: Natural
  , confuseCards :: Natural
  , stunned :: Bool
  , confused :: Bool
  , tough :: Bool
  , toughCards :: Natural
  , counters :: Natural
  -- ^ on the identity card: Groot's growth counters, Drax's vengeance
  , thwart :: Maybe Natural
  , attack :: Maybe Natural
  , defense :: Maybe Natural
  , recover :: Maybe Natural
  , handSize :: Natural
  -- ^ the hand size the player draws up to
  , handCount :: Int
  , hand :: Maybe [CardView]
  -- ^ only when the viewer may see it
  , deckSize :: Int
  , discard :: [CardView]
  , dealtEncounterCards :: Int
  , identityBoostCards :: Int
  , dealtCardIds :: [CardId]
  -- ^ the encounter cards dealt face down, for the board to move each from and to its pile
  , allies :: [AllyView]
  , allyLimit :: Natural
  , supports :: [SupportView]
  , upgrades :: [UpgradeView]
  -- ^ on the identity itself
  , engagedMinions :: [MinionId]
  , attachments :: [AttachmentView]
  -- ^ encounter cards attached to their hero
  , obligations :: [ObligationView]
  -- ^ the obligations in their play area
  , weatherDeckSize :: Maybe Int
  -- ^ Storm's separate deck stays facedown
  , invocation :: Maybe InvocationView
  -- ^ Doctor Strange's Invocation deck
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

-- | An Invocation deck: its top card, always faceup, its size, and its discard pile.
data InvocationView = InvocationView {top :: Maybe CardView, deckSize :: Int, discard :: [CardView]}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data AllyView = AllyView
  { id :: AllyId
  , threat :: Natural
  , code :: CardCode
  , name :: Text
  , hitPoints :: Natural
  , damage :: Natural
  , thwart :: Natural
  , attack :: Natural
  , exhausted :: Bool
  , stunCards :: Natural
  , confuseCards :: Natural
  , toughCards :: Natural
  , stunned :: Bool
  , confused :: Bool
  , tough :: Bool
  , counters :: Natural
  , upgrades :: [UpgradeView]
  , facedownCards :: Int
  , attachments :: [AttachmentView]
  -- ^ encounter cards attached to it (Restrained)
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data SupportView = SupportView {id :: SupportId, code :: CardCode, name :: Text, exhausted :: Bool, uses :: Natural, facedownCards :: Int, threat :: Natural, heldCards :: [CardView]}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data UpgradeView = UpgradeView {id :: UpgradeId, code :: CardCode, name :: Text, exhausted :: Bool, uses :: Natural, controller :: IdentityId, heldCards :: [CardView], facedown :: Bool, damage :: Natural, resourceSlots :: [Resource], occupiedSlots :: [Int], heldCardsFacedown :: Int}
  -- ^ the cards attached faceup to it, played from there (Hawkeye's Quiver); facedown,
  -- a Spectrum energy form she is not in (its owner's, not secret)
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data VillainView = VillainView
  { bombCounters :: Natural
  , counters :: Natural
  , threat :: Natural
  , id :: VillainId
  , code :: CardCode
  , name :: Text
  , stage :: Natural
  , hitPoints :: Int
  , infiniteHitPoints :: Bool
  -- ^ he has infinite hit points (Escape the Museum): shown as ∞, with no damage
  , damage :: Natural
  , scheme :: Natural
  , attack :: Natural
  , stunned :: Bool
  , confused :: Bool
  , tough :: Bool
  , stunCards :: Natural
  , confuseCards :: Natural
  , boostCards :: Int
  , attachments :: [AttachmentView]
  , upgrades :: [UpgradeView]
  , active :: Bool
  -- ^ the active villain of his area
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data MinionView = MinionView
  { bombCounters :: Natural
  , counters :: Natural
  , threat :: Natural
  , toughCards :: Natural
  , id :: MinionId
  , code :: CardCode
  , name :: Text
  , heldCards :: [CardView]
  , facedownCards :: Int
  , attachedToMinion :: Maybe MinionId
  , engagedWith :: IdentityId
  , hitPoints :: Natural
  , exhausted :: Bool
  , damage :: Natural
  , scheme :: Natural
  , attack :: Natural
  , stunned :: Bool
  , stunCards :: Natural
  , confuseCards :: Natural
  , confused :: Bool
  , tough :: Bool
  , boostCards :: Int
  , attachments :: [AttachmentView]
  , upgrades :: [UpgradeView]
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data EnvironmentView = EnvironmentView {id :: EnvironmentId, code :: CardCode, name :: Text, attachments :: [AttachmentView], counters :: Natural, owner :: Maybe IdentityId, damage :: Natural, facedownCards :: Natural}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data ObligationView = ObligationView {id :: ObligationId, code :: CardCode, name :: Text, facedownCards :: Int, threat :: Natural, counters :: Natural}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data AttachmentView = AttachmentView {id :: AttachmentId, code :: CardCode, name :: Text, damage :: Natural, counters :: Natural, crisis :: Bool}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data SchemeView = SchemeView
  { id :: Text
  , code :: CardCode
  , name :: Text
  , threat :: Natural
  , crisis :: Bool
  , upgrades :: [UpgradeView]
  -- ^ the player upgrades attached to it
  , counters :: Natural
  , attachments :: [AttachmentView]
  -- ^ the encounter cards attached to it
  , held :: Int
  -- ^ the cards facedown beneath it (Captured by Hydra's Captive ally, Hydra Prison's)
  , heldCards :: [HeldView]
  -- ^ which, and whose: for the client to fly a card put beneath it there
  , attachedCards :: [CardView]
  -- ^ the player cards attached to it faceup (Odin on Odin's Torment): a main scheme's
  -- held cards
  , accelerationTokens :: Natural
  , glider :: Bool
  , exhausted :: Bool
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

-- | A card facedown beneath a scheme: its id and its owner, not what it is.
data HeldView = HeldView {id :: CardId, owner :: Maybe IdentityId}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data QuestionView = QuestionView
  { kind :: Text
  -- ^ @chooseOne@, @chooseN@, @chooseOneAtATime@ or @choosePlayerOrder@
  , prompt :: Maybe Text
  -- ^ an i18n key under @$label.@, or text
  , source :: Maybe Text
  -- ^ the id of the card the question is about
  , assignedDamage :: HashMap Text Natural
  -- ^ Damage allocated so far, before the simultaneous damage resolves.
  , choices :: [ChoiceView]
  , reading :: [Text]
  -- ^ i18n keys of what to read before answering: a campaign scenario's story and rules
  , searched :: [SearchedView]
  -- ^ the cards searched, to show beside those that can be chosen
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

-- | Cards searched, and where they were (an i18n key) when it was one of several places.
data SearchedView = SearchedView {place :: Maybe Text, cards :: [CardView]}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

-- | One choice, as the client offers it: on a card when it is about one, as a button
-- otherwise.
-- | A card or ability being paid for: the card, or the card the ability is on, and the
-- resources paid so far.
data ActiveCostView = ActiveCostView
  { player :: IdentityId
  , card :: Maybe CardView
  , source :: Maybe Text
  , paid :: [Resource]
  , wilds :: [Int]
  -- ^ which of the paid resources are wild ones, declared as the type shown
  , owed :: [Maybe Resource]
  -- ^ what the paid resources do not yet cover: a resource of that kind, or any
  , anyAmount :: Bool
  -- ^ an X cost, paid as far as the player likes
  , spent :: [CardView]
  , canceledBy :: [Text]
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data ChoiceView = ChoiceView
  { index :: Int
  , kind :: Text
  , label :: Maybe Text
  -- ^ text or an i18n key under @$label.@
  , target :: Maybe Text
  -- ^ the id of the card or entity the choice is about
  , code :: Maybe CardCode
  , abilityType :: Maybe AbilityType
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

publicGame :: Game -> IO Value
publicGame = publicGameFor (const True)

-- | The game as someone sees it who may only look at the hands of the players given.
publicGameFor :: (IdentityId -> Bool) -> Game -> IO Value
publicGameFor seesHand g = do
  app <- newApp g []
  toJSON <$> runGameApp app (gameView seesHand g)

gameView :: (IdentityId -> Bool) -> Game -> GameAppT GameView
gameView seesHand g = do
  players <- traverse playerView [p | pid <- gameSeats g, Just p <- [HashMap.lookup pid (gamePlayers g)]]
  villains <- traverse villainView (HashMap.elems $ gameVillains g)
  schemes <- traverse sideSchemeView (HashMap.elems $ gameSideSchemes g)
  minions <- traverse minionView (HashMap.elems $ gameMinions g)
  uncontrolledAllies <- traverse allyView [a | a <- HashMap.elems (gameAllies g), toId a `elem` gameUncontrolledAllies g]
  mayLook <- anyM (fmap (elem CanLookAtTopOfEncounterDeck) . getModifiers . IdentityRef) [pid | pid <- gameSeats g, seesHand pid]
  threat <- for (headMay [(src, sid, n) | W.Window W.Would (W.ThreatPlaced src sid n) <- concat (headMay (gameWindows g))]) \(src, sid, n) -> do
    threshold <- case sid of
      SchemeMainSchemeId mid | Just s <- HashMap.lookup mid (gameMainSchemes g) -> Just <$> MainScheme.getThreshold (toAttrs s)
      _ -> pure Nothing
    pure $ ThreatView (schemeIdText sid) n src threshold
  activeCost <- for (headMay $ gameActiveCost g) \c -> do
    paid <- paymentResources c.activeCostPayment
    pure
      ActiveCostView
        { player = c.activeCostIdentityId
        , card = case c.activeCostTarget of
            ForCard pc -> Just (cardView (PlayerCard pc))
            _ -> Nothing
        , source = case c.activeCostTarget of
            ForAbility a -> refId a.abilitySource
            ForContinuation source _ -> refId source
            _ -> Nothing
        , paid
        , wilds = [i | (i, True) <- zip [0 ..] (concatMap wildOrNot (payments c.activeCostPayment))]
        , owed = let required = printedResourceRequirements c.activeCostCost
                     receipts = [rs | PrintedResourcesPayment rs <- payments c.activeCostPayment]
                     printed = if null receipts then concatMap (printedResources . getCardDef) c.activeCostSpentCards else concat receipts
                  in if null required then owedFor (costResources c.activeCostCost) paid
                     else map Just (foldl' (flip L.delete) required printed)
        , anyAmount = dynamic c.activeCostCost
        , spent = map (cardView . PlayerCard) c.activeCostSpentCards
        , canceledBy = case c.activeCostTarget of
            ForCard pc | cdCardType (getCardDef pc) == EventType ->
              let labels = cdAbilitySubTypes (getCardDef pc)
                  identity = HashMap.lookup c.activeCostIdentityId (gamePlayers g)
               in ["stunned" | Ability.Attack `elem` labels, maybe False (runIdentity . flip runReaderT g . identityIsStunned) identity]
                    <> ["confused" | Ability.Thwart `elem` labels, maybe False (runIdentity . flip runReaderT g . identityIsConfused) identity]
            _ -> []
        }
  pure
    GameView
      { round = gameRound g
      , phase = gamePhase g
      , outcome = case gameState g of
          Finished s -> Just s
          _ -> Nothing
      , activePlayer = gameActivePlayer g
      , playerOrder = gamePlayerOrder g
      , seats = gameSeats g
      , players
      , villains
      , mainSchemes = map mainSchemeView (HashMap.elems $ gameMainSchemes g)
      , sideSchemes = schemes
      , minions
      , uncontrolledAllies
      , environments =
          [ EnvironmentView a.environmentId (toCardCode a.environmentCardDef) (display $ cdName a.environmentCardDef) (attachmentsIn a.environmentAttachments) a.environmentCounters a.environmentOwner a.environmentDamage (fromIntegral $ length a.environmentHeldCards)
          | e <- HashMap.elems (gameEnvironments g)
          , let a = toAttrs e
          ]
      , futurePastDeckCount = case gameCampaign g of
          Just c | c.kind == Campaign.MutantGenesis -> Just $ length c.campaignLog.mutantGenesis.futurePastDeck
          _ -> Nothing
      , encounterDeckSize = length scenario.scenarioEncounterDeck
      , encounterDeckTop = if mayLook then cardView . EncounterCard <$> headMay scenario.scenarioEncounterDeck else Nothing
      , setAsideModulars = L.nub [set | c <- scenario.scenarioSetAsideCards, Just set <- [cdEncounterSet $ getCardDef c], set `elem` EncounterSet.modularSets]
      , encounterDiscard = map (cardView . EncounterCard) scenario.scenarioDiscard
      , victoryDisplay = map (cardView . EncounterCard) (gameVictoryDisplay g) <> map (cardView . PlayerCard) (gamePlayerVictoryDisplay g)
      , victoryCondition = scenario.scenarioVictoryCondition
      , collection = map cardView (gameCollection g)
      , focusedCards = map cardView (gameFocusedCards g <> concat [cs | (pid, cs) <- HashMap.toList (gamePrivateCards g), seesHand pid])
      , attack = headMay $ mapMaybe attackView (map Villain.villainAttackDetails (HashMap.elems (gameVillains g)) <> map Minion.minionAttackDetails (HashMap.elems (gameMinions g)))
      , threat
      , boostCards =
          [BoostView (ecCardId c) Nothing Nothing | v <- HashMap.elems (gameVillains g), c <- reverse (toAttrs v).villainBoostCards]
            <> [BoostView (ecCardId c) Nothing Nothing | m <- HashMap.elems (gameMinions g), c <- (toAttrs m).minionBoostCards]
            <> [BoostView c.id (Just c.code) (Just c.name) | c <- map (cardView . EncounterCard) (HashMap.elems (gameBoostCards g))]
      , accelerationTokens = sum (map areaAccelerationTokens (gameAreas g)) + sum [(toAttrs scheme).mainSchemeAccelerationTokens | scheme <- HashMap.elems (gameMainSchemes g)]
      , accelerationIcons = sum (map (accelerationIconsIn g . areaId) (gameAreas g))
      , question = HashMap.mapWithKey questionView (gameQuestion g)
      , questionVersion = gameQuestionVersion g
      , activeCost
      , areas = map areaView (gameAreas g)
      , viewerArea = headMay [aid | pid <- gameSeats g, seesHand pid, Just aid <- [areaId <$> areaOfTarget g (IdentityRef pid)]]
      , campaign =
          gameCampaign g <&> \c ->
            CampaignInfo
              { name = c.campaignName
              , mode = c.mode
              , scenario = maybe (c.completed + 1) (+ 1) (L.elemIndex (toId $ gameScenario g) c.scenarios)
              , scenarios = length c.scenarios
              }
      }
 where
  scenario = toAttrs (gameScenario g)
  villainName v = display . cdName . getCardDef <$> (HashMap.lookup v (gameVillains g) <|> HashMap.lookup v (g ^. removedEntitiesL . villainsL))
  areaView a =
    let aid = areaId a
        here t = (areaId <$> areaOfTarget g t) == Just aid
        villainsHere = villainsIn g aid
        pileView key = maybe [] pure (HashMap.lookup key scenario.scenarioDecks) <&> \(Pile d x) ->
          (length d, map (cardView . EncounterCard) x)
        deckView kind name key = [DeckView kind name size discard | (size, discard) <- pileView key]
        first = Just aid == fmap areaId (headMay (gameAreas g))
        defeatedHere v = maybe first ((== aid) . areaId) (L.find (elem v . areaVillains) (gameAreas g))
     in AreaView
          { id = aid
          , players = [pid | pid <- gameSeats g, here (IdentityRef pid)]
          , villains = villainsHere
          , activeVillain = activeVillainIn g aid
          , mainSchemes = [idText k | k <- HashMap.keys (gameMainSchemes g), here (MainSchemeRef k)]
          , sideSchemes = [idText k | k <- HashMap.keys (gameSideSchemes g), here (SideSchemeRef k)]
          -- an environment is in every area (RR FAQ, The Once and Future Kang)
          , environments = map idText (HashMap.keys (gameEnvironments g))
          , decks =
              concat [deckView "villain" (villainName v) (VillainDeckKey v) | v <- villainsHere]
                -- a defeated villain's deck and discard pile, kept for his cards still in
                -- play, in the area he was in (the first, if none lists him)
                <> concat [deckView "villain" (villainName v) k | k@(VillainDeckKey v) <- HashMap.keys scenario.scenarioDecks, not (HashMap.member v (gameVillains g)), defeatedHere v]
                -- a named deck is the scenario's, by the first area's main scheme
                <> concat [deckView "named" (Just n) k | first, k@(NamedDeckKey n) <- HashMap.keys scenario.scenarioDecks]
                <> deckView "sideScheme" Nothing SideSchemeDeckKey
                -- the Infinity Gauntlet's stones, beside the villain (The Mad Titan's Shadow)
                <> concat [deckView "infinityStones" Nothing InfinityStoneDeckKey | first]
          , accelerationTokens = areaAccelerationTokens a + sum [(toAttrs scheme).mainSchemeAccelerationTokens | (sid, scheme) <- HashMap.toList (gameMainSchemes g), here (MainSchemeRef sid)]
          , accelerationIcons = accelerationIconsIn g aid
          }
  upgradesOn p = [upgradeView u | u <- HashMap.elems (gameUpgrades g), p (toAttrs u)]
  attachmentsIn ids = [attachmentView a | a <- HashMap.elems (gameAttachments g), toId a `HashSet.member` ids]
  playerView p = do
    let a = toAttrs p
        handCards = unHand a.playerIdentityHand
        (form, other) = case currentIdentity p of
          HeroSide _ -> ("hero", B)
          AlterEgoSide _ -> ("alterEgo", A)
        otherCode = maybe (toCardCode p) sideCode (HashMap.lookup other a.playerIdentitySides)
    hp <- getModifiedHp p
    hs <- getModifiedHandSize p
    allyLimit <- getModifiedAllyLimit p
    (thw, atk, def, rec) <- case currentIdentity p of
      HeroSide h -> do
        let ha = toAttrs h
        (,,,) <$> (Just <$> Hero.getModifiedThwart ha) <*> (Just <$> Hero.getModifiedAttack ha) <*> (Just <$> Hero.getModifiedDefense ha) <*> pure Nothing
      AlterEgoSide ae -> (Nothing,Nothing,Nothing,) . Just <$> AlterEgo.getModifiedRecovery (toAttrs ae)
    stunnedNow <- identityIsStunned p
    confusedNow <- identityIsConfused p
    allies <- traverse allyView [ally | ally <- HashMap.elems (gameAllies g), toId ally `HashSet.member` a.playerIdentityAllies]
    pure
      PlayerView
        { id = toId p
        , name = a.playerIdentityName
        , form
        , code = toCardCode p
        , otherCode
        , faces = map sideCode [x | k <- [A, B, C], Just x <- [HashMap.lookup k a.playerIdentitySides]]
        , hitPoints = hp
        , damage = a.playerIdentityDamage
        , exhausted = a.playerIdentityExhausted
        , stunCards = (if a.playerIdentityStunned then 1 else 0) + (if a.playerIdentityAdditionalStun then 1 else 0)
        , confuseCards = (if a.playerIdentityConfused then 1 else 0) + (if a.playerIdentityAdditionalConfuse then 1 else 0)
        , stunned = stunnedNow
        , confused = confusedNow
        , toughCards = (if a.playerIdentityTough then 1 else 0) + a.playerIdentityAdditionalTough
        , tough = a.playerIdentityTough
        , counters = a.playerIdentityCounters
        , threat = sum [n | (IdentityCharacter who,n) <- gameCharacterThreat g, who == a.playerIdentityId]
        , thwart = thw
        , attack = atk
        , defense = def
        , recover = rec
        , handSize = hs
        , handCount = length handCards
        , hand = guard (seesHand (toId p)) $> map (cardView . PlayerCard) handCards
        , deckSize = length (unDeck a.playerIdentityDeck)
        , discard = map (cardView . PlayerCard) (unDiscard a.playerIdentityDiscard)
        , dealtEncounterCards = length a.playerIdentityEncounterCards
        , identityBoostCards = length a.playerIdentityBoostCards
        , dealtCardIds = map ecCardId a.playerIdentityEncounterCards
        , allies
        , allyLimit
        , supports = [supportView s | s <- HashMap.elems (gameSupports g), toId s `HashSet.member` a.playerIdentitySupports]
        , upgrades = upgradesOn \u -> (u.upgradeAttachedIdentity == Just (toId p) || (u.upgradeController == toId p && isNothing u.upgradeAttachedIdentity)) && isNothing u.upgradeAttachedAlly && isNothing u.upgradeAttachedEnemy && isNothing u.upgradeAttachedSideScheme && isNothing u.upgradeAttachedMainScheme
        , engagedMinions = HashSet.toList a.playerIdentityMinions
        , attachments = [attachmentView x | x <- HashMap.elems (gameAttachments g), (toAttrs x).attachmentIdentity == Just (toId p)]
        , obligations =
            [ ObligationView a.obligationId (toCardCode a.obligationCardDef) (display $ cdName a.obligationCardDef) (length a.obligationHeldCards) (getObligationDisplayThreat o) (getObligationDisplayCounters o)
            | o <- HashMap.elems (gameObligations g), let a = toAttrs o
            , a.obligationOwner == Just (toId p), isNothing a.obligationSideScheme
            ]
        , weatherDeckSize = length <$> a.playerIdentityWeatherDeck
        , invocation =
            invocationDeckOf p <&> \d ->
              InvocationView
                (cardView . PlayerCard <$> headMay d.invocationDeck)
                (length d.invocationDeck)
                (map (cardView . PlayerCard) d.invocationDiscard)
        }
  allyView (ally :: Ally) = do
    let a = toAttrs ally
    thw <- Ally.getModifiedThwart a
    atk <- Ally.getModifiedAttack a
    hp <- Ally.getModifiedHitPoints a
    pure
      AllyView
        { id = a.allyId
        , code = toCardCode a.allyCardDef
        , name = display (cdName a.allyCardDef)
        , hitPoints = hp
        , damage = a.allyDamage
        , thwart = thw
        , attack = atk
        , exhausted = a.allyExhausted
        , stunCards = (if a.allyStunned then 1 else 0) + (if a.allyAdditionalStun then 1 else 0)
        , confuseCards = (if a.allyConfused then 1 else 0) + (if a.allyAdditionalConfuse then 1 else 0)
        , toughCards = (if a.allyTough then 1 else 0) + a.allyAdditionalTough
        , stunned = a.allyStunned
        , confused = a.allyConfused
        , tough = a.allyTough
        , counters = a.allyCounters
        , threat = sum [n | (AllyCharacter who,n) <- gameCharacterThreat g, who == a.allyId]
        , facedownCards = length a.allyHeldCards + length a.allyHeldTreacheries
        , upgrades = upgradesOn \u -> u.upgradeAttachedAlly == Just a.allyId
        , attachments = [attachmentView x | x <- HashMap.elems (gameAttachments g), (toAttrs x).attachmentAlly == Just a.allyId]
        }
  supportView (s :: Support) =
    let a = toAttrs s
        faceup = toCardCode a.supportCardDef == "38028"
     in SupportView a.supportId (toCardCode a.supportCardDef) (display $ cdName a.supportCardDef) a.supportExhausted a.supportUses (if faceup then 0 else length a.supportHeldCards) a.supportThreat [cardView (PlayerCard card) | faceup || seesHand a.supportController, card <- a.supportHeldCards]
  upgradeView (u :: Upgrade) =
    let a = toAttrs u
     in UpgradeView a.upgradeId (toCardCode a.upgradeCardDef) (display $ cdName a.upgradeCardDef) a.upgradeExhausted (if toCardCode a == "34007" then 0 else a.upgradeUses) a.upgradeController [cardView (PlayerCard card) | not a.upgradeHeldCardsFacedown || seesHand a.upgradeController, card <- a.upgradeHeldCards] a.upgradeFaceDown (if toCardCode a == "34007" then a.upgradeUses else 0) a.upgradeResourceSlots a.upgradeOccupiedSlots (if a.upgradeHeldCardsFacedown then length a.upgradeHeldCards else 0)
  attachmentView (x :: Attachment) =
    let a = toAttrs x
        crisisHere = runIdentity $ runReaderT (elem CrisisIcon <$> getModifiers (AttachmentRef a.attachmentId)) g
     in AttachmentView a.attachmentId (toCardCode a.attachmentCardDef) (display $ cdName a.attachmentCardDef) a.attachmentDamage a.attachmentCounters crisisHere
  villainView (v :: Villain) = do
    let a = toAttrs v
    sch <- Villain.getModifiedScheme a
    atk <- Villain.getModifiedAttack a
    infinite <- elem InfiniteHitPoints <$> getModifiers a
    pure
      VillainView
        { counters = a.villainCounters
        , threat = sum [n | (VillainCharacter who,n) <- gameCharacterThreat g, who == a.villainId]
        , bombCounters = sum [n | (EnemyVillainId vid,_,n) <- gameBombCounters g, vid == toId v]
        , id = a.villainId
        , code = toCardCode a.villainCardDef
        , name = display (cdName a.villainCardDef)
        , stage = a.villainStage
        , hitPoints = unHp a.villainMaxHp
        , infiniteHitPoints = infinite
        , damage = if infinite then 0 else Villain.villainDamage v
        , scheme = sch
        , attack = maybe atk Attack.attackValue (Villain.villainAttackDetails v)
        , stunned = a.villainStunned
        , confused = a.villainConfused
        , tough = a.villainTough
        , stunCards = (if a.villainStunned then 1 else 0) + (if a.villainAdditionalStun then 1 else 0)
        , confuseCards = (if a.villainConfused then 1 else 0) + (if a.villainAdditionalConfuse then 1 else 0)
        , boostCards = length a.villainBoostCards
        , attachments = attachmentsIn a.villainAttachments
        , upgrades = upgradesOn \u -> u.upgradeAttachedEnemy == Just (EnemyVillainId a.villainId)
        , active = any (\area -> activeVillainIn g (areaId area) == Just a.villainId) (gameAreas g)
        }
  minionView (m :: Minion) = do
    let a = toAttrs m
        hiddenHeldCards = toCardCode a `elem` ["39052", "39058"]
    hp <- Minion.getModifiedHitPoints a
    atk <- Minion.getModifiedAttack a
    sch <- Minion.getModifiedScheme a
    pure
      MinionView
        { counters = Minion.getMinionDisplayCounters m
        , threat = sum [n | (MinionCharacter who,n) <- gameCharacterThreat g, who == a.minionId]
        , bombCounters = sum [n | (EnemyMinionId mid,_,n) <- gameBombCounters g, mid == a.minionId]
        , id = a.minionId
        , code = toCardCode a.minionCardDef
        , name = display (cdName a.minionCardDef)
        , heldCards = [cardView (PlayerCard card) | not hiddenHeldCards, card <- a.minionHeldCards]
        , facedownCards = length a.minionHeldTreacheries + if hiddenHeldCards then length a.minionHeldCards else 0
        , attachedToMinion = a.minionAttachedTo
        , engagedWith = fromMaybe a.minionEngagedIdentity $ do
            host <- a.minionAttachedTo
            Minion.minionEngagedIdentity . toAttrs <$> HashMap.lookup host (gameMinions g)
        , hitPoints = hp
        , exhausted = a.minionExhausted
        , damage = a.minionDamage
        , scheme = sch
        , attack = maybe atk Attack.attackValue (Minion.minionAttackDetails m)
        , stunCards = (if a.minionStunned then 1 else 0) + (if a.minionAdditionalStun then 1 else 0)
        , confuseCards = (if a.minionConfused then 1 else 0) + (if a.minionAdditionalConfuse then 1 else 0)
        , stunned = a.minionStunned
        , confused = a.minionConfused
        , tough = a.minionTough
        , toughCards = (if a.minionTough then 1 else 0) + a.minionAdditionalTough
        , boostCards = length a.minionBoostCards
        , attachments = attachmentsIn a.minionAttachments
        , upgrades = upgradesOn \u -> u.upgradeAttachedEnemy == Just (EnemyMinionId a.minionId)
        }
  mainSchemeView (s :: MainScheme) =
    let a = toAttrs s
        code = toCardCode a.mainSchemeCardDef
        awaitingSetup = any (\case
          QuestionLabel "$label.setupMainScheme" (Just target) _ -> target == toTarget a.mainSchemeId
          _ -> False) (HashMap.elems $ gameQuestion g)
        face = if awaitingSetup then CardCode (unCardCode code <> "a") else code
        tokensHere = a.mainSchemeAccelerationTokens + maybe 0 areaAccelerationTokens (areaOfTarget g $ MainSchemeRef a.mainSchemeId)
     in SchemeView (idText a.mainSchemeId) face (display $ cdName a.mainSchemeCardDef) a.mainSchemeThreat a.mainSchemeCrisis (upgradesOn \u -> u.upgradeAttachedMainScheme == Just a.mainSchemeId) a.mainSchemeCounters [attachmentView x | x <- HashMap.elems (gameAttachments g), (toAttrs x).attachmentMainScheme == Just a.mainSchemeId] 0 [] (map (cardView . PlayerCard) a.mainSchemeHeldCards) tokensHere a.mainSchemeHasGlider False
  sideSchemeView (s :: SideScheme) = do
    modifiers <- getModifiers s
    let a = toAttrs s
    pure $ SchemeView (idText a.sideSchemeId) (toCardCode a.sideSchemeCardDef) (display $ cdName a.sideSchemeCardDef) a.sideSchemeThreat ((a.sideSchemeCrisis || CrisisIcon `elem` modifiers) && not (blankIn g $ SideSchemeRef a.sideSchemeId)) (upgradesOn \u -> u.upgradeAttachedSideScheme == Just a.sideSchemeId) a.sideSchemeCounters [attachmentView x | x <- HashMap.elems (gameAttachments g), (toAttrs x).attachmentSideScheme == Just a.sideSchemeId] (length a.sideSchemeHeldCards) (map heldView a.sideSchemeHeldCards) [cardView (toCard o) | o <- HashMap.elems (gameObligations g), (toAttrs o).obligationSideScheme == Just a.sideSchemeId] a.sideSchemeAccelerationTokens False a.sideSchemeExhausted

heldView :: PlayerCard -> HeldView
heldView c = HeldView (pcCardId c) (pcOwner c)

sideCode :: PlayerIdentitySide -> CardCode
sideCode = \case
  HeroSide x -> toCardCode x
  AlterEgoSide x -> toCardCode x

cardView :: Card -> CardView
cardView c = CardView (toCardId c) (toCardCode c) (display . cdName $ getCardDef c)

-- | The question as asked of a player.
questionView :: IdentityId -> Question -> QuestionView
questionView asked = \case
  QuestionLabel l t q ->
    let v = questionView asked q
        -- the choice on the card asked about says what is asked: "When Revealed"
        named c
          -- A saved setup prompt belongs on its scheme too, including prompts saved
          -- before setup choices targeted the card.
          | l == "$label.setupMainScheme", isJust t = c {kind = "target", target = refId =<< t, label = Just l}
          | c.kind == "target" && isJust c.target && c.target == (refId =<< t) && isNothing c.label = c {label = Just l}
          | otherwise = c
     in v {prompt = Just l, source = refId =<< t, choices = map named v.choices}
  Read r q -> (questionView asked q) {reading = r}
  Searched cs q -> (questionView asked q) {searched = [SearchedView l (map cardView xs) | (l, xs) <- cs]}
  ChooseOne cs -> QuestionView "chooseOne" Nothing Nothing (allocations cs) (described cs) [] []
  ChooseN _ cs -> QuestionView "chooseN" Nothing Nothing (allocations cs) (described cs) [] []
  ChooseOneAtATime cs -> QuestionView "chooseOneAtATime" Nothing Nothing (allocations cs) (described cs) [] []
  ChoosePlayerOrder (Unsorted xs) _ ->
    QuestionView "choosePlayerOrder" Nothing Nothing mempty [ChoiceView n "player" Nothing (Just $ idText x) Nothing Nothing | (n, x) <- zip [0 ..] xs] [] []
 where
  described cs = [(describeChoice asked c) {index = n} | (n, c) <- zip [0 ..] cs]
  -- Every allocation choice carries the continuation with its new point prepended.
  -- Remove that point to describe the current state, including targets now full.
  allocations cs = fromMaybe mempty $ headMay $ concatMap allocationChoice cs
  allocationChoice = \case
    TargetLabel _ body -> concatMap allocationChoice body
    Label _ body -> concatMap allocationChoice body
    Run msgs -> concatMap allocationMessage msgs
    _ -> []
  allocationMessage = \case
    Msg.AssignIndirectDamage _ _ _ shares -> [totals [(CharacterRef c, n) | (c, n) <- drop 1 shares]]
    Msg.AssignIndirectDamageAmongPlayers _ _ shares -> [totals [(CharacterRef c, n) | (c, n) <- drop 1 shares]]
    Msg.AssignDividedDamage _ _ _ _ shares -> [totals [(EnemyRef e, n) | (e, n) <- drop 1 shares]]
    _ -> []
  totals shares = HashMap.fromListWith (+) [(k, n) | (r, n) <- shares, Just k <- [refId r]]

-- | What a choice is about, for the client to put it on the card it concerns: the
-- player asked defends with their own identity.
describeChoice :: IdentityId -> Choice -> ChoiceView
describeChoice asked = \case
  -- a card chosen for what choosing it does: the card itself is all there is to show
  CardLabel code (Run _) -> withCode (Just code) (plain "card" Nothing)
  CardLabel code c -> withCode (Just code) (describeChoice asked c)
  Label l _ -> plain "label" (Just l)
  TargetLabel t body -> withKind (fromMaybe "target" (doneTo t body)) (on "target" t)
  EndTurn -> plain "endTurn" (Just "$label.endTurn")
  UseAbility a ->
    (on "ability" (refocus a.abilitySource))
      { label = a.abilityLabel <|> basicLabel a.abilityChoices
      , abilityType = Just a.abilityType
      }
  RunAbility t _ -> on "ability" t
  ChangeForm -> plain "changeForm" (Just "$label.changeForm")
  ChangeToForm side -> plain "changeForm" (Just $ "$label.changeTo" <> tshow side)
  PlayCard c _ -> withCode (Just $ toCardCode c) (onCard "play" $ toCardId c)
  PayWithCard c -> withCode (Just $ toCardCode c) (onCard "pay" $ toCardId c)
  FinishPayment -> plain "finishPayment" (Just "$label.finishPayment")
  -- the nth wild paid: the client puts it on that wild
  DeclareWild n r -> withTarget (Just $ tshow n) (plain "declareWild" (Just $ tshow r))
  ReadyIdentity -> plain "label" (Just "$label.readyIdentity")
  Recover -> plain "recover" (Just "$label.recover")
  Attack -> plain "attack" (Just "$label.attack")
  Thwart -> plain "thwart" (Just "$label.thwart")
  Defend _ -> on "defend" (IdentityRef asked)
  AllyAttack a -> on "allyAttack" (AllyRef a)
  AllyThwart a -> on "allyThwart" (AllyRef a)
  AllyDefend a _ -> on "allyDefend" (AllyRef a)
  DamageEnemy t _ _ -> on "damage" t
  ThwartScheme t _ _ -> on "thwartScheme" t
  BasicThwartScheme t _ _ -> on "thwartScheme" t
  Stun t _ -> on "stun" t
  Confuse t _ -> on "confuse" t
  Heal c _ -> on "heal" (CharacterRef c)
  DamageCharacter c _ _ -> on "damage" (CharacterRef c)
  DiscardTarget t -> on "discard" t
  DiscardCard c -> onCard "discard" (toCardId c)
  ReturnTargetToHand t -> on "returnToHand" t
  other -> plain "other" (Just (tshow other))
 where
  withCode value (ChoiceView i k l t _ a) = ChoiceView i k l t value a
  withKind value (ChoiceView i _ l t c a) = ChoiceView i value l t c a
  withTarget value (ChoiceView i k l _ c a) = ChoiceView i k l value c a
  -- the powers every identity and ally has are named by what they do
  basicLabel = \case
    (ChangeForm : _) -> Just "$label.changeForm"
    (Attack : _) -> Just "$label.attack"
    (Thwart : _) -> Just "$label.thwart"
    (Recover : _) -> Just "$label.recover"
    (AllyAttack _ : _) -> Just "$label.attack"
    (AllyThwart _ : _) -> Just "$label.thwart"
    _ -> Nothing
  -- a choice built from what it does is named by the first thing it does to its target,
  -- as the choice that says so would be: "damage" on the character it damages
  doneTo t = \case
    [Run msgs] -> headMay [k | (k, on') <- mapMaybe doneBy (concatMap flat msgs), refId on' == refId t]
    _ -> Nothing
  flat = \case
    Msg.Simultaneously msgs -> concatMap flat msgs
    m -> [m]
  doneBy :: Msg.Message -> Maybe (Text, Target)
  doneBy = \case
    Msg.IdentityMessage i (Msg.IdentityDamaged _ _) -> Just ("damage", IdentityRef i)
    Msg.AllyMessage a (Msg.AllyDamaged _ _) -> Just ("damage", AllyRef a)
    Msg.VillainMessage v (Msg.VillainDamaged _ _) -> Just ("damage", VillainRef v)
    Msg.MinionMessage m (Msg.MinionDamaged _ _) -> Just ("damage", MinionRef m)
    Msg.IdentityMessage i (Msg.IdentityHealed _) -> Just ("heal", IdentityRef i)
    Msg.AllyMessage a (Msg.AllyHealed _) -> Just ("heal", AllyRef a)
    Msg.VillainMessage v (Msg.VillainStunned _) -> Just ("stun", VillainRef v)
    Msg.MinionMessage m (Msg.MinionStunned _) -> Just ("stun", MinionRef m)
    Msg.VillainMessage v (Msg.VillainConfused _) -> Just ("confuse", VillainRef v)
    Msg.MinionMessage m (Msg.MinionConfused _) -> Just ("confuse", MinionRef m)
    Msg.MainSchemeMessage m (Msg.MainSchemeBasicThwarted _ _) -> Just ("thwartScheme", MainSchemeRef m)
    Msg.MainSchemeMessage m (Msg.MainSchemeThwarted _ _) -> Just ("thwartScheme", MainSchemeRef m)
    Msg.MainSchemeMessage m (Msg.MainSchemeThreatRemoved _ _) -> Just ("thwartScheme", MainSchemeRef m)
    Msg.SideSchemeMessage m (Msg.SideSchemeThwarted _ _) -> Just ("thwartScheme", SideSchemeRef m)
    Msg.SideSchemeMessage m (Msg.SideSchemeThreatRemoved _ _) -> Just ("thwartScheme", SideSchemeRef m)
    Msg.RemoveFromPlay t' -> Just ("discard", t')
    Msg.ReturnToHand t' -> Just ("returnToHand", t')
    _ -> Nothing
  plain kind label = ChoiceView 0 kind label Nothing Nothing Nothing
  on kind t = ChoiceView 0 kind Nothing (refId t) Nothing Nothing
  onCard kind cid = ChoiceView 0 kind Nothing (Just $ idText cid) Nothing Nothing

-- | The resources a cost still needs once those paid are matched to it: each kind first,
-- a wild in place of one, then what is left over against the generic ones.
owedFor :: [Maybe Resource] -> [Resource] -> [Maybe Resource]
owedFor cost paid = map Just unmet <> replicate (length generic - length left) Nothing
 where
  (specific, generic) = (catMaybes cost, filter isNothing cost)
  (unmet, left) = foldl' match ([], paid) specific
  match (missed, have) r
    | r `elem` have = (missed, L.delete r have)
    | Wild `elem` have = (missed, L.delete Wild have)
    | otherwise = (missed <> [r], have)

-- | The resources a payment is of, each whether it is a wild one.
wildOrNot :: Payment -> [Bool]
wildOrNot = \case
  ResourcePayment _ -> [False]
  WildPayment _ -> [True]
  ContributedWildPayment _ _ -> [True]
  _ -> []

dynamic :: Cost -> Bool
dynamic = \case
  DynamicResourceCost _ -> True
  UpToResourcesCost _ -> True
  Costs cs -> any dynamic cs
  _ -> False

schemeIdText :: SchemeId -> Text
schemeIdText = \case
  SchemeMainSchemeId x -> idText x
  SchemeSideSchemeId x -> idText x

attackView :: Maybe Attack.Attack -> Maybe AttackView
attackView = fmap \a ->
  AttackView
    { attacker = case Attack.attackEnemy a of
        EnemyVillainId x -> idText x
        EnemyMinionId x -> idText x
    , target = case Attack.attackCharacter a of
        IdentityCharacter x -> idText x
        AllyCharacter x -> idText x
        VillainCharacter x -> idText x
        MinionCharacter x -> idText x
    }

-- | The id the client keys a card or entity by: the uuid inside the ref.
refId :: Ref focus -> Maybe Text
refId = \case
  IdentityRef x -> Just (idText x)
  VillainRef x -> Just (idText x)
  MinionRef x -> Just (idText x)
  EnemyRef (EnemyVillainId x) -> Just (idText x)
  EnemyRef (EnemyMinionId x) -> Just (idText x)
  AllyRef x -> Just (idText x)
  CharacterRef c -> Just case c of
    IdentityCharacter x -> idText x
    AllyCharacter x -> idText x
    VillainCharacter x -> idText x
    MinionCharacter x -> idText x
  SupportRef x -> Just (idText x)
  EventRef x -> Just (idText x)
  EffectRef x -> Just (idText x)
  TreacheryRef x -> Just (idText x)
  ObligationRef x -> Just (idText x)
  UpgradeRef x -> Just (idText x)
  SideSchemeRef x -> Just (idText x)
  AttachmentRef x -> Just (idText x)
  MainSchemeRef x -> Just (idText x)
  SchemeRef (SchemeMainSchemeId x) -> Just (idText x)
  SchemeRef (SchemeSideSchemeId x) -> Just (idText x)
  CardIdRef x -> Just (idText x)
  ActiveCostRef _ -> Nothing
  GameRef -> Nothing
  EnvironmentRef x -> Just (idText x)

idText :: (ToJSON a) => a -> Text
idText a = case toJSON a of
  String t -> t
  other -> tshow other

