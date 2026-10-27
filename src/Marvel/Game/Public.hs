{-# LANGUAGE DuplicateRecordFields #-}

-- | The game as the client draws it: one record per thing on the table, each card by its
-- id and code with its stats as they now stand, and each choice of the open questions
-- described by what it is about. Decks are hidden information, so only their sizes go
-- out, and at a game with friends so do the hands of the players a viewer does not hold.
--
-- Stored games and exports keep the engine's own JSON; this is only ever sent.
module Marvel.Game.Public (publicGame, publicGameFor, owedFor, describeChoice, GameView (..), ChoiceView (..)) where

import Marvel.Prelude

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Marvel.Ability.Types (Ability (..), AbilityType)
import Marvel.ActiveCost.Types
import Marvel.Payment (Payment (..), paymentResources, payments)
import Data.List qualified as L
import Marvel.Cost.Types (Cost (..), costResources)
import Marvel.Resource.Types (Resource (..))
import Marvel.Ally.Types (Ally)
import Marvel.Area
import Marvel.Ally.Types qualified as Ally
import Marvel.AlterEgo.Runner qualified as AlterEgo
import Marvel.AlterEgo.Types qualified as AlterEgo
import Marvel.Attachment.Types (Attachment, Attrs (..))
import Marvel.Obligation.Types (Attrs (..))
import Marvel.Attack qualified as Attack
import Marvel.Campaign (Campaign (..), CampaignMode)
import Marvel.Card
import Marvel.Deck
import Marvel.Discard
import Marvel.Hand
import Marvel.Entity
import Marvel.Game
import Marvel.Hero.Heroes.DoctorStrange (InvocationDeck (..), invocationDeckOf)
import Marvel.Hero.Runner qualified as Hero
import Marvel.Hero.Types qualified as HeroTypes
import Marvel.Environment.Types qualified as Environment
import Marvel.Hp
import Marvel.Id
import Marvel.Identity
import Marvel.MainScheme.Types (Attrs (..), MainScheme)
import Marvel.MainScheme.Types qualified as MainScheme
import Marvel.Message (FinishedStatus (..))
import Marvel.Message qualified as Msg
import Marvel.Minion.Types (Minion)
import Marvel.Minion.Types qualified as Minion
import Marvel.Name
import Marvel.Phase
import Marvel.Question
import Marvel.Ref
import Marvel.Scenario.Types (Attrs (..))
import Marvel.SideScheme.Types (Attrs (..), SideScheme)
import Marvel.Stats
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
  , environments :: [EnvironmentView]
  , encounterDeckSize :: Int
  , encounterDiscard :: [CardView]
  , victoryDisplay :: [CardView]
  -- ^ the cards in the victory display, the first added first
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
  , stunned :: Bool
  , confused :: Bool
  , tough :: Bool
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
  , code :: CardCode
  , name :: Text
  , hitPoints :: Natural
  , damage :: Natural
  , thwart :: Natural
  , attack :: Natural
  , exhausted :: Bool
  , stunned :: Bool
  , confused :: Bool
  , tough :: Bool
  , counters :: Natural
  , upgrades :: [UpgradeView]
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data SupportView = SupportView {id :: SupportId, code :: CardCode, name :: Text, exhausted :: Bool, uses :: Natural, facedownCards :: Int, threat :: Natural}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data UpgradeView = UpgradeView {id :: UpgradeId, code :: CardCode, name :: Text, exhausted :: Bool, uses :: Natural, controller :: IdentityId, heldCards :: [CardView]}
  -- ^ the cards attached faceup to it, played from there (Hawkeye's Quiver)
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data VillainView = VillainView
  { id :: VillainId
  , code :: CardCode
  , name :: Text
  , stage :: Natural
  , hitPoints :: Int
  , damage :: Natural
  , scheme :: Natural
  , attack :: Natural
  , stunned :: Bool
  , confused :: Bool
  , tough :: Bool
  , boostCards :: Int
  , attachments :: [AttachmentView]
  , upgrades :: [UpgradeView]
  , active :: Bool
  -- ^ the active villain of his area
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data MinionView = MinionView
  { id :: MinionId
  , code :: CardCode
  , name :: Text
  , engagedWith :: IdentityId
  , hitPoints :: Natural
  , damage :: Natural
  , scheme :: Natural
  , attack :: Natural
  , stunned :: Bool
  , confused :: Bool
  , tough :: Bool
  , boostCards :: Int
  , attachments :: [AttachmentView]
  , upgrades :: [UpgradeView]
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data EnvironmentView = EnvironmentView {id :: EnvironmentId, code :: CardCode, name :: Text, attachments :: [AttachmentView], counters :: Natural}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data ObligationView = ObligationView {id :: ObligationId, code :: CardCode, name :: Text}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data AttachmentView = AttachmentView {id :: AttachmentId, code :: CardCode, name :: Text, damage :: Natural, counters :: Natural}
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
  minions <- traverse minionView (HashMap.elems $ gameMinions g)
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
            _ -> Nothing
        , paid
        , wilds = [i | (i, True) <- zip [0 ..] (concatMap wildOrNot (payments c.activeCostPayment))]
        , owed = owedFor (costResources c.activeCostCost) paid
        , anyAmount = dynamic c.activeCostCost
        , spent = map (cardView . PlayerCard) c.activeCostSpentCards
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
      , sideSchemes = map sideSchemeView (HashMap.elems $ gameSideSchemes g)
      , minions
      , environments =
          [ EnvironmentView a.environmentId (toCardCode a.environmentCardDef) (display $ cdName a.environmentCardDef) (attachmentsIn a.environmentAttachments) a.environmentCounters
          | e <- HashMap.elems (gameEnvironments g)
          , let a = toAttrs e
          ]
      , encounterDeckSize = length scenario.scenarioEncounterDeck
      , encounterDiscard = map (cardView . EncounterCard) scenario.scenarioDiscard
      , victoryDisplay = map (cardView . EncounterCard) (gameVictoryDisplay g)
      , focusedCards = map cardView (gameFocusedCards g <> concat [cs | (pid, cs) <- HashMap.toList (gamePrivateCards g), seesHand pid])
      , attack = headMay $ mapMaybe attackView (map Villain.villainAttackDetails (HashMap.elems (gameVillains g)) <> map Minion.minionAttackDetails (HashMap.elems (gameMinions g)))
      , threat
      , boostCards =
          [BoostView (ecCardId c) Nothing Nothing | v <- HashMap.elems (gameVillains g), c <- reverse (toAttrs v).villainBoostCards]
            <> [BoostView (ecCardId c) Nothing Nothing | m <- HashMap.elems (gameMinions g), c <- (toAttrs m).minionBoostCards]
            <> [BoostView c.id (Just c.code) (Just c.name) | c <- map (cardView . EncounterCard) (HashMap.elems (gameBoostCards g))]
      , accelerationTokens = sum (map areaAccelerationTokens (gameAreas g))
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
          , accelerationTokens = areaAccelerationTokens a
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
        , stunned = a.playerIdentityStunned
        , confused = a.playerIdentityConfused
        , tough = a.playerIdentityTough
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
        , dealtCardIds = map ecCardId a.playerIdentityEncounterCards
        , allies
        , allyLimit
        , supports = [supportView s | s <- HashMap.elems (gameSupports g), toId s `HashSet.member` a.playerIdentitySupports]
        , upgrades = upgradesOn \u -> u.upgradeController == toId p && isNothing u.upgradeAttachedAlly && isNothing u.upgradeAttachedEnemy && isNothing u.upgradeAttachedSideScheme && isNothing u.upgradeAttachedMainScheme
        , engagedMinions = HashSet.toList a.playerIdentityMinions
        , attachments = [attachmentView x | x <- HashMap.elems (gameAttachments g), (toAttrs x).attachmentIdentity == Just (toId p)]
        , obligations =
            [ ObligationView a.obligationId (toCardCode a.obligationCardDef) (display $ cdName a.obligationCardDef)
            | a <- map toAttrs (HashMap.elems (gameObligations g))
            , a.obligationOwner == Just (toId p)
            ]
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
        , stunned = a.allyStunned
        , confused = a.allyConfused
        , tough = a.allyTough
        , counters = a.allyCounters
        , upgrades = upgradesOn \u -> u.upgradeAttachedAlly == Just a.allyId
        }
  supportView (s :: Support) =
    let a = toAttrs s
     in SupportView a.supportId (toCardCode a.supportCardDef) (display $ cdName a.supportCardDef) a.supportExhausted a.supportUses (length a.supportHeldCards) a.supportThreat
  upgradeView (u :: Upgrade) =
    let a = toAttrs u
     in UpgradeView a.upgradeId (toCardCode a.upgradeCardDef) (display $ cdName a.upgradeCardDef) a.upgradeExhausted a.upgradeUses a.upgradeController (map (cardView . PlayerCard) a.upgradeHeldCards)
  attachmentView (x :: Attachment) =
    let a = toAttrs x
     in AttachmentView a.attachmentId (toCardCode a.attachmentCardDef) (display $ cdName a.attachmentCardDef) a.attachmentDamage a.attachmentCounters
  villainView (v :: Villain) = do
    let a = toAttrs v
    sch <- Villain.getModifiedScheme a
    atk <- Villain.getModifiedAttack a
    pure
      VillainView
        { id = a.villainId
        , code = toCardCode a.villainCardDef
        , name = display (cdName a.villainCardDef)
        , stage = a.villainStage
        , hitPoints = unHp a.villainMaxHp
        , damage = Villain.villainDamage v
        , scheme = sch
        , attack = maybe atk Attack.attackDamage (Villain.villainAttackDetails v)
        , stunned = a.villainStunned
        , confused = a.villainConfused
        , tough = a.villainTough
        , boostCards = length a.villainBoostCards
        , attachments = attachmentsIn a.villainAttachments
        , upgrades = upgradesOn \u -> u.upgradeAttachedEnemy == Just (EnemyVillainId a.villainId)
        , active = any (\area -> activeVillainIn g (areaId area) == Just a.villainId) (gameAreas g)
        }
  minionView (m :: Minion) = do
    let a = toAttrs m
    hp <- Minion.getModifiedHitPoints a
    atk <- Minion.getModifiedAttack a
    pure
      MinionView
        { id = a.minionId
        , code = toCardCode a.minionCardDef
        , name = display (cdName a.minionCardDef)
        , engagedWith = a.minionEngagedIdentity
        , hitPoints = hp
        , damage = a.minionDamage
        , scheme = unSch a.minionScheme
        , attack = maybe atk Attack.attackDamage (Minion.minionAttackDetails m)
        , stunned = a.minionStunned
        , confused = a.minionConfused
        , tough = a.minionTough
        , boostCards = length a.minionBoostCards
        , attachments = attachmentsIn a.minionAttachments
        , upgrades = upgradesOn \u -> u.upgradeAttachedEnemy == Just (EnemyMinionId a.minionId)
        }
  mainSchemeView (s :: MainScheme) =
    let a = toAttrs s
     in SchemeView (idText a.mainSchemeId) (toCardCode a.mainSchemeCardDef) (display $ cdName a.mainSchemeCardDef) a.mainSchemeThreat a.mainSchemeCrisis (upgradesOn \u -> u.upgradeAttachedMainScheme == Just a.mainSchemeId) a.mainSchemeCounters [] (length a.mainSchemeHeldCards) (map heldView a.mainSchemeHeldCards)
  sideSchemeView (s :: SideScheme) =
    let a = toAttrs s
     in SchemeView (idText a.sideSchemeId) (toCardCode a.sideSchemeCardDef) (display $ cdName a.sideSchemeCardDef) a.sideSchemeThreat a.sideSchemeCrisis (upgradesOn \u -> u.upgradeAttachedSideScheme == Just a.sideSchemeId) 0 [attachmentView x | x <- HashMap.elems (gameAttachments g), (toAttrs x).attachmentSideScheme == Just a.sideSchemeId] (length a.sideSchemeHeldCards) (map heldView a.sideSchemeHeldCards)

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
        named c = if c.kind == "target" && isJust c.target && c.target == (refId =<< t) && isNothing c.label then c {label = Just l} else c
     in v {prompt = Just l, source = refId =<< t, choices = map named v.choices}
  Read r q -> (questionView asked q) {reading = r}
  Searched cs q -> (questionView asked q) {searched = [SearchedView l (map cardView xs) | (l, xs) <- cs]}
  ChooseOne cs -> QuestionView "chooseOne" Nothing Nothing (described cs) [] []
  ChooseN _ cs -> QuestionView "chooseN" Nothing Nothing (described cs) [] []
  ChooseOneAtATime cs -> QuestionView "chooseOneAtATime" Nothing Nothing (described cs) [] []
  ChoosePlayerOrder (Unsorted xs) _ ->
    QuestionView "choosePlayerOrder" Nothing Nothing [ChoiceView n "player" Nothing (Just $ idText x) Nothing Nothing | (n, x) <- zip [0 ..] xs] [] []
 where
  described cs = [(describeChoice asked c) {index = n} | (n, c) <- zip [0 ..] cs]

-- | What a choice is about, for the client to put it on the card it concerns: the
-- player asked defends with their own identity.
describeChoice :: IdentityId -> Choice -> ChoiceView
describeChoice asked = \case
  -- a card chosen for what choosing it does: the card itself is all there is to show
  CardLabel code (Run _) -> (plain "card" Nothing) {code = Just code}
  CardLabel code c -> (describeChoice asked c) {code = Just code}
  Label l _ -> plain "label" (Just l)
  TargetLabel t body -> (on "target" t) {kind = fromMaybe "target" (doneTo t body)}
  EndTurn -> plain "endTurn" (Just "$label.endTurn")
  UseAbility a ->
    (on "ability" (refocus a.abilitySource))
      { label = a.abilityLabel <|> basicLabel a.abilityChoices
      , abilityType = Just a.abilityType
      }
  RunAbility t _ -> on "ability" t
  ChangeForm -> plain "changeForm" (Just "$label.changeForm")
  ChangeToForm side -> plain "changeForm" (Just $ "$label.changeTo" <> tshow side)
  PlayCard c _ -> (onCard "play" (toCardId c)) {code = Just (toCardCode c)}
  PayWithCard c -> (onCard "pay" (toCardId c)) {code = Just (toCardCode c)}
  FinishPayment -> plain "finishPayment" (Just "$label.finishPayment")
  -- the nth wild paid: the client puts it on that wild
  DeclareWild n r -> (plain "declareWild" (Just $ tshow r)) {target = Just (tshow n)}
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
  _ -> []

dynamic :: Cost -> Bool
dynamic = \case
  DynamicResourceCost _ -> True
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

