{-# LANGUAGE DuplicateRecordFields #-}

-- | The game as the client draws it: one record per thing on the table, each card by its
-- id and code with its stats as they now stand, and each choice of the open questions
-- described by what it is about. Decks are hidden information, so only their sizes go
-- out, and at a game with friends so do the hands of the players a viewer does not hold.
--
-- Stored games and exports keep the engine's own JSON; this is only ever sent.
module Marvel.Game.Public (publicGame, publicGameFor, GameView (..), ChoiceView (..)) where

import Marvel.Prelude

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Marvel.Ability.Types (Ability (..), AbilityType)
import Marvel.Ally.Types (Ally)
import Marvel.Ally.Types qualified as Ally
import Marvel.AlterEgo.Types qualified as AlterEgo
import Marvel.Attachment.Types (Attachment, Attrs (..))
import Marvel.Card
import Marvel.Deck
import Marvel.Discard
import Marvel.Hand
import Marvel.Entity
import Marvel.Game
import Marvel.Hero.Runner qualified as Hero
import Marvel.Hero.Types qualified as HeroTypes
import Marvel.Hp
import Marvel.Id
import Marvel.Identity
import Marvel.MainScheme.Types (Attrs (..), MainScheme)
import Marvel.Message (FinishedStatus (..))
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
  , encounterDeckSize :: Int
  , encounterDiscard :: [CardView]
  , focusedCards :: [CardView]
  , accelerationTokens :: Natural
  , question :: HashMap IdentityId QuestionView
  , questionVersion :: Int
  }
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
  , allies :: [AllyView]
  , supports :: [SupportView]
  , upgrades :: [UpgradeView]
  -- ^ on the identity itself
  , engagedMinions :: [MinionId]
  }
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

data SupportView = SupportView {id :: SupportId, code :: CardCode, name :: Text, exhausted :: Bool, uses :: Natural}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data UpgradeView = UpgradeView {id :: UpgradeId, code :: CardCode, name :: Text, exhausted :: Bool, uses :: Natural, controller :: IdentityId}
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
  , attachments :: [AttachmentView]
  , upgrades :: [UpgradeView]
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data AttachmentView = AttachmentView {id :: AttachmentId, code :: CardCode, name :: Text, damage :: Natural}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data SchemeView = SchemeView
  { id :: Text
  , code :: CardCode
  , name :: Text
  , threat :: Natural
  , crisis :: Bool
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data QuestionView = QuestionView
  { kind :: Text
  -- ^ @chooseOne@, @chooseN@, @chooseOneAtATime@ or @choosePlayerOrder@
  , choices :: [ChoiceView]
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

-- | One choice, as the client offers it: on a card when it is about one, as a button
-- otherwise.
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
      , encounterDeckSize = length scenario.scenarioEncounterDeck
      , encounterDiscard = map (cardView . EncounterCard) scenario.scenarioDiscard
      , focusedCards = map cardView (gameFocusedCards g)
      , accelerationTokens = scenario.scenarioAccelerationTokens
      , question = HashMap.map questionView (gameQuestion g)
      , questionVersion = gameQuestionVersion g
      }
 where
  scenario = toAttrs (gameScenario g)
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
    (thw, atk, def, rec) <- case currentIdentity p of
      HeroSide h -> do
        let ha = toAttrs h
        (,,,) <$> (Just <$> Hero.getModifiedThwart ha) <*> (Just <$> Hero.getModifiedAttack ha) <*> (Just <$> Hero.getModifiedDefense ha) <*> pure Nothing
      AlterEgoSide ae -> pure (Nothing, Nothing, Nothing, Just (unRec (toAttrs ae).alterEgoBaseRecovery))
    allies <- traverse allyView [ally | ally <- HashMap.elems (gameAllies g), toId ally `HashSet.member` a.playerIdentityAllies]
    pure
      PlayerView
        { id = toId p
        , name = a.playerIdentityName
        , form
        , code = toCardCode p
        , otherCode
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
        , allies
        , supports = [supportView s | s <- HashMap.elems (gameSupports g), toId s `HashSet.member` a.playerIdentitySupports]
        , upgrades = upgradesOn \u -> u.upgradeController == toId p && isNothing u.upgradeAttachedAlly && isNothing u.upgradeAttachedEnemy
        , engagedMinions = HashSet.toList a.playerIdentityMinions
        }
  allyView (ally :: Ally) = do
    let a = toAttrs ally
    thw <- Ally.getModifiedThwart a
    atk <- Ally.getModifiedAttack a
    pure
      AllyView
        { id = a.allyId
        , code = toCardCode a.allyCardDef
        , name = display (cdName a.allyCardDef)
        , hitPoints = unHp a.allyHitPoints
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
     in SupportView a.supportId (toCardCode a.supportCardDef) (display $ cdName a.supportCardDef) a.supportExhausted a.supportUses
  upgradeView (u :: Upgrade) =
    let a = toAttrs u
     in UpgradeView a.upgradeId (toCardCode a.upgradeCardDef) (display $ cdName a.upgradeCardDef) a.upgradeExhausted a.upgradeUses a.upgradeController
  attachmentView (x :: Attachment) =
    let a = toAttrs x
     in AttachmentView a.attachmentId (toCardCode a.attachmentCardDef) (display $ cdName a.attachmentCardDef) a.attachmentDamage
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
        , attack = atk
        , stunned = a.villainStunned
        , confused = a.villainConfused
        , tough = a.villainTough
        , boostCards = length a.villainBoostCards
        , attachments = attachmentsIn a.villainAttachments
        , upgrades = upgradesOn \u -> u.upgradeAttachedEnemy == Just (EnemyVillainId a.villainId)
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
        , attack = atk
        , stunned = a.minionStunned
        , confused = a.minionConfused
        , tough = a.minionTough
        , attachments = attachmentsIn a.minionAttachments
        , upgrades = upgradesOn \u -> u.upgradeAttachedEnemy == Just (EnemyMinionId a.minionId)
        }
  mainSchemeView (s :: MainScheme) =
    let a = toAttrs s
     in SchemeView (idText a.mainSchemeId) (toCardCode a.mainSchemeCardDef) (display $ cdName a.mainSchemeCardDef) a.mainSchemeThreat a.mainSchemeCrisis
  sideSchemeView (s :: SideScheme) =
    let a = toAttrs s
     in SchemeView (idText a.sideSchemeId) (toCardCode a.sideSchemeCardDef) (display $ cdName a.sideSchemeCardDef) a.sideSchemeThreat a.sideSchemeCrisis

sideCode :: PlayerIdentitySide -> CardCode
sideCode = \case
  HeroSide x -> toCardCode x
  AlterEgoSide x -> toCardCode x

cardView :: Card -> CardView
cardView c = CardView (toCardId c) (toCardCode c) (display . cdName $ getCardDef c)

questionView :: Question -> QuestionView
questionView = \case
  ChooseOne cs -> QuestionView "chooseOne" (described cs)
  ChooseN _ cs -> QuestionView "chooseN" (described cs)
  ChooseOneAtATime cs -> QuestionView "chooseOneAtATime" (described cs)
  ChoosePlayerOrder (Unsorted xs) _ ->
    QuestionView "choosePlayerOrder" [ChoiceView n "player" Nothing (Just $ idText x) Nothing Nothing | (n, x) <- zip [0 ..] xs]
 where
  described cs = [(describeChoice c) {index = n} | (n, c) <- zip [0 ..] cs]

-- | What a choice is about, for the client to put it on the card it concerns.
describeChoice :: Choice -> ChoiceView
describeChoice = \case
  CardLabel code c -> (describeChoice c) {code = Just code}
  Label l _ -> plain "label" (Just l)
  TargetLabel t _ -> on "target" t
  EndTurn -> plain "endTurn" (Just "$label.endTurn")
  UseAbility a ->
    (on "ability" (refocus a.abilitySource))
      { label = a.abilityLabel
      , abilityType = Just a.abilityType
      }
  RunAbility t _ -> on "ability" t
  ChangeForm -> plain "changeForm" (Just "$label.changeForm")
  ChangeToForm side -> plain "changeForm" (Just $ "$label.changeTo" <> tshow side)
  PlayCard c _ -> (onCard "play" (toCardId c)) {code = Just (toCardCode c)}
  PayWithCard c -> (onCard "pay" (toCardId c)) {code = Just (toCardCode c)}
  FinishPayment -> plain "finishPayment" (Just "$label.finishPayment")
  ReadyIdentity -> plain "label" (Just "$label.readyIdentity")
  Recover -> plain "recover" (Just "$label.recover")
  Attack -> plain "attack" (Just "$label.attack")
  Thwart -> plain "thwart" (Just "$label.thwart")
  Defend e -> on "defend" (EnemyRef e)
  AllyAttack a -> on "allyAttack" (AllyRef a)
  AllyThwart a -> on "allyThwart" (AllyRef a)
  AllyDefend a _ -> on "allyDefend" (AllyRef a)
  DamageEnemy t _ _ -> on "damage" t
  ThwartScheme t _ _ -> on "thwartScheme" t
  Stun t _ -> on "stun" t
  Confuse t _ -> on "confuse" t
  Heal c _ -> on "heal" (CharacterRef c)
  DamageCharacter c _ _ -> on "damage" (CharacterRef c)
  DiscardTarget t -> on "discard" t
  DiscardCard c -> onCard "discard" (toCardId c)
  ReturnTargetToHand t -> on "returnToHand" t
  other -> plain "other" (Just (tshow other))
 where
  plain kind label = ChoiceView 0 kind label Nothing Nothing Nothing
  on kind t = ChoiceView 0 kind Nothing (refId t) Nothing Nothing
  onCard kind cid = ChoiceView 0 kind Nothing (Just $ idText cid) Nothing Nothing

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

idText :: (ToJSON a) => a -> Text
idText a = case toJSON a of
  String t -> t
  other -> tshow other

