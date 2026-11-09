{-# OPTIONS_GHC -Wno-orphans #-}

module Marvel.Game.Setup where

import Data.HashMap.Strict qualified as HashMap (insert)
import Marvel.Ally (Ally, withAllyCardCode)
import Marvel.Ally.Types qualified as Ally (Ally (Ally))
import Marvel.AlterEgo.Cards (allAlterEgosMap)
import Marvel.Area (defaultAreaId, newArea)
import Marvel.Attachment (withAttachmentCardCode)
import Marvel.Attachment.Types (Attachment)
import Marvel.Attachment.Types qualified as Attachment (
  Attachment (Attachment),
 )
import Marvel.Campaign qualified as Campaign (noShieldLog)
import Marvel.Card (
  CardCode,
  CardDef (cdTraits),
  CardId (unCardId),
  EncounterCard (ecCardDef, ecCardId),
  HasCardCode (toCardCode),
  HasCardDef (getCardDef),
  PlayerCard (pcCardDef, pcCardId),
  Side (C),
  buildCard,
  flipToSide,
  reprintAs,
  toAlterEgoCardCode,
  toBaseCardCode,
  toHeroCardCode,
 )
import Marvel.Deck (Deck)
import Marvel.Effect (lookupEffect)
import Marvel.Effect.Types (Effect)
import Marvel.Entity (Entity (toId))
import Marvel.Environment (withEnvironmentCardCode)
import Marvel.Environment.Types (Environment)
import Marvel.Environment.Types qualified as MGEnvironment (
  Environment (Environment),
 )
import Marvel.Event (withEventCardCode)
import Marvel.Event.Types (Event (..))
import Marvel.Game.Base (
  Entities (entitiesPlayers),
  GainedTraits (GainedTraits),
  Game (..),
  GameState (Unstarted),
  defaultEntities,
 )
import Marvel.Game.Source ()
import Marvel.Hero.Cards (allHeroesMap)
import Marvel.Id (
  AllyId (AllyId),
  AttachmentId (AttachmentId),
  EffectId,
  EnvironmentId (EnvironmentId),
  EventId (EventId),
  IdentityId,
  MinionId (MinionId),
  ObligationId (ObligationId),
  SideSchemeId (SideSchemeId),
  SupportId (SupportId),
  TreacheryId (TreacheryId),
  UpgradeId (UpgradeId),
 )
import Marvel.Identity (
  Attrs (playerIdentitySides),
  PlayerIdentity (..),
  createIdentity,
  lookupAlterEgo,
  lookupHero,
  setDeck,
 )
import Marvel.Matchers (EntityMatcher)
import Marvel.Message.Lifted (
  EnvironmentMessage (EnvironmentOwnedBy),
  ReverseQueue,
  environmentStep,
 )
import Marvel.Minion (withMinionCardCode)
import Marvel.Minion.Types (Minion)
import Marvel.Minion.Types qualified as Minion (Minion (Minion))
import Marvel.Obligation (withObligationCardCode)
import Marvel.Obligation.Types (Obligation)
import Marvel.Obligation.Types qualified as Obligation (
  Obligation (Obligation),
 )
import Marvel.Phase (Phase (PlayerPhase))
import Marvel.Prelude
import Marvel.Ref (Source)
import Marvel.Scenario.Types (Scenario)
import Marvel.SideScheme (withSideSchemeCardCode)
import Marvel.SideScheme.Types (SideScheme)
import Marvel.SideScheme.Types qualified as SideScheme (
  SideScheme (SideScheme),
 )
import Marvel.Support (withSupportCardCode)
import Marvel.Support.Types (Support)
import Marvel.Support.Types qualified as Support (
  Support (Support),
 )
import Marvel.Trait (Trait (Spell))
import Marvel.Treachery (withTreacheryCardCode)
import Marvel.Treachery.Types (Treachery)
import Marvel.Treachery.Types qualified as Treachery (
  Treachery (Treachery),
 )
import Marvel.Upgrade (withUpgradeCardCode)
import Marvel.Upgrade.Types (Upgrade)
import Marvel.Upgrade.Types qualified as Upgrade (
  Upgrade (Upgrade),
 )

createAlly :: IdentityId -> PlayerCard -> Ally
createAlly ident card =
  withAllyCardCode (toCardCode card) $ \builder ->
    Ally.Ally $ buildCard (reprintAs (pcCardDef card) builder) (ident, AllyId (unCardId $ pcCardId card))

createSupport :: IdentityId -> PlayerCard -> Support
createSupport ident card =
  withSupportCardCode (toCardCode card) $ \builder ->
    Support.Support $ buildCard (reprintAs (pcCardDef card) builder) (ident, SupportId (unCardId $ pcCardId card))

createUpgrade :: IdentityId -> PlayerCard -> Upgrade
createUpgrade ident card =
  withUpgradeCardCode (toCardCode card) $ \builder ->
    Upgrade.Upgrade $ buildCard (reprintAs (pcCardDef card) builder) (ident, UpgradeId (unCardId $ pcCardId card))

createEvent :: IdentityId -> PlayerCard -> Event
createEvent ident card =
  withEventCardCode (toCardCode card) $ \builder ->
    Event $ buildCard (reprintAs (pcCardDef card) builder) (ident, EventId (unCardId $ pcCardId card))

createSideScheme :: EncounterCard -> SideScheme
createSideScheme card =
  withSideSchemeCardCode (toCardCode card) $ \builder ->
    SideScheme.SideScheme $ buildCard (reprintAs (ecCardDef card) builder) (SideSchemeId $ unCardId $ ecCardId card)

createTreachery :: EncounterCard -> Treachery
createTreachery card =
  withTreacheryCardCode (toCardCode card) $ \builder ->
    Treachery.Treachery $ buildCard (reprintAs (ecCardDef card) builder) (TreacheryId $ unCardId $ ecCardId card)

createObligation :: EncounterCard -> Obligation
createObligation card =
  withObligationCardCode (toCardCode card) $ \builder ->
    Obligation.Obligation $ buildCard (reprintAs (ecCardDef card) builder) (ObligationId $ unCardId $ ecCardId card)

createMinion :: IdentityId -> EncounterCard -> Minion
createMinion ident card =
  withMinionCardCode (toCardCode card) $ \builder ->
    Minion.Minion $ buildCard (reprintAs (ecCardDef card) builder) (ident, MinionId (unCardId $ ecCardId card))

createAttachment :: EncounterCard -> Attachment
createAttachment card =
  withAttachmentCardCode (toCardCode card) $ \builder ->
    Attachment.Attachment $ buildCard (reprintAs (ecCardDef card) builder) (AttachmentId $ unCardId $ ecCardId card)

-- | A Spell environment is put in the play area of the player revealing it, or putting
-- it into play (MTS rulebook p.6, Ebony Maw).
spellOwnedBy :: (ReverseQueue m) => IdentityId -> Environment -> m ()
spellOwnedBy ident environment =
  when (Spell `member` cdTraits (getCardDef environment))
    $ environmentStep (toId environment) (EnvironmentOwnedBy ident)

createEnvironment :: EncounterCard -> Environment
createEnvironment card =
  withEnvironmentCardCode (toCardCode card) $ \builder ->
    MGEnvironment.Environment $ buildCard (reprintAs (ecCardDef card) builder) (EnvironmentId $ unCardId $ ecCardId card)

createEffect :: EffectId -> CardCode -> Source -> EntityMatcher -> Effect
createEffect ident cardCode source matcher =
  lookupEffect cardCode source matcher ident

newGame :: PlayerIdentity -> Scenario -> Game
newGame player scenario =
  Game
    { gamePrintedPlayerCounts = []
    , gamePlayerCountContext = Nothing
    , gameCompetition = Nothing
    , gamePhase = PlayerPhase
    , gameState = Unstarted
    , gamePlayerOrder = [toId player]
    , gameBoostEntities = defaultEntities
    , gameRemovedFromGameEncounterCards = []
    , gameRemovedEntities = defaultEntities
    , gameEntities =
        defaultEntities
          { entitiesPlayers = fromList [(toId player, player)]
          }
    , gameActiveCost = []
    , gameWindowDepth = 0
    , gameDepth = 0
    , gameWindows = []
    , gameQuestion = mempty
    , gameUsedAbilities = mempty
    , gameActivePlayer = toId player
    , gamePayingForAbility = Nothing
    , gameResolvingEvents = Just []
    , gameResolvingAbilitySubTypes = []
    , gameYou = Nothing
    , gameScenario = scenario
    , gameFocusedCards = []
    , gamePrivateCards = mempty
    , gameBoostCards = mempty
    , gameQuestionVersion = 0
    , gameRound = 1
    , gameSeats = [toId player]
    , gamePlayedInWindows = []
    , gamePlayedThisRound = []
    , gamePlayedThisPhase = []
    , gameAttacksThisTurn = []
    , gameHeroAttackedThisPhase = []
    , gameHeroThwartedThisPhase = []
    , gamePlayedThisTurn = []
    , gameTreacheriesRevealedThisPhase = []
    , gameAreas = [newArea defaultAreaId [toId player]]
    , gameSignatureSchemes = mempty
    , gameAtEndOfPhase = []
    , gameMissionPlayDestination = Nothing
    , gameMissionTargets = False
    , gameAllAreas = False
    , gameShieldLog = Campaign.noShieldLog
    , gameCampaign = Nothing
    , gameVictoryDisplay = []
    , gamePlayerVictoryDisplay = []
    , gamePeril = Nothing
    , gameDefeatedMinionBoosts = []
    , gameInPlayBoosts = []
    , gameBoostPlayerOverride = Nothing
    , gameCancelledBoostIcons = []
    , gameFriendlyBoosts = []
    , gamePlayerBoostCards = []
    , gameCollection = []
    , gameCollecting = []
    , gameUncontrolledAllies = []
    , gameAosHostages = []
    , gameAosBackup = []
    , gameCharacterThreat = []
    , gameBlankedAttackTargets = []
    , gameTrackedEnemyAttacks = []
    , gameSimultaneousMinionAttack = Nothing
    , gameBombCounters = []
    , gameBoostAllyDefeatEffects = []
    , gamePendingAttackDefeats = []
    , gameGainedTraits = GainedTraits Nothing
    }

{- | A card with the permanent keyword cannot leave play. ponytail: none in play can be
removed by its own set yet, so nothing makes an exception.
-}
initPlayer :: (MonadRandom m) => CardCode -> Deck -> m PlayerIdentity
initPlayer requestedCardCode deck = do
  ident <- getRandom
  let
    -- Progressing identities always begin with their weakest version.
    cardCode = if toBaseCardCode requestedCardCode `elem` ["29002", "29003"] then "29001a" else requestedCardCode
    mAlterEgo = do
      def <- lookup (toAlterEgoCardCode cardCode) allAlterEgosMap
      lookupAlterEgo def ident
    mHero = do
      def <- lookup (toHeroCardCode cardCode) allHeroesMap
      lookupHero def ident
    -- Ant-Man's fold-out card: a second hero form on side C
    addThirdSide p@(PlayerIdentity a) = case lookup (flipToSide C cardCode) allHeroesMap >>= (`lookupHero` ident) of
      Just giant -> PlayerIdentity a {playerIdentitySides = HashMap.insert C giant (playerIdentitySides a)}
      Nothing -> p
  case (mAlterEgo, mHero) of
    (Just alterEgoSide, Just heroSide) ->
      pure . setDeck deck . addThirdSide $ createIdentity ident alterEgoSide heroSide
    _ -> error "stuff"
