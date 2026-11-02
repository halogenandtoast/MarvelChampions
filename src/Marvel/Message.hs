module Marvel.Message (
  module Marvel.Message,
  module X,
) where

import Marvel.Prelude
import Data.List (nub)
import Marvel.Window.Types qualified as W

import Marvel.Message.Ally as X
import Marvel.Message.Attachment as X
import Marvel.Message.Deck as X
import Marvel.Message.Effect as X
import Marvel.Message.Environment as X
import Marvel.Message.Event as X
import Marvel.Message.Identity as X
import Marvel.Message.MainScheme as X
import Marvel.Message.Minion as X
import Marvel.Message.Obligation as X
import Marvel.Message.SideScheme as X
import Marvel.Message.Support as X
import Marvel.Message.Treachery as X
import Marvel.Message.Upgrade as X
import Marvel.Message.Villain as X

import Marvel.Ability.Types
import Marvel.ActiveCost.Types
import Marvel.Area (AreaId, DeckKey)
import Marvel.Campaign (CampaignRecord, CampaignStepKind)
import Marvel.Card.Types
import Marvel.Cost.Types (Cost)
import Marvel.Game.Source
import Marvel.Attack (sourceEnemy)
import Marvel.Damage (Damage (..), DamageSource (FromAttack, FromOverkill, FromRetaliate, FromPlayerAttack, FromAllyAttack), toDamage)
import Marvel.Id
import Marvel.Identity.Types
import Marvel.Matchers.Types
import Marvel.Modifier.Types (Modifier (CannotBeCanceled, UseDefenseForAttackAndThwart))
import Marvel.Payment.Types
import Marvel.Resource.Types (Resource)
import Marvel.Phase
import Marvel.Projection
import {-# SOURCE #-} Marvel.Question
import Marvel.Queue
import Marvel.Ref
import Marvel.Status as X
import Marvel.Window.Types qualified
import Marvel.Window.Types (RevealSource, ThreatSource (ThreatFromAbility), Window (..), WindowTiming (After, When, Would), WindowType (EngagedMinion, AllyTakeDamage, DamagedVillain, DefeatedMinion, DefeatedVillain, EnemyAttack, EnemyAttacksAndDamages, IdentityTakeDamage, IdentityTakeDamageFrom, ThreatPlaced))

class RunMessage a where
  runMessage :: (HasCallStack, MonadThrow m, MonadRandom m, HasGame m, HasQueue m, Projection m PlayerIdentity) => Message -> a -> m a

-- | Damage to a hero or alter-ego from anything but an attack: first the window for
-- what answers "when you would take damage", then the damage.
damageIdentity :: IdentityId -> Source -> Damage -> [Message]
damageIdentity ident source damage
  | damagePiercing damage && damageAmount damage > 0 = [CardSpecific (IdentityRef ident) "discardToughForPiercing" (toJSON (source, damage))]
  | otherwise =
  [ CheckWindows [Window When $ IdentityTakeDamage ident damage, Window When $ IdentityTakeDamageFrom ident source damage]
  , IdentityMessage ident $ IdentityDamaged source damage
  ]

-- | Damage to an ally: first the window for what answers "when a character would take
-- damage", then the damage.
damageAlly :: AllyId -> Source -> Damage -> [Message]
damageAlly aid source damage
  | damagePiercing damage && damageAmount damage > 0 = [CardSpecific (AllyRef aid) "discardToughForPiercing" (toJSON (source,damage))]
  | otherwise =
  [ CheckWindows [Window When $ AllyTakeDamage aid damage, Window When $ Marvel.Window.Types.AllyTakeDamageFrom aid source damage]
  , AllyMessage aid $ AllyDamaged source damage
  ]

-- | Prevent some of the damage the ally would take, as an interrupt to "when a character
-- would take damage" does.
preventAllyDamage :: (HasQueue m) => AllyId -> Natural -> m ()
preventAllyDamage aid n = replaceMatchingMessage prevent \case
  AllyMessage aid' (AllyDamaged _ _) -> aid' == aid
  _ -> False
 where
  prevent = \case
    msg@(AllyMessage _ (AllyDamaged _ dmg)) | damageUnpreventable dmg -> [msg]
    AllyMessage a (AllyDamaged source dmg) ->
      [AllyMessage a $ AllyDamaged source dmg {damageAmount = damageAmount dmg - n} | damageAmount dmg > n]
    msg -> [msg]

-- | Prevent some of the damage the identity would take, as an interrupt to "when you
-- would take damage" does: all of it prevented, the damage is not taken at all.
preventDamage :: (HasQueue m) => IdentityId -> Natural -> m ()
preventDamage ident n = replaceMatchingMessage prevent \case
  IdentityMessage ident' (IdentityDamaged _ _) -> ident' == ident
  _ -> False
 where
  prevent = \case
    msg@(IdentityMessage _ (IdentityDamaged _ dmg)) | damageUnpreventable dmg -> [msg]
    IdentityMessage i (IdentityDamaged source dmg) ->
      [ IdentityMessage i $ IdentityDamaged source dmg {damageAmount = damageAmount dmg - n}
      | damageAmount dmg > n
      ]
    msg -> [msg]

-- | Threat a card places on a scheme: first the window for what answers "when threat
-- would be placed", then the threat.
placeThreatOn :: SchemeId -> Natural -> [Message]
placeThreatOn scheme n =
  [CheckWindows [Window Would $ ThreatPlaced ThreatFromAbility scheme n] | n > 0]
    <> [threatPlacement scheme n]

-- | The threat placed on the scheme, past the window for it.
threatPlacement :: SchemeId -> Natural -> Message
threatPlacement scheme n = case scheme of
  SchemeMainSchemeId sid -> MainSchemeMessage sid (MainSchemePlaceThreat n)
  SchemeSideSchemeId sid -> SideSchemeMessage sid (SideSchemePlaceThreat n)

-- | Whether the message places threat on the scheme.
isThreatPlacement :: SchemeId -> Message -> Bool
isThreatPlacement scheme = \case
  MainSchemeMessage sid (MainSchemePlaceThreat _) -> scheme == SchemeMainSchemeId sid
  SideSchemeMessage sid (SideSchemePlaceThreat _) -> scheme == SchemeSideSchemeId sid
  _ -> False

-- | Cancel a treachery's "When Revealed" effects as it is revealed: none of them resolves,
-- the surge it has or would gain included (surge is a "When Revealed" ability), and it is
-- discarded as it would be.
cancelWhenRevealed :: (HasQueue m, HasGame m) => TreacheryId -> m ()
cancelWhenRevealed tid = do
  forbidden <- elem CannotBeCanceled <$> getModifiers (TreacheryRef tid)
  resolvedPart <- treacheryHasResolvedPart tid
  unless forbidden $ cancelMatchingMessage $ \case
    CardSpecific GameRef "grantedSurge" (fromJSON -> Success (_ :: IdentityId, cid :: CardId)) -> cid == CardId (unTreacheryId tid)
    _ -> False
  unless forbidden $ withQueue_ $ mapMaybe $ \case
    Ask _ q | asksToResolveWhenRevealed (TreacheryRef tid) q -> Nothing
    TreacheryMessage tid' msg | tid' == tid -> case msg of
      RevealTreachery _ -> Nothing
      TreacheryIncite -> Nothing
      CheckTreacheryCondition _ -> Nothing
      ResolvedTreachery who -> Just $ if resolvedPart then TreacheryMessage tid (ResolvedTreacheryAfterCancel who) else RemoveFromPlay (TreacheryRef tid)
      TreacheryMarkResolved -> Nothing
      ResolvedTreacheryAfterCancel who -> Just $ TreacheryMessage tid (ResolvedTreacheryAfterCancel who)
    msg -> Just msg

-- | A minion that would be defeated is not ("instead"): its defeat, the damage overkill
-- would deal past it, and the window after its defeat are cancelled.
cancelMinionDefeat :: (HasQueue m) => MinionId -> m ()
cancelMinionDefeat mid = withQueue_ \q -> case break isDefeat q of
  (pre, _ : rest) -> pre <> cancelAfter (dropOverkill rest)
  _ -> q
 where
  isDefeat = \case
    MinionMessage mid' MinionDefeated -> mid' == mid
    ResolveMinionLeavingPlay mid' _ -> mid' == mid
    _ -> False
  -- the excess on to the villain, its "would take damage" window first when it opens
  dropOverkill = \case
    message : rest | Just (_, _, damage) <- enemyDamageMessage (queuedMessage message), damageSource damage == FromOverkill -> rest
    CheckWindows windows : VillainMessage _ (VillainDamaged _ d) : rest
      | any (\case Window When (DamagedVillain _ other) -> damageSource other == FromOverkill; _ -> False) windows
      , damageSource d == FromOverkill -> rest
    VillainMessage _ (VillainDamaged _ d) : rest | damageSource d == FromOverkill -> rest
    rest -> rest
  cancelAfter rest = case break isAfter rest of
    (pre, _ : rest') -> pre <> rest'
    _ -> rest
  isAfter = \case
    CheckWindows ws -> any (\case Window After (DefeatedMinion mid' _ _ _) -> mid' == mid; _ -> False) ws
    _ -> False

-- | Replace a suspended minion departure. Once cancelled, the would-leave window
-- is no longer imminent, so another replacement cannot consume itself.
cancelMinionLeavingPlay :: HasQueue m => MinionId -> m ()
cancelMinionLeavingPlay mid = do
  cancelMinionDefeat mid
  withQueue_ $ filter $ \case
    ResolveMinionLeavingPlay mid' _ -> mid' /= mid
    _ -> True

-- | A villain that would be defeated is not ("instead"), as it answers
-- 'VillainWouldBeDefeated': the window after its defeat is cancelled.
cancelVillainDefeat :: (HasQueue m) => VillainId -> m ()
cancelVillainDefeat vid = withQueue_ \q -> case break isAfter q of
  (pre, _ : rest) -> pre <> rest
  _ -> q
 where
  isAfter = \case
    CheckWindows ws -> any (\case Window After (DefeatedVillain vid' _ _ _) -> vid' == vid; _ -> False) ws
    _ -> False

-- | The minion engages the player, leaving the player it was engaged with.
engageMinion :: MinionId -> IdentityId -> [Message]
engageMinion minion ident =
  [ CheckWindows [Window When $ EngagedMinion ident minion]
  , IdentityMessage ident (MinionEngaged minion)
  , MinionMessage minion (MinionEngagedIdentity ident)
  ]

-- | Add to the power of the basic attack, thwart or defense the player is making, as an
-- interrupt to making it.
addToBasicPower :: (HasQueue m, HasGame m) => IdentityId -> Natural -> m ()
addToBasicPower ident n = do
  substitutes <- elem UseDefenseForAttackAndThwart <$> getModifiers (IdentityRef ident)
  replaceMatchingMessage (raise substitutes) $ \case
    IdentityMessage ident' (SideMessage (RecoveredWithBonus _)) -> ident == ident'
    IdentityMessage ident' (SideMessage (BasicAttack _)) -> ident == ident'
    IdentityMessage ident' (SideMessage (BasicAttackOn _ _)) -> ident == ident'
    IdentityMessage ident' (SideMessage (BasicThwart _)) -> ident == ident'
    IdentityMessage ident' (SideMessage (DefenseUsingAttackApplied _ _)) -> ident == ident'
    IdentityMessage ident' (SideMessage (DefenseApplied _ _)) -> ident == ident'
    _ -> False
 where
  raise substitutes = \case
    message@(IdentityMessage _ (SideMessage (BasicAttack _))) | substitutes -> [message]
    message@(IdentityMessage _ (SideMessage (BasicAttackOn _ _))) | substitutes -> [message]
    message@(IdentityMessage _ (SideMessage (BasicThwart _))) | substitutes -> [message]
    IdentityMessage i (SideMessage (RecoveredWithBonus x)) -> [IdentityMessage i (SideMessage (RecoveredWithBonus (x + n)))]
    IdentityMessage i (SideMessage (DefenseUsingAttackApplied x e)) -> [IdentityMessage i (SideMessage (DefenseUsingAttackApplied (x + n) e))]
    IdentityMessage i (SideMessage (DefenseApplied x e)) -> [IdentityMessage i (SideMessage (DefenseApplied (x + n) e))]
    IdentityMessage i (SideMessage (BasicAttack x)) -> [IdentityMessage i (SideMessage (BasicAttack (x + n)))]
    IdentityMessage i (SideMessage (BasicAttackOn e x)) -> [IdentityMessage i (SideMessage (BasicAttackOn e (x + n)))]
    IdentityMessage i (SideMessage (BasicThwart x)) -> [IdentityMessage i (SideMessage (BasicThwart (x + n)))]
    other -> [other]

-- | "After [enemy] attacks you" is for the player whose character defended the attack,
-- or was attacked undefended (RR "Defend"): once the enemy's attack has its target
-- character, the after window waiting for it is for that character's controller.
attackedYou :: (HasGame m, HasQueue m) => EnemyId -> CharacterId -> m ()
attackedYou enemy character = do
  you <- case character of
    IdentityCharacter ident -> pure (Just ident)
    AllyCharacter aid -> getSourceController (AllyRef aid)
    _ -> pure Nothing
  for_ you \ident ->
    replaceMatchingMessage (const [CheckWindows [Window After $ EnemyAttack enemy ident]]) \case
      CheckWindows [Window After (EnemyAttack enemy' _)] -> enemy' == enemy
      _ -> False

-- | Record actual damage on the completion window already waiting for this attack.
-- Keeping this on its continuation also preserves attribution of overkill damage.
recordPlayerAttackDamage :: (HasGame m, HasQueue m) => EnemyId -> Source -> Damage -> m ()
recordPlayerAttackDamage enemy source damage = when (damageAmount damage > 0 && isAttack) $ do
  confused <- characterHasCondition ConfusedStatus $ case enemy of
    EnemyVillainId v -> VillainCharacter v
    EnemyMinionId m -> MinionCharacter m
    EnemyAttachmentId a -> AttachmentCharacter a
  replaceMatchingMessage (mapWindow confused) isCompletion
 where
  isAttack = case damageSource damage of
    FromPlayerAttack _ -> True
    FromAllyAttack _ -> True
    FromOverkill -> True
    _ -> False
  isCompletion = \case
    CheckWindows ws -> any completion ws
    _ -> False
  completion (Window After w) = case (source, w) of
    (IdentityRef you, W.MadeBasicAttack who) -> you == who
    (AllyRef aid, W.AllyAttack who _) -> aid == who
    (AllyRef aid, W.AllyAttackCompletedAgainst who _) -> aid == who
    _ -> False
  completion _ = False
  mapWindow confused (CheckWindows ws) = [CheckWindows $ map update ws <> case source of
    IdentityRef you -> [Window After $ W.BasicAttackDamaged you [enemy] | not (any (hasBasic you) ws)]
    AllyRef aid -> [Window After $ W.AllyAttackDamaged aid enemy confused | not (any (hasAllyEnemy aid) ws)]
    _ -> []]
  mapWindow _ message = [message]
  update (Window timing (W.BasicAttackDamaged who enemies)) | source == IdentityRef who = Window timing $ W.BasicAttackDamaged who (nub $ enemies <> [enemy])
  update window = window
  hasBasic you (Window _ (W.BasicAttackDamaged who _)) = who == you
  hasBasic _ _ = False
  hasAllyEnemy aid (Window _ (W.AllyAttackDamaged who target _)) = who == aid && target == enemy
  hasAllyEnemy _ _ = False

-- | After an enemy's attack deals damage to a character, the window for it.
attackDealtDamage :: Source -> Damage -> CharacterId -> [Message]
attackDealtDamage source damage character =
  [ CheckWindows [Window After $ EnemyAttacksAndDamages enemy character (damageAmount damage)]
  | damageSource damage `elem` [FromAttack, FromOverkill]
  , damageAmount damage > 0
  , Just enemy <- [sourceEnemy source]
  ]

-- | Damage dealt to each of several targets as one effect, each given as the messages
-- dealing it ('DamageEnemy'): the interrupts to all of it, then all of the damage at
-- once, then what answers it.
simultaneousDamage :: [[Message]] -> [Message]
simultaneousDamage dealt = [CheckWindows attacks | notNull attacks] <> concatMap withoutAttacks (concat befores) <> [Simultaneously (concat damage) | notNull (concat damage)] <> concat afters
 where
  (befores, damage, afters) = unzip3 $ map splitDamage dealt
  attacks = nub [w | CheckWindows ws <- concat befores, w <- ws, isAttack w]
  isAttack (Window When (W.IdentityAttack _ _)) = True
  isAttack (Window When (W.IdentityAttackTarget _ _)) = True
  isAttack _ = False
  withoutAttacks (CheckWindows ws) = [CheckWindows rest | let rest = filter (not . isAttack) ws, notNull rest]
  withoutAttacks message = [message]

-- | The messages dealing damage ('DamageEnemy'), split into the windows before it, the
-- damage, and what answers it.
splitDamage :: [Message] -> ([Message], [Message], [Message])
splitDamage msgs = (before, now, after)
 where
  (before, rest) = span isWindow msgs
  (now, after) = splitAt 1 rest
  isWindow = \case
    CheckWindows _ -> True
    CardSpecific GameRef "playerAttackDamageBeforeTarget" _ -> True
    _ -> False

-- | The attack by the character on the enemy is resolved: the enemy's retaliate, if it is
-- still in play, then the window after the attack.
enemyRetaliates :: EnemyId -> CharacterId -> Message
enemyRetaliates enemy attacker = case enemy of
  EnemyVillainId vid -> VillainMessage vid (VillainRetaliates attacker)
  EnemyMinionId mid -> MinionMessage mid (MinionRetaliates attacker)
  EnemyAttachmentId _ -> CheckWindows []

-- | Retaliate's damage to the character attacking: an identity takes it as damage from
-- anything but an attack does.
retaliateDamage :: Source -> CharacterId -> Natural -> [Message]
retaliateDamage source attacker n = case attacker of
  IdentityCharacter ident -> damageIdentity ident source (toDamage n FromRetaliate)
  AllyCharacter aid -> damageAlly aid source (toDamage n FromRetaliate)
  -- a minion made to attack another enemy (Moondragon)
  MinionCharacter mid -> [MinionMessage mid (MinionDamaged source (toDamage n FromRetaliate))]
  VillainCharacter vid -> [VillainMessage vid (VillainDamaged source (toDamage n FromRetaliate))]
  AttachmentCharacter _ -> []

-- | The messages a 'Simultaneously' holds, for the queue to look inside it.
simultaneousMessages :: Message -> Maybe [Message]
simultaneousMessages = \case
  Simultaneously msgs -> Just msgs
  _ -> Nothing

simultaneously :: [Message] -> Message
simultaneously = Simultaneously

-- | "If this attack deals damage to a character", as a boost ability that lasts the
-- activation sees it: the attacking enemy and the character, as the attack's damage is
-- dealt. Damage a boost ability deals is not the attack's.
attackDamages :: Message -> Maybe (EnemyId, CharacterId)
attackDamages = \case
  CheckWindows [Window After (EnemyAttacksAndDamages enemy character _)] -> Just (enemy, character)
  _ -> Nothing

-- | The damage an enemy's attack dealt a character, as it is dealt.
attackDamageWindow :: Message -> Maybe (EnemyId, CharacterId, Natural)
attackDamageWindow = \case
  CheckWindows [Window After (EnemyAttacksAndDamages enemy character n)] -> Just (enemy, character, n)
  _ -> Nothing

-- | The character's own ref, as status cards are given by.
characterRef :: CharacterId -> Target
characterRef = \case
  IdentityCharacter i -> IdentityRef i
  AllyCharacter a -> AllyRef a
  VillainCharacter v -> VillainRef v
  MinionCharacter m -> MinionRef m
  AttachmentCharacter a -> AttachmentRef a

-- | A step of the card's own, for it to answer as @CardSpecific@.
cardSpecific :: (IsRef a, ToJSON v) => a -> Text -> v -> Message
cardSpecific a key v = CardSpecific (toTarget a) key (toJSON v)

data FinishedStatus
  = Won
  | Lost
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data DefensePriority
  = AnyDefense
  | AllyIfAble
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Message
  = ResolveWhenDefeated Message
  | WithSimultaneousAttackContext Int Message
  | PrepareEnemyDamageGroup [Message] Int
  | TakeEnemyDamageGroup [Message] Int [Message]
  | ResolveEnemyDamageGroup [Message] Int [Message]
  | PrepareEnemyDamage Message
  | ResolveEnemyDamage Message
  | StartGame
  | StartScenario
  | BeginPhase Phase
  | EndPhase Phase
  | PassFirstPlayer
  | -- | a villain phase step begins: does nothing, the log tells it
    VillainPhaseStep VillainPhaseStep
  | RemovePlayerCardFromHeldZones CardId
  | PlaceCharacterThreat CharacterId Natural
  | RemoveCharacterThreat CharacterId Natural
  | MoveCharacterThreatToMainScheme CharacterId
  | EndRound
  | BeginRound
  | AccelerateMainScheme
  | -- | an acceleration token would be placed on the main scheme: a
    -- 'RedirectAccelerationTokens' on it places it on another instead
    PlaceAccelerationToken MainSchemeId
  | IdentityEndedTurn IdentityId
  | AddVillain CardCode
  | AddMainScheme CardCode
  | -- | the main scheme leaves the game, the one given taking its place in its area
    ReplaceMainScheme MainSchemeId CardCode
  | SetupMainScheme
  | SetPlayerOrder [IdentityId]
  | -- | the rest of setup, once the first player is chosen: hands, mulligans and the
    -- player cards' setup abilities, in player order, then the first round
    SetupPlayers
  | -- | whose turn it is, or whom the villain phase is resolving for: "you" on cards
    SetActivePlayer IdentityId
  | -- | the response window, offered to these players in turn until all pass
    CheckWindowsFor [IdentityId] [Window]
  | IdentityMessage IdentityId IdentityMessage
  | VillainMessage VillainId VillainMessage
  | EventMessage EventId EventMessage
  | EffectMessage EffectId EffectMessage
  | AllyMessage AllyId AllyMessage
  | SupportMessage SupportId SupportMessage
  | UpgradeMessage UpgradeId UpgradeMessage
  | MainSchemeMessage MainSchemeId MainSchemeMessage
  | SideSchemeMessage SideSchemeId SideSchemeMessage
  | TreacheryMessage TreacheryId TreacheryMessage
  | ObligationMessage ObligationId ObligationMessage
  | MinionMessage MinionId MinionMessage
  | AttachmentMessage AttachmentId AttachmentMessage
  | Ask IdentityId Question
  | UsedAbility IdentityId Ability
  | RanAbility IdentityId Target Natural [WindowType] Payment
  | SearchFoundCards Target [Card]
  | DiscardTopOfEncounterDeck Natural (Maybe Target)
  | WithDiscarded Target FromZone [Card]
  | WithChosen Target FromZone [Card]
  | -- | one nested normal card play, with a reduction scoped to this exact card
    PlayCardWithReduction IdentityId PlayerCard Natural
  | PlayCardIgnoringResourceCost IdentityId PlayerCard
  | SetActiveCost ActiveCost
  | CreatedActiveCost ActiveCostId
  | DisableActiveCost ActiveCostId
  | Spent ActiveCostId PlayerCard
  | SpentWithResources ActiveCostId PlayerCard [Resource] [Resource]
  | Paid Payment
  | -- | another player is invited to contribute resources to an alliance card
    ContributeResources ActiveCostId IdentityId
  | -- | newly contributed wild resources are declared by their contributor
    DeclareContributionWilds ActiveCostId IdentityId Natural
  | -- | a player pays a non-resource part of an alliance cost with their own cards
    ContributeCost ActiveCostId IdentityId Cost
  | -- | obtain every contributor's consent before non-resource costs are committed
    PrepareAllianceCosts ActiveCostId [(IdentityId, Cost)] [Cost]
  | OfferAllianceCost ActiveCostId IdentityId Cost [(IdentityId, Cost)] [Cost]
  | -- | hold an event facedown on an ally, playable as if in its controller's hand
    AttachPlayableEventToAlly AllyId PlayerCard
  | AttachTreacheryToAlly AllyId EncounterCard
  | AttachPlayerCardToMinion MinionId PlayerCard
  | DivideAttackDamage IdentityId Source Damage
  | AssignEvenDamage IdentityId Source Damage Natural [(CharacterId, Natural)] [CharacterId]
  | IndirectDamageAndExhaust IdentityId Source Damage [(CharacterId, Natural)]
  | ExhaustIfDamaged CharacterId Natural
  | ReserveAllyHeldCard AllyId PlayerCard
  | -- | the nth wild resource paid toward the active cost is declared as this type
    DeclaredWild Natural Resource
  | FinishedPayment ActiveCostId
  | -- | which generated resources actually paid the final modified resource cost
    AllocatedPayment ActiveCostId [Resource]
  | -- | one part of the cost is paid, as one more ally exhausted for "exhaust any
    -- number of allies"
    PayCost ActiveCostId Cost
  | -- | the resources are paid and the rest of the cost has been: the card is played,
    -- or the ability resolves
    PaidCosts ActiveCostId
  | -- | the cost cannot be paid after all: what was spent goes back to hand
    AbortPayment ActiveCostId
  | -- | the card enters play under the player: played (True), opening the windows after
    -- it is played, or only put into play (False)
    PutCardIntoPlay IdentityId PlayerCard Payment (Maybe Window) Bool
  | -- | the effect of an event its player played, which a stun or confusion replaces as
    -- it would resolve and a cancel cancels
    ResolveEventEffect IdentityId Message
  | CheckWindows [Window]
  | EndCheckWindows
  | DealBoost Target
  | -- | The ally's interrupt has finished; perform its departure once, then respond.
    ResolveAllyLeavingPlay IdentityId AllyId CardDef Message
  | PrepareAllyDepartureByDefeat AllyId Message
  | -- | Perform a minion departure only if no interrupt replaced it.
    ResolveMinionLeavingPlay MinionId Message
  | -- | Recalculate threat removal after the thwart interrupt resolves.
    ResolveThreatRemoval Bool Bool IdentityId Target Source Natural
  | -- | Calculate a divided thwart total after its interrupt.
    PrepareDividedThreat IdentityId Source Bool Natural SchemeMatcher
  | -- | Move the top encounter card into a player's deck, preserving its card id.
    PutTopEncounterCardInPlayerDeck IdentityId
  | FlipBoostCards Target
  | PutBoostIntoPlay Target IdentityId
  | -- | "Boost: reveal this card"
    RevealBoost Target IdentityId
  | DealEncounterCard IdentityId
  | RevealDealtEncounterCards
  | GainSurge Target
  | Surge IdentityId
  | DrawAndRevealEncounterCard IdentityId
  | DiscardedCard Card
  | DeclareDefense IdentityId EnemyId DefensePriority
  | -- | the defender is declared, the players who declined to defend not asked again
    DeclareDefenseWithout IdentityId EnemyId DefensePriority [IdentityId]
  | RemoveFromPlay Target
  | -- | A player card leaves after its would-discard interrupts, before its destination.
    RemoveFromPlayAfterWindow Target
  | RemoveFromGame Target
  | CreatedEffect CardDef Source EntityMatcher
  | DisabledEffect EffectId
  | RevealEncounterCard IdentityId EncounterCard
  | RevealEncounterCardFrom RevealSource IdentityId EncounterCard
  | RevealedEncounterCard IdentityId EncounterCard
  | RevealedEncounterCardFrom RevealSource IdentityId EncounterCard
  | RevealedEncounterCardWithoutWhenRevealed IdentityId EncounterCard
  | RevealedEncounterCardWithoutWhenRevealedFrom RevealSource IdentityId EncounterCard
  | RevealBoostCard EncounterCard EnemyId
  | -- | Turn the card faceup, preserving its icons and windows but cancel its boost ability.
    RevealBoostCardWithoutAbility EncounterCard EnemyId
  | BoostCardResolved EnemyId EncounterCard
  | ConsumeBoostCard EncounterCard
  | -- | the main scheme is completed
    AdvanceMainScheme MainSchemeId
  | -- | the main scheme gives way to the scenario's next stage
    NextMainScheme MainSchemeId
  | GameOver FinishedStatus
  | UpgradeRemoved UpgradeId
  | AttachmentRemoved AttachmentId
  | FocusCards [Card]
  | -- | the cards are shown to the player only: "look at"
    ShowCardsTo IdentityId [Card]
  | HideCardsFrom IdentityId
  | UnfocusCards
  | SearchForAndRevealScheme CardDef
  | -- | the first card matching in the encounter deck or its discard pile is revealed
    -- for the player, then the deck is shuffled
    SearchForAndRevealEncounterCard IdentityId CardMatcher
  | ShuffleEncounterDeck
  | ReturnToHand Target
  | SetAside [Card]
  | SwapWeatherSupport IdentityId (Maybe SupportId) PlayerCard Bool
  | ResolveCurrentWeatherSpecial IdentityId
  | ResolveWeatherSpecial IdentityId SupportId
  | WeatherSpecialFinished IdentityId SupportId
  | ShuffleIntoEncounterDeck [EncounterCard]
  | -- | An ability makes a friendly hero or ally attack an identity (Old Rivals).
    FriendlyCharacterAttacks CharacterId IdentityId
  | FriendlyAttackLands CharacterId IdentityId
  | FriendlyAttackResolved CharacterId IdentityId Bool
  | FriendlyAttackConsequential AllyId
  | ClearBoosts
  | ClearRemoved
  | CheckPayment ActiveCostId
  | Boost Message
  | RevealedAsBoost Target EnemyId
  | ChoseEnemy EnemyId Target
  | ChosePlayer IdentityId Target
  | ChoseUpgrade UpgradeId Target
  | DiscardUntil FromZone CardMatcher Target
  | WithDiscardedMatch Target FromZone Card
  | -- | the boost icons on each card are counted ("Chaos Control"): each card's count is
    -- its icons, then the window to counting them, whose interrupts change the count
    -- queued; the target is told the counts with the value it asked with
    CountBoostIcons Target Value [EncounterCard]
  | WithBoostIcons Target Value [(EncounterCard, Natural)]
  | -- | setup puts the first main scheme into play: its 1A "Setup", then its 1B "When
    -- Revealed", then the villain's "When Revealed"
    AddSetupMainScheme CardCode
  | EnvironmentMessage EnvironmentId EnvironmentMessage
  | -- | the top card of the player's deck is put into play facedown, engaged with them,
    -- as a Drone minion
    PutTopOfDeckIntoPlayAsDrone IdentityId
  -- Hela (and Enchantress)
  | -- | the ally card is put into play as a minion with a blank text box, engaged with
    -- the player (Fallen Warrior), out of where it was; unless a matching unique card in
    -- play keeps it out (RR Unique)
    PutAllyIntoPlayAsMinion IdentityId PlayerCard
  | -- | the ally in play is treated as a minion engaged with its controller (Beguiled):
    -- it does not leave play; its damage and status cards stay, what is attached to it
    -- is discarded
    AllyBecomesMinion AllyId
  | -- | the minion an ally was treated as is that ally again, under its owner's control,
    -- its damage kept (its Fallen Warrior or Beguiled gone first)
    MinionBecomesAlly MinionId
  | -- | treat the same physical minion as a blank controlled ally
    TakeControlOfMinion IdentityId MinionId
  | -- | the continuous control effect ends while its host remains in play
    RestoreControlledMinion AllyId
  | -- | an encounter card put into play without being revealed: its "When Revealed"
    -- abilities and surge do not resolve
    PutEncounterCardIntoPlay IdentityId EncounterCard
  | -- | the encounter card enters play with what follows, unless a matching unique card is
    -- in play: then it is discarded instead, and if it was being revealed its player is
    -- dealt a facedown encounter card (RR Unique Icon)
    EntersPlayUnlessUnique IdentityId Bool Target [Message]
  | -- | the card is kept out of play by a matching unique card
    KeptOutByUnique Target
  | -- | a step only one card knows, as ArkhamHorror's @ScenarioSpecific@: the card the
    -- target names reads the key and decodes the value; the engine passes it by
    CardSpecific Target Text Value
  | -- | the cards, taken from wherever they are in the encounter deck, go on top of it,
    -- the first given on top
    PutOnTopOfEncounterDeck [EncounterCard]
  | -- | the cards, taken from wherever they are in the encounter deck, go to its bottom,
    -- the last given bottommost
    PutOnBottomOfEncounterDeck [EncounterCard]
  | -- | the top cards of the encounter deck are looked at, staying where they are: the
    -- target is answered with @SearchFoundCards@
    LookAtTopOfEncounterDeck Natural Target
  | -- | the first card matching in the encounter deck or its discard pile is put into
    -- play for the player, then the deck is shuffled
    SearchForAndPutIntoPlayEncounterCard IdentityId CardMatcher
  | -- | as 'SearchForAndPutIntoPlayEncounterCard', the set-aside area searched too
    -- ("search the encounter deck, discard pile, and set-aside area", Enchantress)
    SearchEverywhereAndPutIntoPlayEncounterCard IdentityId CardMatcher
  | SearchEverywhereAndRevealEncounterCard IdentityId CardMatcher
  | -- | cards are discarded from the top of the encounter deck until one matches, which
    -- is put into play for the player (Ebony Maw's Spells); a deck run out first is
    -- fulfilled, nothing put into play
    DiscardUntilIntoPlay IdentityId CardMatcher
  | -- | a card matching is found in the encounter deck or discard pile and dealt to the
    -- player facedown
    SearchForAndDealEncounterCard IdentityId CardMatcher
  | -- | the encounter card, found, is dealt to the player facedown
    DealFoundEncounterCard IdentityId EncounterCard
  | -- | an event's ability resolved for the player without the event being played, as an
    -- Invocation card's "Special": nothing that answers playing an event sees it
    ResolveEventWithoutPlaying IdentityId PlayerCard
  | -- | a character, by its own ref (identity, ally, villain or minion), is given a status
    -- card it does not have, or has one taken off it
    GiveStatus Target StatusCard
  | RemoveStatus Target StatusCard
  | -- | counters on an environment, main scheme or attachment, by its ref: what they
    -- count is the card's to say
    PlaceCounters Target Natural
  | RemoveCounters Target Natural
  | -- | each player's obligations are taken out of the encounter deck and the game, as a
    -- setup says
    RemovePlayerObligationsFromGame
  | -- | indirect damage the player has still to assign among the characters they
    -- control (the damage carrying how much is left, and its kind: an attack's is
    -- attack damage), and what they have assigned so far: dealt at once when all is
    -- assigned
    AssignIndirectDamage IdentityId Source Damage [(CharacterId, Natural)]
  | -- | indirect damage "among players": the group divides it among the friendly
    -- characters in play, the first player deciding (RR Indirect Damage)
    AssignIndirectDamageAmongPlayers Source Natural [(CharacterId, Natural)]
  | -- | damage the player has still to divide among the enemies matching (the damage
    -- carrying how much is left), and the shares assigned so far
    AssignDividedDamage IdentityId Source Damage EnemyMatcher [(EnemyId, Natural)]
  | -- | threat the player has still to divide among the schemes matching, by a basic
    -- thwart or not, whether they may stop short ("up to"), and the shares assigned so far
    AssignDividedThreat IdentityId Source Bool Bool Natural SchemeMatcher [(SchemeId, Natural)]
  | -- | one effect on several game elements at once, as damage dealt to each of several
    -- enemies: each message is run before anything any of them leads to
    Simultaneously [Message]
  | -- | a new game area, the players given leaving theirs for it
    CreateArea AreaId [IdentityId]
  | MovePlayerToArea IdentityId AreaId
  | -- | what is in the first area joins the second, and the first is gone
    CombineAreas AreaId AreaId
  | -- | the villain is the one that activates in its area, and "the villain" there
    SetActiveVillain VillainId
  | -- | the side scheme is the villain's own: the villain schemes onto it, it cannot
    -- leave play while the villain is in play, and it is removed from the game with him
    SetSignatureScheme VillainId SideSchemeId
  | -- | a deck of its own is made from the cards, shuffled: a villain's encounter deck,
    -- a named deck, the side-scheme deck
    CreateDeck DeckKey [EncounterCard]
  | RemoveDeck DeckKey
  | -- | the encounter-deck message run against the deck given in place of the area's
    -- encounter deck
    UsingDeck DeckKey Message
  | -- | the top card of the encounter deck (or the deck given by 'UsingDeck') is put into
    -- play for the player, not revealed: an Infinity Stone (The Mad Titan's Shadow)
    PutTopOfEncounterDeckIntoPlay IdentityId
  | -- | the encounter deck's discard pile (or that of the deck given by 'UsingDeck') is
    -- shuffled back into it
    ShuffleEncounterDiscardIntoDeck
  | -- | a villain is defeated for good: the scenario says what that does, by default
    -- the players win with no villain left, and otherwise he leaves play
    ResolveVillainDefeat VillainId
  | -- | activations initiated during another, each by its villain, following it once it
    -- has fully resolved, in the order the first player chooses (RR "Activation")
    PendingActivations [(VillainId, [Message])]
  | -- | the attachment card is put into play attached to the enemy, from wherever it
    -- was, without being revealed
    AttachEncounterCard EncounterCard EnemyId
  | -- | each player in the area has been defeated, with players left elsewhere: the
    -- scenario says what that does, by default nothing
    AreaDefeated AreaId
  | -- | the messages are to run at the end of the phase under way: "at the end of the
    -- phase"
    AtEndOfPhase [Message]
  | -- | copies of a unique card in the area, as two areas come together, are discarded
    -- down to one, the first player choosing which stays
    DiscardDuplicateUniques AreaId
  | -- | a part of the scenario's Campaign Instructions, in a campaign game: the
    -- scenario says what it does ('Marvel.Campaign.Steps' by default), and may add its own
    CampaignStep CampaignStepKind
  | -- | what the game writes in the campaign log it was dealt with
    RecordInCampaignLog CampaignRecord
  | -- | an obligation drawn from the player's deck goes into play in their play area,
    -- as an expert campaign's do
    PutObligationIntoPlay IdentityId PlayerCard
  -- The Galaxy's Most Wanted
  | -- | the card leaves play for the victory display instead of its discard pile, what is
    -- attached to it discarded as it would be (RR Victory X)
    AddToVictoryDisplay Target
  | -- | the card is defeated and leaves play: to the victory display if it has victory
    -- X, its discard pile otherwise
    DefeatedLeavesPlay Target
  | -- | the player is resolving a card with peril, or no one is: while one is, the
    -- others cannot play cards or trigger abilities (RR Peril)
    SetPeril (Maybe IdentityId)
  | -- | the boost cards turned faceup for the enemy's activation have their icons
    -- counted, each gaining one for each amplify icon in play (RR Amplify Icon), as
    -- 'CountBoostIcons' with no value
    CountActivationBoostIcons Target [EncounterCard]
  | -- | the encounter cards with the setup keyword go into play (RR Appendix II, step 11)
    PutSetupEncounterCardsIntoPlay
  -- Infiltrate the Museum
  | -- | the card goes faceup into The Collection, the shared out-of-play area of the
    -- scenario, from wherever it is: a card in play leaves play for it, not discarded
    PlaceInCollection Card
  -- The Mad Titan's Shadow: Loki
  | -- | "swap <the villain> with a random set-aside <villain>": the scenario picks one of
    -- the same title set aside, and the villain is replaced by it ('ReplaceVillain');
    -- with none, nothing (RR "Swap": it cannot be completed)
    SwapVillain VillainId
  | -- | the villain card in play is replaced by the one given, the same villain id:
    -- what was on him carries over ('Marvel.Villain.Types.carryOver'); swapped, his hit
    -- point dial stays, advanced, the new one enters play at its own hit points
    ReplaceVillain VillainId CardCode ReplaceKind
  | -- | a card out of play goes straight to the victory display (a defeated villain)
    AddCardToVictoryDisplay EncounterCard
  | -- | the ability of the source, paid for, has resolved all it does: an attack it made
    -- has ended (Master of Illusions' prevention lasts until then)
    AbilityResolved Source
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | How a villain card is replaced: swapped (RR "Swap", the hit point dial kept) or
-- advanced to (it enters play as a next stage would).
data ReplaceKind = Swapped | Advanced
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)


-- | The actionable message behind an enemy-damage continuation. Queue edits see
-- the original message, while the continuation retains which windows have run.
queuedMessage :: Message -> Message
queuedMessage = \case
  PrepareEnemyDamage original -> queuedMessage original
  ResolveEnemyDamage original -> queuedMessage original
  WithSimultaneousAttackContext _ original -> queuedMessage original
  original -> original

-- | Preserve the continuation for modified damage, but run replacements of other
-- kinds normally. Canceling the raw damage therefore cancels its continuation.
rewrapQueuedMessages :: Message -> [Message] -> [Message]
rewrapQueuedMessages (WithSimultaneousAttackContext cursor original) = map (WithSimultaneousAttackContext cursor) . rewrapQueuedMessages original
rewrapQueuedMessages wrapper = map $ case wrapper of
  PrepareEnemyDamage _ -> wrap PrepareEnemyDamage
  ResolveEnemyDamage _ -> wrap ResolveEnemyDamage
  _ -> id
 where
  wrap constructor message = case enemyDamageMessage message of
    Just _ -> constructor message
    Nothing -> message

enemyDamageMessage :: Message -> Maybe (EnemyId, Source, Damage)
enemyDamageMessage = \case
  WithSimultaneousAttackContext _ original -> enemyDamageMessage original
  VillainMessage vid (VillainDamaged source damage) -> Just (EnemyVillainId vid, source, damage)
  MinionMessage mid (MinionDamaged source damage) -> Just (EnemyMinionId mid, source, damage)
  _ -> Nothing




-- | Focused child edits retain each enclosing message and the original order.
-- A replacement may remove a child or insert several messages in its position.
queuedChildEdits :: Message -> [(Message, [Message] -> [Message])]
queuedChildEdits message = case message of
  WithSimultaneousAttackContext cursor original -> [(original, map (WithSimultaneousAttackContext cursor))]
  PrepareAllyDepartureByDefeat _ departure ->
    [(departure, map (\replacement -> if replacement == departure then message else replacement))]
  PrepareEnemyDamageGroup messages cursor -> group PrepareEnemyDamageGroup messages cursor
  TakeEnemyDamageGroup messages cursor deferred -> remaining False TakeEnemyDamageGroup messages cursor deferred
  ResolveEnemyDamageGroup messages cursor deferred -> remaining True ResolveEnemyDamageGroup messages cursor deferred
  Simultaneously messages ->
    [(child, \replacement -> [simultaneously $ take index messages <> replacement <> drop (index + 1) messages]) | (index, child) <- zip [0..] messages]
  _ -> []
 where
  remaining resolving constructor messages cursor deferred =
    [(messages !! index, \replacement ->
      if resolving && index == cursor && not (case replacement of [raw] -> isJust (enemyDamageMessage raw); _ -> False)
        then replacement <> [TakeEnemyDamageGroup (take index messages <> drop (index + 1) messages) cursor deferred]
        else [constructor (take index messages <> replacement <> drop (index + 1) messages) cursor deferred]) | index <- [cursor .. length messages - 1]]
    <> [(child, \replacement -> [constructor messages cursor (take index deferred <> replacement <> drop (index + 1) deferred)]) | (index, child) <- zip [0..] deferred]
  group constructor messages cursor =
    let focus = cursor - 1
        indices = [focus | focus >= 0, focus < length messages] <> [index | index <- [0 .. length messages - 1], index /= focus]
     in [(messages !! index, \replacement ->
          let next = if index < cursor then cursor + length replacement - 1 else cursor
           in [constructor (take index messages <> replacement <> drop (index + 1) messages) next]) | index <- indices]

queuedMessageLeaves :: Message -> [Message]
queuedMessageLeaves message = case queuedChildEdits message of
  [] -> [queuedMessage message]
  children -> concatMap (queuedMessageLeaves . fst) children
