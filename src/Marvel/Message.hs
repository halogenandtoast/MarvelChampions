module Marvel.Message (
  module Marvel.Message,
  module X,
) where

import Marvel.Prelude

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
import Marvel.Damage (Damage (..), DamageSource (FromAttack, FromOverkill, FromRetaliate), toDamage)
import Marvel.Id
import Marvel.Identity.Types
import Marvel.Matchers.Types
import Marvel.Payment.Types
import Marvel.Resource.Types (Resource)
import Marvel.Phase
import Marvel.Projection
import {-# SOURCE #-} Marvel.Question
import Marvel.Queue
import Marvel.Ref
import Marvel.Status as X
import Marvel.Window.Types (ThreatSource (ThreatFromAbility), Window (..), WindowTiming (After, When, Would), WindowType (DefeatedMinion, EnemyAttack, EnemyAttacksAndDamages, IdentityTakeDamage, ThreatPlaced))

class RunMessage a where
  runMessage :: (HasCallStack, MonadThrow m, MonadRandom m, HasGame m, HasQueue m, Projection m PlayerIdentity) => Message -> a -> m a

-- | Damage to a hero or alter-ego from anything but an attack: first the window for
-- what answers "when you would take damage", then the damage.
damageIdentity :: IdentityId -> Source -> Damage -> [Message]
damageIdentity ident source damage =
  [ CheckWindows [Window When $ IdentityTakeDamage ident damage]
  , IdentityMessage ident $ IdentityDamaged source damage
  ]

-- | Prevent some of the damage the identity would take, as an interrupt to "when you
-- would take damage" does: all of it prevented, the damage is not taken at all.
preventDamage :: (HasQueue m) => IdentityId -> Natural -> m ()
preventDamage ident n = replaceMatchingMessage prevent \case
  IdentityMessage ident' (IdentityDamaged _ _) -> ident' == ident
  _ -> False
 where
  prevent = \case
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
cancelWhenRevealed :: (HasQueue m) => TreacheryId -> m ()
cancelWhenRevealed tid = withQueue_ $ mapMaybe \case
  TreacheryMessage tid' msg | tid' == tid -> case msg of
    RevealTreachery _ -> Nothing
    TreacheryIncite -> Nothing
    CheckTreacheryCondition _ -> Nothing
    ResolvedTreachery _ -> Just $ RemoveFromPlay (TreacheryRef tid)
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
    _ -> False
  dropOverkill = \case
    VillainMessage _ (VillainDamaged _ d) : rest | damageSource d == FromOverkill -> rest
    rest -> rest
  cancelAfter rest = case break isAfter rest of
    (pre, _ : rest') -> pre <> rest'
    _ -> rest
  isAfter = \case
    CheckWindows [Window After (DefeatedMinion mid' _ _ _)] -> mid' == mid
    _ -> False

-- | The minion engages the player, leaving the player it was engaged with.
engageMinion :: MinionId -> IdentityId -> [Message]
engageMinion minion ident =
  [IdentityMessage ident (MinionEngaged minion), MinionMessage minion (MinionEngagedIdentity ident)]

-- | Add to the power of the basic attack, thwart or defense the player is making, as an
-- interrupt to making it.
addToBasicPower :: (HasQueue m) => IdentityId -> Natural -> m ()
addToBasicPower ident n = replaceMatchingMessage raise \case
  IdentityMessage ident' (SideMessage (BasicAttack _)) -> ident == ident'
  IdentityMessage ident' (SideMessage (BasicThwart _)) -> ident == ident'
  IdentityMessage ident' (SideMessage (DefenseApplied _ _)) -> ident == ident'
  _ -> False
 where
  raise = \case
    IdentityMessage i (SideMessage (DefenseApplied x e)) -> [IdentityMessage i (SideMessage (DefenseApplied (x + n) e))]
    IdentityMessage i (SideMessage (BasicAttack x)) -> [IdentityMessage i (SideMessage (BasicAttack (x + n)))]
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

-- | After an enemy's attack deals damage to a character, the window for it.
attackDealtDamage :: Source -> Damage -> CharacterId -> [Message]
attackDealtDamage source damage character =
  [ CheckWindows [Window After $ EnemyAttacksAndDamages enemy character (damageAmount damage)]
  | damageSource damage == FromAttack
  , damageAmount damage > 0
  , Just enemy <- [sourceEnemy source]
  ]

-- | Damage dealt to each of several targets as one effect, each given as the messages
-- dealing it ('DamageEnemy'): the interrupts to all of it, then all of the damage at
-- once, then what answers it.
simultaneousDamage :: [[Message]] -> [Message]
simultaneousDamage dealt = concat befores <> [Simultaneously (concat damage) | notNull (concat damage)] <> concat afters
 where
  (befores, damage, afters) = unzip3 $ map splitDamage dealt

-- | The messages dealing damage ('DamageEnemy'), split into the windows before it, the
-- damage, and what answers it.
splitDamage :: [Message] -> ([Message], [Message], [Message])
splitDamage msgs = (before, now, after)
 where
  (before, rest) = span isWindow msgs
  (now, after) = splitAt 1 rest
  isWindow = \case
    CheckWindows _ -> True
    _ -> False

-- | The attack by the character on the enemy is resolved: the enemy's retaliate, if it is
-- still in play, then the window after the attack.
enemyRetaliates :: EnemyId -> CharacterId -> Message
enemyRetaliates enemy attacker = case enemy of
  EnemyVillainId vid -> VillainMessage vid (VillainRetaliates attacker)
  EnemyMinionId mid -> MinionMessage mid (MinionRetaliates attacker)

-- | Retaliate's damage to the character attacking: an identity takes it as damage from
-- anything but an attack does.
retaliateDamage :: Source -> CharacterId -> Natural -> [Message]
retaliateDamage source attacker n = case attacker of
  IdentityCharacter ident -> damageIdentity ident source (toDamage n FromRetaliate)
  AllyCharacter aid -> [AllyMessage aid (AllyDamaged source (toDamage n FromRetaliate))]
  _ -> []

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
  = StartGame
  | StartScenario
  | BeginPhase Phase
  | EndPhase Phase
  | PassFirstPlayer
  | -- | a villain phase step begins: does nothing, the log tells it
    VillainPhaseStep VillainPhaseStep
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
  | SetActiveCost ActiveCost
  | CreatedActiveCost ActiveCostId
  | DisableActiveCost ActiveCostId
  | Spent ActiveCostId PlayerCard
  | Paid Payment
  | -- | the nth wild resource paid toward the active cost is declared as this type
    DeclaredWild Natural Resource
  | FinishedPayment ActiveCostId
  | -- | one part of the cost is paid, as one more ally exhausted for "exhaust any
    -- number of allies"
    PayCost ActiveCostId Cost
  | -- | the resources are paid and the rest of the cost has been: the card is played,
    -- or the ability resolves
    PaidCosts ActiveCostId
  | -- | the cost cannot be paid after all: what was spent goes back to hand
    AbortPayment ActiveCostId
  | PutCardIntoPlay IdentityId PlayerCard Payment (Maybe Window)
  | -- | the effect of an event its player played, which a stun or confusion replaces as
    -- it would resolve and a cancel cancels
    ResolveEventEffect IdentityId Message
  | CheckWindows [Window]
  | EndCheckWindows
  | DealBoost Target
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
  | RemoveFromGame Target
  | CreatedEffect CardDef Source EntityMatcher
  | DisabledEffect EffectId
  | RevealEncounterCard IdentityId EncounterCard
  | RevealedEncounterCard IdentityId EncounterCard
  | RevealBoostCard EncounterCard EnemyId
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
  | ShuffleIntoEncounterDeck [EncounterCard]
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
  | -- | setup puts the first main scheme into play: its 1A "Setup", then its 1B "When
    -- Revealed", then the villain's "When Revealed"
    AddSetupMainScheme CardCode
  | EnvironmentMessage EnvironmentId EnvironmentMessage
  | -- | the top card of the player's deck is put into play facedown, engaged with them,
    -- as a Drone minion
    PutTopOfDeckIntoPlayAsDrone IdentityId
  | -- | an encounter card put into play without being revealed: its "When Revealed"
    -- abilities and surge do not resolve
    PutEncounterCardIntoPlay IdentityId EncounterCard
  | -- | a step only one card knows, as ArkhamHorror's @ScenarioSpecific@: the card the
    -- target names reads the key and decodes the value; the engine passes it by
    CardSpecific Target Text Value
  | -- | the cards, taken from wherever they are in the encounter deck, go on top of it,
    -- the first given on top
    PutOnTopOfEncounterDeck [EncounterCard]
  | -- | the top cards of the encounter deck are looked at, staying where they are: the
    -- target is answered with @SearchFoundCards@
    LookAtTopOfEncounterDeck Natural Target
  | -- | the first card matching in the encounter deck or its discard pile is put into
    -- play for the player, then the deck is shuffled
    SearchForAndPutIntoPlayEncounterCard IdentityId CardMatcher
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
    -- control, and what they have assigned so far: dealt at once when all is assigned
    AssignIndirectDamage IdentityId Source Natural [(CharacterId, Natural)]
  | -- | damage the player has still to divide among the enemies matching (the damage
    -- carrying how much is left), and the shares assigned so far
    AssignDividedDamage IdentityId Source Damage EnemyMatcher [(EnemyId, Natural)]
  | -- | threat the player has still to divide among the schemes matching, by a basic
    -- thwart or not, and the shares assigned so far
    AssignDividedThreat IdentityId Source Bool Natural SchemeMatcher [(SchemeId, Natural)]
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
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
