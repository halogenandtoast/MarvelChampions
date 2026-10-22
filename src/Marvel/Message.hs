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
import Marvel.Card.Types
import Marvel.Game.Source
import Marvel.Attack (sourceEnemy)
import Marvel.Damage (Damage (..), DamageSource (FromAttack))
import Marvel.Id
import Marvel.Identity.Types
import Marvel.Matchers.Types
import Marvel.Payment.Types
import Marvel.Phase
import Marvel.Projection
import {-# SOURCE #-} Marvel.Question
import Marvel.Queue
import Marvel.Ref
import Marvel.Status as X
import Marvel.Window.Types (ThreatSource (ThreatFromAbility), Window (..), WindowTiming (After, When, Would), WindowType (EnemyAttacksAndDamages, IdentityTakeDamage, ThreatPlaced))

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
    <> [ case scheme of
          SchemeMainSchemeId sid -> MainSchemeMessage sid (MainSchemePlaceThreat n)
          SchemeSideSchemeId sid -> SideSchemeMessage sid (SideSchemePlaceThreat n)
       ]

-- | Cancel a treachery's "When Revealed" effects as it is revealed: none of them resolves,
-- the surge it has or would gain included (surge is a "When Revealed" ability), and it is
-- discarded as it would be.
cancelWhenRevealed :: (HasQueue m) => TreacheryId -> m ()
cancelWhenRevealed tid = withQueue_ $ mapMaybe \case
  TreacheryMessage tid' msg | tid' == tid -> case msg of
    RevealTreachery _ -> Nothing
    CheckTreacheryCondition _ -> Nothing
    ResolvedTreachery _ -> Just $ RemoveFromPlay (TreacheryRef tid)
  msg -> Just msg

-- | The minion engages the player, leaving the player it was engaged with.
engageMinion :: MinionId -> IdentityId -> [Message]
engageMinion minion ident =
  [IdentityMessage ident (MinionEngaged minion), MinionMessage minion (MinionEngagedIdentity ident)]

-- | Add to the power of the basic attack or thwart the player is making, as an
-- interrupt to making it.
addToBasicPower :: (HasQueue m) => IdentityId -> Natural -> m ()
addToBasicPower ident n = replaceMatchingMessage raise \case
  IdentityMessage ident' (SideMessage (BasicAttack _)) -> ident == ident'
  IdentityMessage ident' (SideMessage (BasicThwart _)) -> ident == ident'
  _ -> False
 where
  raise = \case
    IdentityMessage i (SideMessage (BasicAttack x)) -> [IdentityMessage i (SideMessage (BasicAttack (x + n)))]
    IdentityMessage i (SideMessage (BasicThwart x)) -> [IdentityMessage i (SideMessage (BasicThwart (x + n)))]
    other -> [other]

-- | After an enemy's attack deals damage to a character, the window for it.
attackDealtDamage :: Source -> Damage -> CharacterId -> [Message]
attackDealtDamage source damage character =
  [ CheckWindows [Window After $ EnemyAttacksAndDamages enemy character]
  | damageSource damage == FromAttack
  , damageAmount damage > 0
  , Just enemy <- [sourceEnemy source]
  ]

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
  | EndRound
  | BeginRound
  | AccelerateMainScheme
  | AddAccelerationToken
  | IdentityEndedTurn IdentityId
  | AddVillain CardCode
  | AddMainScheme CardCode
  | ReplaceMainScheme CardCode
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
  | FinishedPayment ActiveCostId
  | -- | the cost cannot be paid after all: what was spent goes back to hand
    AbortPayment ActiveCostId
  | PutCardIntoPlay IdentityId PlayerCard Payment (Maybe Window)
  | CheckWindows [Window]
  | EndCheckWindows
  | DealBoost Target
  | FlipBoostCards Target
  | PutBoostIntoPlay Target IdentityId
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
  | AdvanceMainScheme
  | NextMainScheme
  | GameOver FinishedStatus
  | UpgradeRemoved UpgradeId
  | AttachmentRemoved AttachmentId
  | FocusCards [Card]
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
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
