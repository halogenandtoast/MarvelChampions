module Marvel.Window.Types where

import Marvel.Prelude
import Marvel.Phase (Phase)
import Marvel.Resource.Types (Resource)

import {-# SOURCE #-} Marvel.Card.Def (CardDef, CardMatcher)
import {-# SOURCE #-} Marvel.Card.EncounterCard
import {-# SOURCE #-} Marvel.Card.PlayerCard.Types
import Marvel.Damage
import Marvel.Id
import Marvel.Matchers.Types
import Marvel.Ref (Source)

data WindowTiming = After | When | Would
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data RevealSource
  = RevealedFromEncounterDeck
  | RevealedFromVillain
  | RevealedFromDiscardPile
  | RevealedFromSetAside
  | RevealedFromRemovedFromGame
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data ThreatSource
  = ThreatFromVillain
  | ThreatFromMinion
  | ThreatFromAbility
  | ThreatFromAcceleration
  | AnyThreatSource
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data WindowMatcher
  = ResourcesGeneratedBy IdentityMatcher
  | TreacheryResolvedBy IdentityMatcher
  | HeroThwarted WindowTiming IdentityMatcher
  | IdentityDamaged WindowTiming IdentityMatcher
  | ThreatRemovedByThwart WindowTiming SourceMatcher SchemeMatcher
  | RemovesLastThreat IdentityMatcher
  | -- | "after you play <this card>": played, not only put into play
    PlayThis WindowTiming
  | -- | "after <this ally> enters play": played or put into play
    EntersPlayThis
  | WouldTakeDamage IdentityMatcher DamageSource GameValueMatcher
  | -- | an ally matching would take damage
    AllyWouldTakeDamage AllyMatcher
  | AllyWouldTakeDamageOf WindowTiming AllyMatcher DamageSource
  | CharacterWouldTakeAttackDamage CharacterMatcher
  | -- | an ally matching would take damage exactly equal to its remaining hit points
    AllyWouldTakeDamageEqualToRemainingHitPoints AllyMatcher
  | -- | after a player matching's hero thwarts and removes all threat from a scheme
    HeroThwartsAllThreat IdentityMatcher
  | EnemyWouldAttack EnemyMatcher IdentityMatcher
  | ThreatWouldBePlaced ThreatSource SchemeMatcher
  | EnemyAttacked WindowTiming EnemyMatcher IdentityMatcher
  | EnemyAttackedAndDamaged EnemyMatcher CharacterMatcher
  | IdentityAttacked WindowTiming IdentityMatcher EnemyMatcher
  | AllyThwarted WindowTiming AllyMatcher SchemeMatcher
  | AllyAttacked WindowTiming AllyMatcher EnemyMatcher
  | TreacheryRevealed WindowTiming TreacheryMatcher RevealSource
  | VillainRevealed WindowTiming VillainMatcher RevealSource
  | VillainDamaged WindowTiming VillainMatcher
  | -- | damage of the kind matching would be, or was, dealt to a villain matching
    VillainDamagedBy WindowTiming VillainMatcher DamageMatcher
  | -- | "when <the villain> would take damage" of the kind matching: damage he can take,
    -- not all of it prevented by his tough status card
    VillainWouldTakeDamage VillainMatcher DamageMatcher
  | VillainWouldTakeDamageFrom VillainMatcher DamageMatcher
  | VillainSchemed WindowTiming VillainMatcher
  | MinionDefeated WindowTiming MinionMatcher
  | MinionEntersPlay WindowTiming MinionMatcher
  | SideSchemeEntersPlay WindowTiming SideSchemeMatcher
  | PhaseBegins WindowTiming Phase
  | EnemyDefeatedWithUpgrade WindowTiming CardDef DamageMatcher
  | EnemyDefeated WindowTiming EnemyMatcher DamageMatcher
  | -- | after an enemy matching takes damage of the kind matching
    EnemyTookDamage EnemyMatcher DamageMatcher
  | IdentityChangedToForm WindowTiming IdentityMatcher
  | -- | "after you change form": a flip, or a change of an additional form (Spectrum's
    -- energy forms, MTS rulebook p.2)
    IdentityChangedAnyForm WindowTiming IdentityMatcher
  | -- | "after you change to this form": an energy form matching turned faceup
    EnergyFormChangedTo WindowTiming UpgradeMatcher
  | MassFormChangedTo WindowTiming UpgradeMatcher
  | IdentityDiscardsTough WindowTiming IdentityMatcher
  | VillainWouldActivate IdentityMatcher
  | IdentityAttackResolved IdentityMatcher
  | IdentityIgnoresKeyword WindowTiming IdentityMatcher MinionMatcher
  | IdentityIgnoresCrisis WindowTiming IdentityMatcher SideSchemeMatcher
  | MakesBasicAttack WindowTiming IdentityMatcher
  | -- | a player matching makes a basic attack against an enemy matching, its target chosen
    MakesBasicAttackAgainst WindowTiming IdentityMatcher EnemyMatcher
  | SideSchemeDefeated WindowTiming SideSchemeMatcher
  | HeroDefended WindowTiming IdentityMatcher EnemyMatcher
  | MakesBasicDefense WindowTiming IdentityMatcher EnemyMatcher
  | -- | revealed from the encounter deck, by any player
    EncounterCardRevealedFromDeck WindowTiming BasicEncounterCardMatcher
  | RoundEnds
  | IdentityWouldBeDefeated IdentityMatcher
  | TurnBegins IdentityMatcher
  | TurnBeginsAt WindowTiming IdentityMatcher
  | -- | a player matching plays an event matching
    PlaysEvent WindowTiming IdentityMatcher CardMatcher
  | PlaysEventPaidWith WindowTiming IdentityMatcher CardMatcher Resource
  | AllyDefeated WindowTiming AllyMatcher
  | -- | an ally matching is defeated by damage from an enemy's attack
    AllyDefeatedByAttack WindowTiming AllyMatcher
  | -- | an ally is defeated by damage from the attack of an enemy matching (Direct Assault)
    AllyDefeatedByAttackOf WindowTiming EnemyMatcher
  | AllyDefeatedByAttackOfEnemy WindowTiming AllyMatcher EnemyMatcher
  | -- | after an ally matching takes damage of this kind
    AllyTookDamageOf AllyMatcher DamageSource
  | -- | a boost card is turned face up for an enemy's activation
    BoostCardFlipped WindowTiming EnemyMatcher
  | -- | "you reveal": the encounter card is revealed for a player matching
    EncounterCardRevealedBy WindowTiming IdentityMatcher BasicEncounterCardMatcher
  | EncounterCardRevealedFromDeckBy WindowTiming IdentityMatcher BasicEncounterCardMatcher
  | -- | a player matching plays a card matching (paid for and put into play or resolved)
    PlaysCard WindowTiming IdentityMatcher CardMatcher
  | -- | A player matching played an upgrade onto an ally; target is snapshotted.
    PlaysUpgradeOnAlly WindowTiming IdentityMatcher
  | -- | after a player plays an ally matching, as it is in play (traits it gains count)
    PlaysAlly AllyMatcher
  | -- | "when you play a card from your hand": a player matching is about to pay for a
    -- card matching, from their hand, that costs resources
    PlaysCardFromHand IdentityMatcher CardMatcher
  | -- | "after you play <card> from your hand" (White Tiger): played, from hand
    PlayedFromHand IdentityMatcher CardMatcher
  | -- | a player matching resolves the ability of a card matching
    ResolvesAbility WindowTiming IdentityMatcher CardMatcher
  | -- | an interrupt or response event has fully resolved, distinguished by its copy
    ResolvesEventAbility WindowTiming IdentityMatcher CardMatcher
  | AllyLeavesPlay WindowTiming IdentityMatcher CardMatcher
  | MinionHasSchemed WindowTiming MinionMatcher
  | BoostCardTurnedFaceup WindowTiming BasicEncounterCardMatcher
  | -- | the boost icons on an encounter card would be, or are, counted
    BoostIconsCounted
  | -- | after a player matching takes damage of this kind from an enemy matching
    TookDamage IdentityMatcher DamageSource EnemyMatcher
  | -- | an ally a player matching controls is defeated
    AllyWasDefeated WindowTiming IdentityMatcher
  | SurgeWouldResolve
  | EngagesMinion WindowTiming IdentityMatcher MinionMatcher
  | MakesBasicThwart WindowTiming IdentityMatcher
  | WindowOneOf [WindowMatcher]
  | -- | the hero defended an attack and took no damage from it
    HeroDefendedWithoutDamage WindowTiming IdentityMatcher
  | -- | a player's turn ends: "when your turn ends"
    TurnEnds WindowTiming IdentityMatcher
  | -- | "after you defeat a side scheme"
    DefeatsSideScheme IdentityMatcher
  | -- | a minion matching is defeated by damage from a source matching
    MinionDefeatedBy WindowTiming MinionMatcher SourceMatcher
  | -- | after a side scheme matching is defeated, its last threat removed by a source
    -- matching
    SideSchemeDefeatedBy SideSchemeMatcher SourceMatcher
  | -- | "after you use your REC"
    UsesRecovery IdentityMatcher
  | -- | "after you ready <identity>": an exhausted identity matching is readied
    IdentityReadies IdentityMatcher
  | -- | "when your hero's attack deals any amount of excess damage": before it is counted
    HeroAttackDealsExcessDamage IdentityMatcher
  | -- | "after you deal excess damage to an enemy": a player matching dealt damage past
    -- an enemy's remaining hit points
    DealsExcessDamage IdentityMatcher
  | -- | "after <ally> defends against an attack": the attack it defended has resolved
    AllyDefends AllyMatcher
  | AllyDefendsAgainst WindowTiming AllyMatcher EnemyMatcher
  | IdentityWouldTakeAttackDamageFrom EnemyMatcher IdentityMatcher
  -- Infiltrate the Museum
  | -- | a card would be placed into a discard pile from play
    CardWouldBeDiscardedFromPlay
  | PlayerCardWouldBeDiscardedFromPlay
  -- Adam Warlock
  | -- | "after your deck runs out of cards" (Soul World), "after a player resets their
    -- deck" (Universal Church of Truth)
    PlayerDeckResets WindowTiming IdentityMatcher
  | -- | "after <enemy> activates against you": its attack or scheme against a player
    -- matching, from start to end, whether it resolved or was cancelled (The Magus)
    EnemyActivates WindowTiming EnemyMatcher IdentityMatcher
  -- The Mad Titan's Shadow: Loki
  | -- | "after Loki is swapped with a set-aside Loki villain"
    VillainWasSwapped WindowTiming VillainMatcher
  | -- | "when <the villain> is defeated": his When Defeated and "when defeated" forced
    -- interrupts, before his defeat resolves
    VillainIsDefeated WindowTiming VillainMatcher
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data Window = Window
  { windowTiming :: WindowTiming
  , windowType :: WindowType
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data WindowType
  = GeneratedResources IdentityId Natural
  | ResolvedTreacheryCard IdentityId EncounterCard
  | HeroThwartComplete IdentityId
  | ThwartRemovedThreat Source SchemeId Natural Natural
  | ThreatRemovedBy Source SchemeId Natural Natural
  | LastThreatRemoved IdentityId SchemeId
  | PlayedAlly AllyId
  | -- | the ally entered play, played or put into play
    AllyEnteredPlay AllyId
  | PlayedSupport SupportId
  | IdentityTakeDamage IdentityId Damage
  | IdentityTakeDamageFrom IdentityId Source Damage
  | AllyTakeDamage AllyId Damage
  | -- | the player's hero thwarted the scheme, removing all its threat
    ThwartedAllThreat IdentityId SchemeId
  | RevealTreachery TreacheryId RevealSource
  | RevealVillain VillainId RevealSource
  | DamagedVillain VillainId Damage
  | VillainDamageFrom VillainId Source Damage
  | -- | the villain, the damage that defeated its stage, what dealt it, and how much of
    -- it went past its hit points
    DefeatedVillain VillainId Damage Source Natural
  | -- | the minion, the damage that defeated it, what dealt it, and how much of it went
    -- past its hit points
    DefeatedMinion MinionId Damage Source Natural
  | DefeatedEnemyWithUpgrades EnemyId [CardDef] Damage Source
  | MinionEnteredPlay MinionId
  | SideSchemeEnteredPlay SideSchemeId
  | EncounterCardResolved IdentityId EncounterCard
  | PhaseBegan Phase
  | -- | the enemy took this damage, from this source: none a tough status card prevented
    EnemyTookDamageFrom EnemyId Damage Source
  | EnemyAttack EnemyId IdentityId
  | -- | the damage an enemy's attack dealt the character
    EnemyAttacksAndDamages EnemyId CharacterId Natural
  | Schemed VillainId
  | IdentityAttack IdentityId EnemyId
  | PlayerAttackResolved Source IdentityId [EnemyId]
  | AllyThwart AllyId SchemeId
  | AllyAttackCompletedAgainst AllyId [EnemyId]
  | AllyAttack AllyId EnemyId
  | -- | A hero or ally attacks a friendly identity, without becoming an enemy.
    FriendlyCharacterAttack CharacterId IdentityId
  | ThreatPlaced ThreatSource SchemeId Natural
  | IdentityChangesForm IdentityId
  | -- | the identity changed to the energy form of this upgrade, turned faceup
    EnergyFormChanged IdentityId UpgradeId
  | MassFormChanged IdentityId UpgradeId
  | IdentityDiscardedTough IdentityId
  | VillainActivationWouldBegin VillainId IdentityId
  | IgnoredKeyword IdentityId MinionId
  | IgnoredCrisis IdentityId SideSchemeId
  | MadeBasicAttack IdentityId
  | MadeBasicAttackAgainst IdentityId EnemyId
  | DefeatedSideScheme SideSchemeId
  | -- | the side scheme has been defeated, by the player, and by what removed its last
    -- threat when that is known
    DefeatedSideSchemeBy SideSchemeId IdentityId (Maybe Source)
  | -- | the alter-ego has used their REC
    UsedRecovery IdentityId
  | -- | the identity was exhausted and has been readied
    IdentityReadied IdentityId
  | HeroDefends IdentityId EnemyId
  | MadeBasicDefense IdentityId EnemyId
  | EncounterCardRevealed RevealSource IdentityId EncounterCard
  | RoundEnded
  | IdentityDefeat IdentityId
  | TurnBegan IdentityId
  | -- | the player, the event and its card
    EventPlayed IdentityId EventId CardDef
  | -- | the ally, and the enemy whose attack defeated it
    DefeatedAlly AllyId (Maybe EnemyId)
  | FlippedBoostCard EncounterCard EnemyId
  | CountingBoostIcons EncounterCard
  | CardPlayed IdentityId PlayerCard
  | -- | The player, upgrade and ally named when it was played, even after either leaves.
    UpgradePlayedOnAlly IdentityId UpgradeId AllyId
  | -- | the player played the card from their hand, its cost paid
    PlayedCardFromHand IdentityId PlayerCard
  | -- | the player is about to pay for the card from their hand
    PlayingCardFromHand IdentityId PlayerCard
  | -- | the player resolved the ability of a card with this def
    ResolvedAbility IdentityId CardDef
  | EventAbilityResolved IdentityId EventId CardDef
  | AllyLeavingPlay IdentityId AllyId CardDef
  | SchemedByMinion MinionId
  | -- | the player took damage from this source
    IdentityTookDamage IdentityId Source Damage
  | -- | the ally took damage from this source: none a tough status card prevented
    AllyTookDamage AllyId Source Damage
  | -- | the ally, as a card, and its controller
    AllyCardDefeated IdentityId PlayerCard
  | -- | the surge keyword of a card revealed for the player
    SurgeResolves IdentityId
  | -- | the player engages the minion: it enters play engaged with them, or is made to
    -- engage them
    EngagedMinion IdentityId MinionId
  | MadeBasicThwart IdentityId
  | DefendedWithoutDamage IdentityId EnemyId
  | -- | the player's hero's attack is dealing this much damage past the enemy's
    -- remaining hit points, which may yet be increased
    ExcessDamageDealt IdentityId EnemyId Natural
  | -- | the attack the ally defended against has resolved
    AllyDefendedAgainst AllyId EnemyId
  | TurnEnded IdentityId
  -- Infiltrate the Museum
  | -- | the card, a player's or an encounter card, is leaving play for its discard pile
    WouldBeDiscardedFromPlay (Either PlayerCard EncounterCard)
  -- The Mad Titan's Shadow: Thanos
  | -- | the enemy's activation against the player: its attack, or its scheme in their
    -- alter-ego's activation; When as it begins, After once it has fully resolved, its
    -- boost cards discarded. A stunned attack or confused scheme is none (RR Activation)
    EnemyActivation EnemyId IdentityId
  | -- | the Infinity Stone deck's last card has left it, before its discard pile is
    -- shuffled back
    InfinityStoneDeckRanOut
  -- Adam Warlock
  | -- | the player's deck ran out and was made again from their discard pile, their
    -- facedown encounter card dealt (RR Player Deck)
    PlayerDeckReset IdentityId
  -- The Mad Titan's Shadow: Loki
  | -- | the villain card was swapped with a set-aside one of his title
    VillainSwapped VillainId
  | -- | the villain is being defeated, his defeat not yet resolved
    VillainDefeat VillainId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)
