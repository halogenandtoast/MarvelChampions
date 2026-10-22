module Marvel.Window.Types where

import Marvel.Prelude

import {-# SOURCE #-} Marvel.Card.Def
import {-# SOURCE #-} Marvel.Card.EncounterCard
import {-# SOURCE #-} Marvel.Card.PlayerCard.Types
import Marvel.Damage
import Marvel.Id
import Marvel.Matchers.Types
import Marvel.Ref (Source)

data WindowTiming = After | When | Would
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data RevealSource = RevealedFromEncounterDeck | RevealedFromVillain
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
  = PlayThis WindowTiming
  | WouldTakeDamage IdentityMatcher DamageSource GameValueMatcher
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
  | VillainSchemed WindowTiming VillainMatcher
  | MinionDefeated WindowTiming MinionMatcher
  | MinionEntersPlay WindowTiming MinionMatcher
  | EnemyDefeated WindowTiming EnemyMatcher DamageMatcher
  | IdentityChangedToForm WindowTiming IdentityMatcher
  | MakesBasicAttack WindowTiming IdentityMatcher
  | SideSchemeDefeated WindowTiming SideSchemeMatcher
  | HeroDefended WindowTiming IdentityMatcher EnemyMatcher
  | EncounterCardReveal WindowTiming BasicEncounterCardMatcher
  | RoundEnds
  | -- | "you reveal": the encounter card is revealed for a player matching
    EncounterCardRevealedBy WindowTiming IdentityMatcher BasicEncounterCardMatcher
  | -- | a player matching plays a card matching (paid for and put into play or resolved)
    PlaysCard WindowTiming IdentityMatcher CardMatcher
  | -- | a player matching resolves the ability of a card matching
    ResolvesAbility WindowTiming IdentityMatcher CardMatcher
  | MinionHasSchemed WindowTiming MinionMatcher
  | BoostCardTurnedFaceup WindowTiming BasicEncounterCardMatcher
  | -- | after a player matching takes damage of this kind
    TookDamage IdentityMatcher DamageSource
  | -- | an ally a player matching controls is defeated
    AllyWasDefeated WindowTiming IdentityMatcher
  | SurgeWouldResolve
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data Window = Window
  { windowTiming :: WindowTiming
  , windowType :: WindowType
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data WindowType
  = PlayedAlly AllyId
  | PlayedSupport SupportId
  | IdentityTakeDamage IdentityId Damage
  | RevealTreachery TreacheryId RevealSource
  | RevealVillain VillainId RevealSource
  | DamagedVillain VillainId Damage
  | DefeatedVillain VillainId Damage
  | -- | the minion, the damage that defeated it, and what dealt it
    DefeatedMinion MinionId Damage Source
  | MinionEnteredPlay MinionId
  | EnemyAttack EnemyId IdentityId
  | EnemyAttacksAndDamages EnemyId CharacterId
  | Schemed VillainId
  | IdentityAttack IdentityId EnemyId
  | AllyThwart AllyId SchemeId
  | AllyAttack AllyId EnemyId
  | ThreatPlaced ThreatSource SchemeId Natural
  | IdentityChangesForm IdentityId
  | MadeBasicAttack IdentityId
  | DefeatedSideScheme SideSchemeId
  | HeroDefends IdentityId EnemyId
  | EncounterCardRevealed IdentityId EncounterCard
  | RoundEnded
  | CardPlayed IdentityId PlayerCard
  | -- | the player resolved the ability of a card with this def
    ResolvedAbility IdentityId CardDef
  | SchemedByMinion MinionId
  | BoostCardFlipped EnemyId EncounterCard
  | -- | the player took damage from this source
    IdentityTookDamage IdentityId Source Damage
  | -- | the ally, as a card, and its controller
    DefeatedAlly IdentityId PlayerCard
  | -- | the surge keyword of a card revealed for the player
    SurgeResolves IdentityId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)
