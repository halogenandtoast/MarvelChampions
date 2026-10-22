module Marvel.Window.Types where

import Marvel.Prelude

import {-# SOURCE #-} Marvel.Card.EncounterCard
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
  | -- | a player plays an event: not an event's ability resolved without it being played
    PlaysEvent WindowTiming IdentityMatcher
  | BoostCardFlipped WindowTiming Activation
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
  | EventPlayed IdentityId EventId
  | -- | a boost card dealt to an enemy turned faceup, before its boost icons count
    BoostCardTurnedFaceup EnemyId EncounterCard Activation
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

-- | What an enemy's activation is.
data Activation = AttackActivation | SchemeActivation
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)
