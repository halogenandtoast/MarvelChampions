module Marvel.Message.Villain where

import Marvel.Prelude

import Marvel.Card.Code (CardCode)
import Marvel.Card.EncounterCard
import Marvel.Damage
import Marvel.Id
import Marvel.Keyword (Keyword)
import Marvel.Ref

data VillainMessage
  = VillainDefeatPending (Maybe (Source, Damage, Natural))
  | VillainResolveDefeat CardCode (Maybe (Source, Damage, Natural))
  | SetVillainHp
  | VillainDamaged Source Damage
  | VillainStunned Source
  | VillainConfused Source
  | VillainBecomeTough
  | VillainDealtBoost EncounterCard
  | VillainFlipBoostCards
  | VillainCheckAdditionalBoosts
  | -- | its attack under way gains the keyword: overkill, piercing or ranged
    VillainAttackGains Keyword
  | -- | the villain gets +X (or -X) ATK for the attack under way
    VillainAttackChanged Int
  | -- | all damage from its attack under way is prevented
    VillainAttackDamagePrevented
  | VillainAttacksWithAdditionalBoostCards IdentityId Natural
  | VillainSetAttackAdditionalBoostCards Natural
  | VillainAttacks IdentityId
  | -- | an explicit character target; enemy targets allow no declared defender
    VillainAttacksCharacter IdentityId CharacterId
  | -- | it attacks the player, the attack given no boost card (Escaped Convict)
    VillainAttacksWithoutBoost IdentityId
  | -- | it attacks the player, the attack gaining the keywords as it is declared
    -- (Avatar of Death: overkill and piercing)
    VillainAttacksGaining IdentityId [Keyword]
  | -- | its attack by the character resolved: retaliate answers it
    VillainRetaliates CharacterId
  | VillainBeginAttack IdentityId
  | VillainBeginAttackWithoutBoost IdentityId
  | VillainEndAttack
  | VillainAttacked
  | VillainSchemesWithBonus IdentityId Natural
  | VillainSchemes
  | -- | its coming scheme is against this player, not the active player's: a card one
    -- reveals outside their activation (Avatar of Death)
    VillainSchemesAgainst IdentityId
  | -- | that scheme is over, however it went (replaced, cancelled, confused): the player
    -- it was against is forgotten
    VillainSchemeAgainstEnded
  | -- | its scheme under way, past a confusion: what a replaced scheme never reaches
    VillainBeginScheme
  | VillainSchemed
  | VillainHealed Natural
  | VillainDefeated
  | -- | its hit points are gone: by default it is defeated ('VillainDefeated'); a villain
    -- that "would be defeated" answers it instead (Collector, Escape the Museum)
    VillainWouldBeDefeated
  | VillainDefendedBy CharacterId
  | AttachedToVillain AttachmentId
  | AttachedUpgradeToVillain UpgradeId
  | VillainAdvanced
  | -- | it turns to its other face, keeping its damage, status cards, attachments and
    -- boosts: the villain answers it with @flipVillainTo@
    FlipVillain
  | -- | its hit points (and remaining hit points) go up or down by this much
    VillainHitPointsChanged Int
  | -- | a boost card's icons are added to the activation
    VillainAddBoost Natural
  | -- | its next activation gets this much more SCH or ATK ("+1 ATK for this
    -- activation")
    VillainActivationBonus Natural
  | -- | it attacks the player's ally, the player being attacked
    VillainAttacksAlly IdentityId AllyId
  | -- | all damage to him from the attack the source makes is prevented, until that
    -- attack ends (Master of Illusions)
    VillainPreventAttackDamage Source
  | VillainSchemeRemovesThreat IdentityId Source
  | VillainAttackDamageRedirected EnemyId
  | VillainAttackBecomesIndirect
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
