module Marvel.Identity.Types where

import Marvel.Prelude

import Marvel.AlterEgo.Types
import Marvel.Card.Types
import Marvel.Deck
import Marvel.Discard
import Marvel.Entity
import Marvel.Hand
import Marvel.Hero.Types

data PlayerIdentitySide = HeroSide Hero | AlterEgoSide AlterEgo
  deriving stock (Show, Eq, Generic)
  deriving anyclass ToJSON

newtype PlayerIdentity = PlayerIdentity (Attrs PlayerIdentity)
  deriving newtype (Show, Eq)

instance Entity PlayerIdentity where
  type Id PlayerIdentity = IdentityId
  data Attrs PlayerIdentity = PlayerIdentityAttrs
    { playerIdentityId :: IdentityId
    , playerIdentitySide :: Side
    , playerIdentitySides :: HashMap Side PlayerIdentitySide
    , playerIdentityHP :: HP Natural
    , playerIdentityDamage :: Natural
    , playerIdentityDeck :: Deck
    , playerIdentityDiscard :: Discard
    , playerIdentityHand :: Hand
    , playerIdentityPassed :: Bool
    , playerIdentityAllies :: HashSet AllyId
    , playerIdentityAllyLimit :: Natural
    , playerIdentityMinions :: HashSet MinionId
    , playerIdentitySupports :: HashSet SupportId
    , playerIdentityUpgrades :: HashSet UpgradeId
    , playerIdentityExhausted :: Bool
    , playerIdentityEncounterCards :: [EncounterCard]
    , playerIdentityDamageReduction :: Natural
    , playerIdentityStunned :: Bool
    , playerIdentityConfused :: Bool
    , playerIdentityTough :: Bool
    , playerIdentityDefeated :: Bool
    , playerIdentityDefended :: Bool
    }
    deriving stock (Show, Eq, Generic)
  data Field PlayerIdentity :: Type -> Type where
    PlayerIdentityId :: Field PlayerIdentity IdentityId
    PlayerIdentitySide :: Field PlayerIdentity Side
    PlayerIdentitySides :: Field PlayerIdentity (HashMap Side PlayerIdentitySide)
    PlayerIdentityHP :: Field PlayerIdentity (HP Natural)
    PlayerIdentityDamage :: Field PlayerIdentity Natural
    PlayerIdentityDeck :: Field PlayerIdentity Deck
    PlayerIdentityDiscard :: Field PlayerIdentity Discard
    PlayerIdentityHand :: Field PlayerIdentity Hand
    PlayerIdentityPassed :: Field PlayerIdentity Bool
    PlayerIdentityAllies :: Field PlayerIdentity (HashSet AllyId)
    PlayerIdentityAllyLimit :: Field PlayerIdentity Natural
    PlayerIdentityMinions :: Field PlayerIdentity (HashSet MinionId)
    PlayerIdentitySupports :: Field PlayerIdentity (HashSet SupportId)
    PlayerIdentityUpgrades :: Field PlayerIdentity (HashSet UpgradeId)
    PlayerIdentityExhausted :: Field PlayerIdentity Bool
    PlayerIdentityEncounterCards :: Field PlayerIdentity [EncounterCard]
    PlayerIdentityDamageReduction :: Field PlayerIdentity Natural
    PlayerIdentityStunned :: Field PlayerIdentity Bool
    PlayerIdentityConfused :: Field PlayerIdentity Bool
    PlayerIdentityTough :: Field PlayerIdentity Bool
    PlayerIdentityDefeated :: Field PlayerIdentity Bool
    PlayerIdentityDefended :: Field PlayerIdentity Bool
  field fld p = let PlayerIdentityAttrs {..} = toAttrs p in case fld of
    PlayerIdentityId -> playerIdentityId
    PlayerIdentitySide -> playerIdentitySide
    PlayerIdentitySides -> playerIdentitySides
    PlayerIdentityHP -> playerIdentityHP
    PlayerIdentityDamage -> playerIdentityDamage
    PlayerIdentityDeck -> playerIdentityDeck
    PlayerIdentityDiscard -> playerIdentityDiscard
    PlayerIdentityHand -> playerIdentityHand
    PlayerIdentityPassed -> playerIdentityPassed
    PlayerIdentityAllies -> playerIdentityAllies
    PlayerIdentityAllyLimit -> playerIdentityAllyLimit
    PlayerIdentityMinions -> playerIdentityMinions
    PlayerIdentitySupports -> playerIdentitySupports
    PlayerIdentityUpgrades -> playerIdentityUpgrades
    PlayerIdentityExhausted -> playerIdentityExhausted
    PlayerIdentityEncounterCards -> playerIdentityEncounterCards
    PlayerIdentityDamageReduction -> playerIdentityDamageReduction
    PlayerIdentityStunned -> playerIdentityStunned
    PlayerIdentityConfused -> playerIdentityConfused
    PlayerIdentityTough -> playerIdentityTough
    PlayerIdentityDefeated -> playerIdentityDefeated
    PlayerIdentityDefended -> playerIdentityDefended
  toAttrs (PlayerIdentity attrs) = attrs
  toId = playerIdentityId . toAttrs
