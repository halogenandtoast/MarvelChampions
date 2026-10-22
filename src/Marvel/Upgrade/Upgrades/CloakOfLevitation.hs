module Marvel.Upgrade.Upgrades.CloakOfLevitation (
  cloakOfLevitation,
  CloakOfLevitation (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Trait
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

cloakOfLevitation :: UpgradeCard CloakOfLevitation
cloakOfLevitation = upgrade CloakOfLevitation Cards.cloakOfLevitation

newtype CloakOfLevitation = CloakOfLevitation (Attrs Upgrade)
  deriving anyclass (IsUpgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | You gain the Aerial trait.
instance HasModifiersFor CloakOfLevitation where
  getModifiersFor _ (IdentityRef ident) (CloakOfLevitation a)
    | ident == upgradeController a = pure [TraitModifier Aerial]
  getModifiersFor _ _ _ = pure []

instance HasAbilities CloakOfLevitation where
  getAbilities (CloakOfLevitation a) =
    [ability a 1 HeroAction (OwnsThis <> Exhausted) ExhaustCost ReadyIdentity]

instance RunMessage CloakOfLevitation where
  runMessage msg (CloakOfLevitation attrs) = CloakOfLevitation <$> runMessage msg attrs
