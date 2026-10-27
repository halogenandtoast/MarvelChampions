module Marvel.Upgrade.Upgrades.ThrusterBoots (
  thrusterBoots,
  ThrusterBoots (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Trait
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

thrusterBoots :: UpgradeCard ThrusterBoots
thrusterBoots = upgrade ThrusterBoots Cards.thrusterBoots

newtype ThrusterBoots = ThrusterBoots (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | While in hero form, Rocket Raccoon gets +1 THW and gains the Aerial trait.
instance HasModifiersFor ThrusterBoots where
  getModifiersFor _ (IdentityRef ident) (ThrusterBoots a) | ident == upgradeController a = do
    isHero <- identityMatches HeroIdentity ident
    pure [m | isHero, m <- [ThwartModifier 1, TraitModifier Aerial]]
  getModifiersFor _ _ _ = pure []

instance RunMessage ThrusterBoots where
  runMessage msg (ThrusterBoots attrs) = ThrusterBoots <$> runMessage msg attrs
