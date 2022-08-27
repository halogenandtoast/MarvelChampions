module Marvel.Upgrade.Upgrades.CaptainMarvelsHelmet
  ( captainMarvelsHelmet
  , CaptainMarvelsHelmet(..)
  ) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Source
import Marvel.Target
import Marvel.Trait
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

captainMarvelsHelmet :: UpgradeCard CaptainMarvelsHelmet
captainMarvelsHelmet = upgrade CaptainMarvelsHelmet Cards.captainMarvelsHelmet

newtype CaptainMarvelsHelmet = CaptainMarvelsHelmet (Attrs Upgrade)
  deriving anyclass IsUpgrade
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsSource, IsTarget)

instance HasModifiersFor CaptainMarvelsHelmet where
  getModifiersFor _ (IdentityTarget ident) (CaptainMarvelsHelmet attrs)
    | ident == upgradeController attrs = do
      hasAerial <- identityMatches (IdentityWithTrait Aerial) ident
      pure [DefenseModifier $ if hasAerial then 2 else 1]
  getModifiersFor _ _ _ = pure []

instance HasAbilities CaptainMarvelsHelmet where
  getAbilities _ = []

instance RunMessage CaptainMarvelsHelmet where
  runMessage msg (CaptainMarvelsHelmet attrs) =
    CaptainMarvelsHelmet <$> runMessage msg attrs
