module Marvel.Upgrade.Upgrades.StarLordsHelmet (
  starLordsHelmet,
  StarLordsHelmet (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.Identity.Types
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Projection
import Marvel.Query
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

starLordsHelmet :: UpgradeCard StarLordsHelmet
starLordsHelmet = upgrade StarLordsHelmet Cards.starLordsHelmet

newtype StarLordsHelmet = StarLordsHelmet (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | While you are in hero form, +1 hand size for each facedown encounter card in front
-- of you, to a maximum of +3.
instance HasModifiersFor StarLordsHelmet where
  getModifiersFor _ (IdentityRef ident) (StarLordsHelmet a)
    | ident == upgradeController a = do
        hero <- member ident <$> select HeroIdentity
        n <- length <$> project PlayerIdentityEncounterCards ident
        pure [HandSizeModifier (fromIntegral $ min 3 n) | hero, n > 0]
  getModifiersFor _ _ _ = pure []

instance RunMessage StarLordsHelmet where
  runMessage msg (StarLordsHelmet attrs) = StarLordsHelmet <$> runMessage msg attrs
