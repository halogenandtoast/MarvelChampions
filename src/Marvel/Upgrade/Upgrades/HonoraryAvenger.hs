module Marvel.Upgrade.Upgrades.HonoraryAvenger (
  honoraryAvenger,
  HonoraryAvenger (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Choice
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Trait
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

honoraryAvenger :: UpgradeCard HonoraryAvenger
honoraryAvenger = upgrade HonoraryAvenger Cards.honoraryAvenger

newtype HonoraryAvenger = HonoraryAvenger (Attrs Upgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance HasAbilities HonoraryAvenger where
  getAbilities _ = []

-- | On an ally, or on the identity of the player controlling it.
instance HasModifiersFor HonoraryAvenger where
  getModifiersFor _ target (HonoraryAvenger a) = pure case (target, upgradeAttachedAlly a) of
    (AllyRef aid, Just aid') | aid == aid' -> modifiers
    (IdentityRef ident, Nothing) | ident == upgradeController a -> modifiers
    _ -> []
   where
    modifiers = [HitPointModifier 1, TraitModifier Avenger]

-- | Attached to a friendly character without one: an identity, which takes control of
-- it, or an ally.
instance IsUpgrade HonoraryAvenger where
  upgradeTargets _ = do
    let named = UpgradeIs Cards.honoraryAvenger
    players <- getPlayers
    -- one on an identity is controlled by its player, and attached to nothing else
    identities <- filterM (\p -> selectNone (named <> UpgradeOnIdentity (IdentityWithId p))) players
    allies <- selectList $ NotAlly $ AllyWithUpgrade named
    pure $ Just $ map toTarget identities <> map toTarget allies

instance RunMessage HonoraryAvenger where
  runMessage msg (HonoraryAvenger a) = HonoraryAvenger <$> runMessage msg a
