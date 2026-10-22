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
  deriving anyclass (IsUpgrade)
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
instance RunMessage HonoraryAvenger where
  runMessage msg u@(HonoraryAvenger a) = case msg of
    UpgradeMessage ident PlayedUpgrade | ident == upgradeId a -> do
      let named = UpgradeIs Cards.honoraryAvenger
      players <- getPlayers
      -- this one is on its player's identity until it is attached
      identities <- flip filterM players \p ->
        all (== upgradeId a) <$> selectList (named <> UpgradeOnIdentity (IdentityWithId p))
      allies <- selectList $ NotAlly $ AllyWithUpgrade named
      chooseOne (upgradeController a) $
        [ TargetLabel (IdentityRef p) [Run [UpgradeMessage (upgradeId a) $ UpgradeControlledByPlayer p]]
        | p <- identities
        ]
          <> [ TargetLabel (toRef allyId) [Run [UpgradeMessage (upgradeId a) $ UpgradeAttachedToAlly allyId]]
             | allyId <- allies
             ]
      pure u
    _ -> HonoraryAvenger <$> runMessage msg a
