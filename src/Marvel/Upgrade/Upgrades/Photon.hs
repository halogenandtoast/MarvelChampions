module Marvel.Upgrade.Upgrades.Photon (
  photon,
  Photon (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Window

photon :: UpgradeCard Photon
photon = upgrade Photon Cards.photon

newtype Photon = Photon (Attrs Upgrade)
  deriving anyclass (IsUpgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Energy form. Spectrum gets +2 THW: faceup, which she is only in hero form (a
-- facedown upgrade gives nothing, 'Marvel.Game.isBlanked').
instance HasModifiersFor Photon where
  getModifiersFor _ (IdentityRef ident) (Photon a) | ident == upgradeController a = do
    hero <- identityMatches HeroIdentity ident
    pure [ThwartModifier 2 | hero]
  getModifiersFor _ _ _ = pure []

-- | Hero Response: After you change to this form, remove 1 threat from a scheme.
instance HasAbilities Photon where
  getAbilities (Photon a) =
    [ limitedWindowAbility a 1 (EnergyFormChangedTo After (UpgradeWithId $ upgradeId a)) HeroResponse OwnsThis NoCost $
        runAbility a 1
    ]

instance RunMessage Photon where
  runMessage msg u@(Photon attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      removeThreat you attrs 1 ThwartableScheme
      pure u
    _ -> Photon <$> runMessage msg attrs
