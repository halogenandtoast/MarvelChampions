module Marvel.Upgrade.Upgrades.EnergyDuplication (
  energyDuplication,
  EnergyDuplication (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

energyDuplication :: UpgradeCard EnergyDuplication
energyDuplication = upgrade EnergyDuplication Cards.energyDuplication

newtype EnergyDuplication = EnergyDuplication (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Resource: Exhaust Energy Duplication → generate the printed resource on your
-- faceup energy form upgrade. One ability for each form, usable while it is the faceup
-- one; with none faceup, none is (R24).
instance HasAbilities EnergyDuplication where
  getAbilities (EnergyDuplication a) =
    [ ability a n HeroResource (OwnsThis <> UpgradeExists (UpgradeIs form <> NotUpgrade FacedownUpgrade <> upgradeControlledBy (upgradeController a))) ExhaustCost $
        Pay $ ResourcePayment r
    | (n, form) <- zip [1 ..] [Cards.gamma, Cards.photon, Cards.pulsar]
    , r <- take 1 (printedResources form)
    ]

instance RunMessage EnergyDuplication where
  runMessage msg (EnergyDuplication attrs) = EnergyDuplication <$> runMessage msg attrs
