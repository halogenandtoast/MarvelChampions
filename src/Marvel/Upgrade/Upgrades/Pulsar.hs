module Marvel.Upgrade.Upgrades.Pulsar (
  pulsar,
  Pulsar (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Window

pulsar :: UpgradeCard Pulsar
pulsar = upgrade Pulsar Cards.pulsar

newtype Pulsar = Pulsar (Attrs Upgrade)
  deriving anyclass (IsUpgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Energy form. Spectrum gets +2 DEF: faceup, which she is only in hero form (a
-- facedown upgrade gives nothing, 'Marvel.Game.isBlanked').
instance HasModifiersFor Pulsar where
  getModifiersFor _ (IdentityRef ident) (Pulsar a) | ident == upgradeController a = do
    hero <- identityMatches HeroIdentity ident
    pure [DefenseModifier 2 | hero]
  getModifiersFor _ _ _ = pure []

-- | Hero Response: After you change to this form, heal 1 damage from Spectrum.
instance HasAbilities Pulsar where
  getAbilities (Pulsar a) =
    [ limitedWindowAbility a 1 (EnergyFormChangedTo After (UpgradeWithId $ upgradeId a)) HeroResponse (OwnsThis <> SelfMatches healableIdentity) NoCost $
        runAbility a 1
    ]

instance RunMessage Pulsar where
  runMessage msg u@(Pulsar attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      heal (IdentityCharacter you) 1
      pure u
    _ -> Pulsar <$> runMessage msg attrs
