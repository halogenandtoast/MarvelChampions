module Marvel.Upgrade.Upgrades.Gamma (
  gamma,
  Gamma (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
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

gamma :: UpgradeCard Gamma
gamma = upgrade Gamma Cards.gamma

newtype Gamma = Gamma (Attrs Upgrade)
  deriving anyclass (IsUpgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Energy form. Spectrum gets +2 ATK: faceup, which she is only in hero form (a
-- facedown upgrade gives nothing, 'Marvel.Game.isBlanked').
instance HasModifiersFor Gamma where
  getModifiersFor _ (IdentityRef ident) (Gamma a) | ident == upgradeController a = do
    hero <- identityMatches HeroIdentity ident
    pure [AttackModifier 2 | hero]
  getModifiersFor _ _ _ = pure []

-- | Hero Response: After you change to this form, deal 1 damage to an enemy.
instance HasAbilities Gamma where
  getAbilities (Gamma a) =
    [ limitedWindowAbility a 1 (EnergyFormChangedTo After (UpgradeWithId $ upgradeId a)) HeroResponse OwnsThis NoCost $
        runAbility a 1
    ]

instance RunMessage Gamma where
  runMessage msg u@(Gamma attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      chooseDamage you attrs (toDamage 1 FromAbility) DamageableEnemy
      pure u
    _ -> Gamma <$> runMessage msg attrs
