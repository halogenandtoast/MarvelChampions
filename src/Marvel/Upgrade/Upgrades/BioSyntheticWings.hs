module Marvel.Upgrade.Upgrades.BioSyntheticWings (
  bioSyntheticWings,
  BioSyntheticWings (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Trait
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Window

bioSyntheticWings :: UpgradeCard BioSyntheticWings
bioSyntheticWings = upgrade BioSyntheticWings Cards.bioSyntheticWings

newtype BioSyntheticWings = BioSyntheticWings (Attrs Upgrade)
  deriving anyclass (IsUpgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Wasp gains the Aerial trait: the hero, either form; Nadia Van Dyne is not Wasp.
instance HasModifiersFor BioSyntheticWings where
  getModifiersFor _ (IdentityRef ident) (BioSyntheticWings a) | ident == upgradeController a = do
    isHero <- identityMatches HeroIdentity ident
    pure [TraitModifier Aerial | isHero]
  getModifiersFor _ _ _ = pure []

-- | Interrupt: When you would take any amount of damage, if you are in Tiny hero form,
-- exhaust Bio-Synthetic Wings → prevent 1 of that damage.
instance HasAbilities BioSyntheticWings where
  getAbilities a@(BioSyntheticWings attrs) =
    [ limitedWindowAbility
        a
        1
        (WouldTakeDamage (IdentityWithId $ upgradeController attrs) FromAnyDamageSource AnyValue)
        Interrupt
        (OwnsThis <> SelfMatches (HeroIdentity <> IdentityWithTrait Tiny))
        ExhaustCost
        (runAbility a 1)
    ]

instance RunMessage BioSyntheticWings where
  runMessage msg a@(BioSyntheticWings attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget a -> True) 1 _ _ -> do
      preventDamage ident 1
      pure a
    _ -> BioSyntheticWings <$> runMessage msg attrs
