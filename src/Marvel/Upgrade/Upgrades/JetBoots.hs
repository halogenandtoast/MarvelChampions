module Marvel.Upgrade.Upgrades.JetBoots (
  jetBoots,
  JetBoots (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Identity.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Projection
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Trait
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Window

jetBoots :: UpgradeCard JetBoots
jetBoots = upgrade JetBoots Cards.jetBoots

newtype JetBoots = JetBoots (Attrs Upgrade)
  deriving anyclass (IsUpgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Star-Lord gains the Aerial trait: the hero, not Peter Quill.
instance HasModifiersFor JetBoots where
  getModifiersFor _ (IdentityRef ident) (JetBoots attrs)
    | ident == upgradeController attrs = do
        hero <- member ident <$> select HeroIdentity
        pure [TraitModifier Aerial | hero]
  getModifiersFor _ _ _ = pure []

-- | Hero Interrupt: When Star-Lord would take any amount of damage, exhaust Jet Boots →
-- prevent 1 of that damage for each facedown encounter card in front of you.
instance HasAbilities JetBoots where
  getAbilities a@(JetBoots attrs) =
    [ limitedWindowAbility
        a
        1
        (WouldTakeDamage (IdentityWithId $ upgradeController attrs) FromAnyDamageSource AnyValue)
        HeroInterrupt
        (OwnsThis <> InHeroForm)
        ExhaustCost
        (runAbility a 1)
    ]

instance RunMessage JetBoots where
  runMessage msg u@(JetBoots attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> do
      n <- length <$> project PlayerIdentityEncounterCards ident
      when (n > 0) $ preventDamage ident (fromIntegral n)
      pure u
    _ -> JetBoots <$> runMessage msg attrs
