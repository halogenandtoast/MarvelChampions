module Marvel.Upgrade.Upgrades.ImmovableObject (
  immovableObject,
  ImmovableObject (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

immovableObject :: UpgradeCard ImmovableObject
immovableObject = upgrade ImmovableObject Cards.immovableObject

newtype ImmovableObject = ImmovableObject (Attrs Upgrade)
  deriving anyclass (IsUpgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | "You get +4 hit points. Hulk gains retaliate 1": retaliate in hero form only.
instance HasModifiersFor ImmovableObject where
  getModifiersFor _ (IdentityRef ident) (ImmovableObject a)
    | ident == upgradeController a = do
        hero <- identityMatches HeroIdentity ident
        pure $ HitPointModifier 4 : [KeywordModifier (Retaliate 1) | hero]
  getModifiersFor _ _ _ = pure []

instance HasAbilities ImmovableObject

instance RunMessage ImmovableObject where
  runMessage msg (ImmovableObject attrs) = ImmovableObject <$> runMessage msg attrs
