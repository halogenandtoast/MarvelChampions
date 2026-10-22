module Marvel.Upgrade.Upgrades.TheSorcererSupreme (
  theSorcererSupreme,
  TheSorcererSupreme (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

theSorcererSupreme :: UpgradeCard TheSorcererSupreme
theSorcererSupreme = upgrade TheSorcererSupreme Cards.theSorcererSupreme

newtype TheSorcererSupreme = TheSorcererSupreme (Attrs Upgrade)
  deriving anyclass (IsUpgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | You get +1 hand size while in hero form.
instance HasModifiersFor TheSorcererSupreme where
  getModifiersFor _ (IdentityRef ident) (TheSorcererSupreme a)
    | ident == upgradeController a = do
        hero <- member ident <$> select HeroIdentity
        pure [HandSizeModifier 1 | hero]
  getModifiersFor _ _ _ = pure []

instance HasAbilities TheSorcererSupreme where
  getAbilities _ = []

instance RunMessage TheSorcererSupreme where
  runMessage msg (TheSorcererSupreme attrs) = TheSorcererSupreme <$> runMessage msg attrs
