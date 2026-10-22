module Marvel.Upgrade.Upgrades.CaptainAmericasShield (
  captainAmericasShield,
  CaptainAmericasShield (..),
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

captainAmericasShield :: UpgradeCard CaptainAmericasShield
captainAmericasShield = upgrade CaptainAmericasShield Cards.captainAmericasShield

newtype CaptainAmericasShield = CaptainAmericasShield (Attrs Upgrade)
  deriving anyclass (IsUpgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Captain America, not Steve Rogers: only the hero gets them.
instance HasModifiersFor CaptainAmericasShield where
  getModifiersFor _ (IdentityRef ident) (CaptainAmericasShield a) | ident == upgradeController a = do
    isHero <- identityMatches HeroIdentity ident
    pure [m | isHero, m <- [DefenseModifier 1, KeywordModifier (Retaliate 1)]]
  getModifiersFor _ _ _ = pure []

instance HasAbilities CaptainAmericasShield where
  getAbilities _ = []

instance RunMessage CaptainAmericasShield where
  runMessage msg (CaptainAmericasShield attrs) = CaptainAmericasShield <$> runMessage msg attrs
