module Marvel.Upgrade.Upgrades.BruteForce (
  bruteForce,
  BruteForce (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Window

bruteForce :: UpgradeCard BruteForce
bruteForce = upgrade BruteForce Cards.bruteForce

newtype BruteForce = BruteForce (Attrs Upgrade)
  deriving anyclass (IsUpgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | "Your hero gets +1 ATK. Your basic attacks gain piercing."
instance HasModifiersFor BruteForce where
  getModifiersFor _ (IdentityRef ident) (BruteForce a)
    | ident == upgradeController a = pure [AttackModifier 1, KeywordModifier Piercing]
  getModifiersFor _ _ _ = pure []

-- | "Forced Response: After you make a basic attack, discard Brute Force."
instance HasAbilities BruteForce where
  getAbilities (BruteForce a) =
    [ limitedWindowAbility a 1 (MakesBasicAttack After You) ForcedResponse OwnsThis NoCost (DiscardTarget $ toTarget a)
    ]

instance RunMessage BruteForce where
  runMessage msg (BruteForce attrs) = BruteForce <$> runMessage msg attrs
