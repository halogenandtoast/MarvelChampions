module Marvel.Upgrade.Upgrades.MysticalArmor where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Card
import Marvel.Deck
import Marvel.Identity.Types
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Projection
import Marvel.Resource.Types

mysticalArmor :: UpgradeCard MysticalArmor
mysticalArmor = upgrade MysticalArmor Cards.mysticalArmor

newtype MysticalArmor = MysticalArmor (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor MysticalArmor where
  getModifiersFor _ (IdentityRef you) (MysticalArmor attrs) = do
    magik <- identityMatches (HeroIdentity <> IdentityWithTitle "Magik") you
    deck <- projectMap PlayerIdentityDeck unDeck (upgradeController attrs)
    let matches = maybe False (any (`elem` [Energy, Wild]) . printedResources . getCardDef) (headMay deck)
    pure $ if magik then [KeywordModifier (Retaliate 1)] <> [DefenseModifier 1 | matches] else []
  getModifiersFor _ _ _ = pure []

instance RunMessage MysticalArmor where
  runMessage msg (MysticalArmor attrs) = runQueueT $ MysticalArmor <$> runMessage msg attrs
