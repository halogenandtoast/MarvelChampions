module Marvel.Upgrade.Upgrades.Soulsword where

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

soulsword :: UpgradeCard Soulsword
soulsword = upgrade Soulsword Cards.soulsword

newtype Soulsword = Soulsword (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor Soulsword where
  getModifiersFor _ (IdentityRef you) (Soulsword attrs) = do
    magik <- identityMatches (HeroIdentity <> IdentityWithTitle "Magik") you
    deck <- projectMap PlayerIdentityDeck unDeck (upgradeController attrs)
    let matches = maybe False (any (`elem` [Physical, Wild]) . printedResources . getCardDef) (headMay deck)
    pure $ if magik then [BasicAttackKeyword Piercing] <> [AttackModifier 1 | matches] else []
  getModifiersFor _ _ _ = pure []

instance RunMessage Soulsword where
  runMessage msg (Soulsword attrs) = runQueueT $ Soulsword <$> runMessage msg attrs
