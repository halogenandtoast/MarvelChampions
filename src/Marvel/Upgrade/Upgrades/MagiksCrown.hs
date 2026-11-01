module Marvel.Upgrade.Upgrades.MagiksCrown where

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

magiksCrown :: UpgradeCard MagiksCrown
magiksCrown = upgrade MagiksCrown Cards.magiksCrown

newtype MagiksCrown = MagiksCrown (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor MagiksCrown where
  getModifiersFor _ (IdentityRef you) (MagiksCrown attrs) = do
    magik <- identityMatches (HeroIdentity <> IdentityWithTitle "Magik") you
    deck <- projectMap PlayerIdentityDeck unDeck (upgradeController attrs)
    let matches = maybe False (any (`elem` [Mental, Wild]) . printedResources . getCardDef) (headMay deck)
    pure $ if magik then [KeywordModifier (Steady)] <> [ThwartModifier 1 | matches] else []
  getModifiersFor _ _ _ = pure []

instance RunMessage MagiksCrown where
  runMessage msg (MagiksCrown attrs) = runQueueT $ MagiksCrown <$> runMessage msg attrs
