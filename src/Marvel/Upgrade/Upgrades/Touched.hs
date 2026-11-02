module Marvel.Upgrade.Upgrades.Touched where

import Marvel.Id
import Marvel.Prelude
import Marvel.Card.Code
import Marvel.Ability
import Marvel.Entity
import Marvel.Game.Source (touchedHost)
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Query
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Trait qualified as Trait
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

-- Its owner remains Rogue even while attached to another player's character.
touched :: UpgradeCard Touched
touched = upgrade Touched Cards.touched

newtype Touched = Touched (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor Touched where
  getModifiersFor _ (IdentityRef you) (Touched a) | you == upgradeOwner a = do
    host <- touchedHost you
    case host of
      Just (AttachmentCharacter _) -> pure []
      Just (IdentityCharacter p) -> do
        hero <- selectAny (IdentityWithId p <> HeroIdentity)
        pure [KeywordModifier Stalwart | hero]
      Just (AllyCharacter _) -> pure [TraitModifier Trait.Aerial]
      Just (MinionCharacter _) -> pure [KeywordModifier Overkill]
      Just (VillainCharacter _) -> pure [KeywordModifier $ Retaliate 1]
      Nothing -> pure []
  getModifiersFor _ _ _ = pure []

instance RunMessage Touched where
  runMessage msg (Touched attrs) = runQueueT $ Touched <$> runMessage msg attrs
