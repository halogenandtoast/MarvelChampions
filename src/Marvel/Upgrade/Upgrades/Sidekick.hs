module Marvel.Upgrade.Upgrades.Sidekick where

import Marvel.Prelude
import Marvel.Query
import Marvel.Game.Source
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Projection ()
import Marvel.Question
import Marvel.Window qualified as W
import Marvel.Ability
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

sidekick :: UpgradeCard Sidekick
sidekick = upgrade Sidekick Cards.sidekick

newtype Sidekick = Sidekick (Attrs Upgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Sidekick where
  getAbilities a@(Sidekick attrs) =
    [limitedWindowAbility a 1 (W.UsesRecovery $ IdentityWithId $ upgradeController attrs)
      Response OwnsThis NoCost (runAbility a 1) | isJust (upgradeAttachedAlly attrs)]
instance IsUpgrade Sidekick where
  upgradeTargets (Sidekick attrs) = do
    allies <- selectList (AllyControlledBy $ IdentityWithId $ upgradeController attrs)
    valid <- filterM (\aid -> getsGame $ maybe False (cardMatch HeroSpecificCard) . (`abilityCardDef` AllyRef aid)) allies
    pure $ Just $ map toTarget valid

instance HasModifiersFor Sidekick where
  getModifiersFor _ (AllyRef aid) (Sidekick attrs) = pure [HitPointModifier 2 | Just aid == upgradeAttachedAlly attrs]
  getModifiersFor _ _ _ = pure []
instance RunMessage Sidekick where
  runMessage msg a@(Sidekick attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      for_ (upgradeAttachedAlly attrs) $ \aid -> heal aid 2
      pure a
    _ -> Sidekick <$> runMessage msg attrs
