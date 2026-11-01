module Marvel.Upgrade.Upgrades.ContainmentStrategy where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Window qualified as W
import Data.Maybe (maybeToList)
import Marvel.Keyword (Keyword (Permanent))
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

containmentStrategy :: UpgradeCard ContainmentStrategy
containmentStrategy = upgrade ContainmentStrategy Cards.containmentStrategy
newtype ContainmentStrategy = ContainmentStrategy (Attrs Upgrade)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsUpgrade ContainmentStrategy where
  upgradeTargets _ = do
    schemes <- selectList $ NotSideScheme (SideSchemeWithKeyword Permanent)
    existing <- selectList $ UpgradeIs Cards.containmentStrategy
    occupied <- concatMapM getUpgradeAttachedSchemes existing
    let available = [sid | sid <- schemes, SchemeSideSchemeId sid `notElem` occupied]
    pure $ Just $ map (schemeRef . SchemeSideSchemeId) available

instance HasAbilities ContainmentStrategy where
  getAbilities a@(ContainmentStrategy attrs) =
    [limitedWindowAbility a 1 (W.WindowOneOf [W.HeroDefended W.After AnyIdentity AnyEnemy, W.HeroDefendedWithoutDamage W.After AnyIdentity])
      Response (OwnsThis <> SchemeExists (SchemeWithId $ SchemeSideSchemeId sid)) NoCost (runAbility a 1)
    | sid <- maybeToList (upgradeAttachedSideScheme attrs)]

instance RunMessage ContainmentStrategy where
  runMessage msg a@(ContainmentStrategy attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      let undamaged = any (\case W.DefendedWithoutDamage _ _ -> True; _ -> False) windows
      for_ (upgradeAttachedSideScheme attrs) $ \sid -> thwartScheme (SchemeSideSchemeId sid) attrs (if undamaged then 2 else 1)
      pure a
    _ -> ContainmentStrategy <$> runMessage msg attrs
