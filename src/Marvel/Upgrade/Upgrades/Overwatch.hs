module Marvel.Upgrade.Upgrades.Overwatch where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Window qualified as W
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Id
import Marvel.Game.Source (getUpgradeAttachedSchemes)
import Marvel.Question (runAbility)
import Data.Maybe (maybeToList)



overwatch :: UpgradeCard Overwatch
overwatch = upgrade Overwatch Cards.overwatch

newtype Overwatch = Overwatch (Attrs Upgrade)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

attachedScheme :: Attrs Upgrade -> Maybe SchemeId
attachedScheme a = (SchemeMainSchemeId <$> upgradeAttachedMainScheme a) <|> (SchemeSideSchemeId <$> upgradeAttachedSideScheme a)

instance IsUpgrade Overwatch where
  upgradeTargets _ = do
    existing <- selectList (UpgradeIs Cards.overwatch)
    occupied <- concatMapM getUpgradeAttachedSchemes existing
    schemes <- filter (`notElem` occupied) <$> selectList AnyScheme
    pure $ Just $ map schemeRef schemes

instance HasAbilities Overwatch where
  getAbilities a@(Overwatch attrs) =
    [limitedWindowAbility a 1 (W.ThreatRemovedByThwart W.When AnySource (SchemeWithId sid))
      HeroInterrupt (OwnsThis <> SchemeExists (NotScheme (SchemeWithId sid) <> ThwartableScheme)) (DiscardCost $ toTarget a) (runAbility a 1)
    | sid <- maybeToList (attachedScheme attrs)]

instance RunMessage Overwatch where
  runMessage msg a@(Overwatch attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows _ -> do
      for_ [(sid,n) | W.ThwartRemovedThreat _ sid _ n <- windows] $ \(sid,n) -> do
        schemes <- filter (/= sid) <$> selectList ThwartableScheme
        unless (null schemes) $ chooseOne you
          [targeting (schemeRef scheme) (thwartScheme scheme attrs n) | scheme <- schemes]
      pure a
    _ -> Overwatch <$> runMessage msg attrs
