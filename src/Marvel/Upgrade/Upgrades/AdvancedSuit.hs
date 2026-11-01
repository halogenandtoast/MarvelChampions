module Marvel.Upgrade.Upgrades.AdvancedSuit where

import Marvel.Prelude
import Marvel.Query
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Payment
import Marvel.Question
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Ability
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

advancedSuit :: UpgradeCard AdvancedSuit
advancedSuit = upgrade AdvancedSuit Cards.advancedSuit

newtype AdvancedSuit = AdvancedSuit (Attrs Upgrade)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities AdvancedSuit where
  getAbilities a@(AdvancedSuit attrs) =
    [limitedWindowAbility a 1
      (W.WindowOneOf [W.MinionDefeatedBy W.After AnyMinion (SourceIs $ AllyRef aid),
        W.SideSchemeDefeatedBy AnySideScheme (SourceIs $ AllyRef aid)])
      Response OwnsThis (DiscardHandCardsCost 1 $ Just 1) (runAbility a 1)
    | Just aid <- [upgradeAttachedAlly attrs]]
instance IsUpgrade AdvancedSuit where
  upgradeTargets _ = do
    force <- selectList (AllyWithTrait Trait.XForce <> NotAlly (AllyWithUpgrade $ UpgradeIs Cards.advancedSuit))
    men <- selectList (AllyWithTrait Trait.XMen <> NotAlly (AllyWithUpgrade $ UpgradeIs Cards.advancedSuit))
    pure $ Just $ map toTarget $ force <> filter (`notElem` force) men
instance RunMessage AdvancedSuit where
  runMessage msg a@(AdvancedSuit attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ payment -> do
      let n = fromIntegral $ sum $ map (length . printedResources . getCardDef) $ paymentDiscards payment
      for_ (upgradeAttachedAlly attrs) $ \aid -> heal aid n
      pure a
    _ -> AdvancedSuit <$> runMessage msg attrs
