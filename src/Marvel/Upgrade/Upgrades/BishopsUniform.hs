module Marvel.Upgrade.Upgrades.BishopsUniform where

import Marvel.Prelude
import Marvel.Query
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Hand
import Marvel.Hero.Cards qualified as Heroes
import Marvel.Identity.Types (Field (PlayerIdentityHand))
import Marvel.Matchers
import Marvel.Projection
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

bishopsUniform :: UpgradeCard BishopsUniform
bishopsUniform = upgrade BishopsUniform Cards.bishopsUniform

newtype BishopsUniform = BishopsUniform (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities BishopsUniform where
  getAbilities a@(BishopsUniform attrs) =
    [limitedWindowAbility a 1 (W.ResolvesAbility W.After (IdentityWithId $ upgradeController attrs) (CardIs Heroes.bishopHero))
      Response (OwnsThis <> SelfMatches IdentityWithAnyDamage <> ExtendedCardExists (InHandOf You $ BasicCardMatches $ CardWithType ResourceType)) ExhaustCost (runAbility a 1)]

instance RunMessage BishopsUniform where
  runMessage msg a@(BishopsUniform attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      n <- fromIntegral . length . filter (cardMatch $ CardWithType ResourceType) <$> projectMap PlayerIdentityHand unHand you
      targets <- selectList $ IdentityIs Heroes.bishopHero
      for_ targets $ \bishop -> heal bishop n
      pure a
    _ -> BishopsUniform <$> runMessage msg attrs
