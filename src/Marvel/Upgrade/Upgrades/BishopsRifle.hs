module Marvel.Upgrade.Upgrades.BishopsRifle where

import Marvel.Prelude
import Marvel.Id
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Hand
import Marvel.Identity.Types (Field (PlayerIdentityHand))
import Marvel.Matchers
import Marvel.Payment
import Marvel.Projection
import Marvel.Question hiding (Attack)
import Marvel.Ability
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

bishopsRifle :: UpgradeCard BishopsRifle
bishopsRifle = upgrade BishopsRifle Cards.bishopsRifle

newtype BishopsRifle = BishopsRifle (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities BishopsRifle where
  getAbilities a = [subtype Attack $ ability a 1 HeroAction (OwnsThis <> ExtendedCardExists (InHandOf You $ BasicCardMatches $ CardWithType ResourceType))
    (ExhaustCost <> ChooseEnemyCost AttackableEnemy) (runAbility a 1)]

instance RunMessage BishopsRifle where
  runMessage msg a@(BishopsRifle attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ payment -> do
      n <- fromIntegral . length . filter (cardMatch $ CardWithType ResourceType) <$> projectMap PlayerIdentityHand unHand you
      for_ (paymentTarget payment >>= \case
        VillainRef vid -> Just (EnemyVillainId vid)
        MinionRef mid -> Just (EnemyMinionId mid)
        EnemyRef enemy -> Just enemy
        _ -> Nothing) $ \enemy -> dealDamage enemy attrs (withRanged $ toDamage n $ FromPlayerAttack you)
      pure a
    _ -> BishopsRifle <$> runMessage msg attrs
