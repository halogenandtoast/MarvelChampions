module Marvel.Upgrade.Upgrades.Sharpshooter where
import Marvel.Prelude
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Domino
import Marvel.Deck ()
import Marvel.Discard ()
import Marvel.Hand ()
import Marvel.Id ()
import Marvel.Identity.Types
import Marvel.Matchers
import Marvel.Payment
import Marvel.Projection ()
import Marvel.Query ()
import Marvel.Question
import Marvel.Resource.Types ()
import Marvel.Trait qualified as Trait ()
import Marvel.Window qualified as W
import Marvel.Ability
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.GameValue ()
import Marvel.Stats ()
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

sharpshooter :: UpgradeCard Sharpshooter
sharpshooter = upgrade Sharpshooter Cards.sharpshooter
newtype Sharpshooter = Sharpshooter (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Sharpshooter where
  getAbilities a@(Sharpshooter attrs) = [limitedWindowAbility a 1
    (W.MakesRangedAttack W.When $ IdentityWithId $ upgradeController attrs) HeroInterrupt OwnsThis
    (DiscardTopOfDeckCardsCost 1 1) (runAbility a 1)]

instance RunMessage Sharpshooter where
  runMessage msg a@(Sharpshooter attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ payment -> do
      n <- discardedResourceCount you (paymentDiscards payment)
      addToRangedAttackDamage you n
      pure a
    _ -> Sharpshooter <$> runMessage msg attrs
