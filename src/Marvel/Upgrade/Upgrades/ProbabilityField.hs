module Marvel.Upgrade.Upgrades.ProbabilityField where
import Marvel.Prelude
import Marvel.Card
import Marvel.Game.Source ()
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

probabilityField :: UpgradeCard ProbabilityField
probabilityField = upgrade ProbabilityField Cards.probabilityField
newtype ProbabilityField = ProbabilityField (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities ProbabilityField where
  getAbilities a@(ProbabilityField attrs) = [limitedWindowAbility a 1
    (W.WindowOneOf [W.MakesBasicAttack W.When you, W.MakesBasicThwart W.When you,
      W.MakesBasicDefense W.When you AnyEnemy, W.UsesRecoveryAt W.When you])
    Interrupt OwnsThis (DiscardTopOfDeckCardsCost 1 1) (runAbility a 1)]
    where you = IdentityWithId $ upgradeController attrs

instance RunMessage ProbabilityField where
  runMessage msg a@(ProbabilityField attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ payment -> do
      n <- discardedResourceCount you (paymentDiscards payment)
      addToBasicPower you n
      pure a
    _ -> ProbabilityField <$> runMessage msg attrs
