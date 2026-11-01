module Marvel.Upgrade.Upgrades.DominosPistol where
import Marvel.Prelude
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Domino
import Marvel.Deck
import Marvel.Discard
import Marvel.Hand
import Marvel.Id
import Marvel.Identity.Types
import Marvel.Matchers
import Marvel.Payment
import Marvel.Projection
import Marvel.Query
import Marvel.Question hiding (Attack)
import Marvel.Resource.Types
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.GameValue
import Marvel.Stats
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

dominosPistol :: UpgradeCard DominosPistol
dominosPistol = upgrade DominosPistol Cards.dominosPistol
newtype DominosPistol = DominosPistol (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities DominosPistol where
  getAbilities a = [subtype Attack $ ability a 1 HeroAction (OwnsThis <> EnemyExists AttackableEnemy)
    (ExhaustCost <> ChooseEnemyCost AttackableEnemy <> DiscardTopOfDeckCardsCost 1 1) (runAbility a 1)]

instance RunMessage DominosPistol where
  runMessage msg a@(DominosPistol attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ payment -> do
      n <- discardedResourceCount you (paymentDiscards payment)
      let chosen = paymentTarget payment >>= \case
            VillainRef vid -> Just (EnemyVillainId vid)
            MinionRef mid -> Just (EnemyMinionId mid)
            EnemyRef enemy -> Just enemy
            _ -> Nothing
      for_ chosen $ \enemy -> dealDamage enemy attrs (withRanged $ toDamage n $ FromPlayerAttack you)
      pure a
    _ -> DominosPistol <$> runMessage msg attrs
