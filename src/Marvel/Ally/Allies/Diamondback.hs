module Marvel.Ally.Allies.Diamondback where
import Marvel.Prelude
import Marvel.Card
import Marvel.Game.Source
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
import Marvel.Question
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
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner

diamondback :: AllyCard Diamondback
diamondback = ally Diamondback Cards.diamondback (Thw 1, 1) (Atk 1, 1) (HP 2)
newtype Diamondback = Diamondback (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Diamondback where
  getAbilities a = [ability a 1 Action (OwnsThis <> EnemyExists DamageableEnemy)
    (ExhaustCost <> (DiscardTopOfDeckCardsCost 1 1) <> DamageThisCost 1) (runAbility a 1)]

instance RunMessage Diamondback where
  runMessage msg a@(Diamondback attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ payment -> do
      n <- discardedResourceCount you (paymentDiscards payment)
      enemies <- selectList DamageableEnemy
      dealDamageToEach enemies attrs (toDamage n FromAbility)
      pure a
    _ -> Diamondback <$> runMessage msg attrs
