module Marvel.Ally.Allies.Outlaw where
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

outlaw :: AllyCard Outlaw
outlaw = ally Outlaw Cards.outlaw (Thw 1, 1) (Atk 1, 1) (HP 3)
newtype Outlaw = Outlaw (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Outlaw where
  getAbilities a@(Outlaw attrs) = [limitedWindowAbility a 1
    (W.AllyAttacked W.When (AllyWithId $ allyId attrs) AnyEnemy) Interrupt OwnsThis
    (DiscardTopOfDeckCardsCost 1 1) (runAbility a 1)]

instance RunMessage Outlaw where
  runMessage msg a@(Outlaw attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ payment -> do
      n <- discardedResourceCount you (paymentDiscards payment)
      addToAllyBasicPower (allyId attrs) n
      pure a
    _ -> Outlaw <$> runMessage msg attrs
