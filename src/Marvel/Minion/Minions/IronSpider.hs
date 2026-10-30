module Marvel.Minion.Minions.IronSpider where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Types
import Marvel.Minion.Runner
import Marvel.Keyword

ironSpider :: MinionCard IronSpider
ironSpider = minion IronSpider Cards.ironSpider (Sch 2) (Atk 2) (HP 6)

newtype IronSpider = IronSpider (Attrs Minion)
  deriving anyclass (IsMinion, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor IronSpider where
  getModifiersFor _ (MinionRef mid) (IronSpider attrs) | mid == minionId attrs = pure [KeywordModifier Overkill]
  getModifiersFor _ _ _ = pure []

instance RunMessage IronSpider where
  runMessage msg e@(IronSpider attrs) = runQueueT $ case msg of
    _ -> IronSpider <$> runMessage msg attrs
