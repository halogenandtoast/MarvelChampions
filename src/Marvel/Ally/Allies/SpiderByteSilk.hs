module Marvel.Ally.Allies.SpiderByteSilk where
import Marvel.Prelude
import Marvel.Id
import Marvel.Query ()
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Matchers ()
import Marvel.Question ()
import Marvel.Window qualified as W ()
import Marvel.Trait qualified as T
import Marvel.Damage ()
import Marvel.Game.Source
import Marvel.Card
import Marvel.Card.Id ()
import Marvel.Silk.Helpers ()

import Marvel.Ability
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Silk.Cards qualified as Cards
import Marvel.Ally.Runner

spiderByteSilk :: AllyCard SpiderByteSilk
spiderByteSilk = ally SpiderByteSilk Cards.spiderByteSilk (Thw 2, 1) (Atk 1, 1) (HP 2)
newtype SpiderByteSilk = SpiderByteSilk (Attrs Ally)
  deriving anyclass (IsAlly, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance RunMessage SpiderByteSilk where
  runMessage msg _u@(SpiderByteSilk attrs) = runQueueT $ case msg of
    _ -> SpiderByteSilk <$> runMessage msg attrs

instance HasModifiersFor SpiderByteSilk where
  getModifiersFor _ (CardIdRef cid) (SpiderByteSilk attrs) | cid == CardId (unAllyId $ allyId attrs) = do
    n <- countControlledCardsWithTrait (allyController attrs) T.Tech
    pure [ResourceCostReduction n]
  getModifiersFor _ _ _ = pure []
