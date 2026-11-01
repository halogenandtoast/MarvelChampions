module Marvel.Ally.Allies.SpiderWomanJessicaDrewSinisterMotives where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Card.Id ()
import Marvel.Id (AllyId (..))
import Marvel.Matchers
import Marvel.Query

spiderWomanJessicaDrewSinisterMotives :: AllyCard SpiderWomanJessicaDrewSinisterMotives
spiderWomanJessicaDrewSinisterMotives = ally SpiderWomanJessicaDrewSinisterMotives Cards.spiderWomanJessicaDrewSinisterMotives (Thw 1, 1) (Atk 2, 2) (HP 3)

newtype SpiderWomanJessicaDrewSinisterMotives = SpiderWomanJessicaDrewSinisterMotives (Attrs Ally)
  deriving anyclass (IsAlly, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor SpiderWomanJessicaDrewSinisterMotives where
  getModifiersFor _ (CardIdRef cid) (SpiderWomanJessicaDrewSinisterMotives attrs)
    | cid == CardId (unAllyId $ allyId attrs) = do
        n <- selectListCount ConfusedEnemy
        pure [ResourceCostReduction n]
  getModifiersFor _ _ _ = pure []

instance RunMessage SpiderWomanJessicaDrewSinisterMotives where
  runMessage msg (SpiderWomanJessicaDrewSinisterMotives attrs) = SpiderWomanJessicaDrewSinisterMotives <$> runMessage msg attrs
