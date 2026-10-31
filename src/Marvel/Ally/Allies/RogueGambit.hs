module Marvel.Ally.Allies.RogueGambit where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Card.Id
import Marvel.Game.Source
import Marvel.Id
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref

rogueGambit :: AllyCard RogueGambit
rogueGambit = ally RogueGambit Cards.rogueGambit (Thw 2, 1) (Atk 2, 1) (HP 3)

newtype RogueGambit = RogueGambit (Attrs Ally)
  deriving anyclass (IsAlly, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor RogueGambit where
  getModifiersFor _ (CardIdRef cid) (RogueGambit attrs) | cid == CardId (unAllyId $ allyId attrs) = do
    n <- getCountersOn $ allyController attrs
    pure [ResourceCostReduction n, ContinuouslyRecalculateResourceCost]
  getModifiersFor _ _ _ = pure []

instance RunMessage RogueGambit where
  runMessage msg (RogueGambit attrs) = runQueueT $ RogueGambit <$> runMessage msg attrs
