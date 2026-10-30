module Marvel.Ally.Allies.Angel where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Stats
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Trait qualified as Trait
import Marvel.Ref
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner

angel :: AllyCard Angel
angel = ally Angel Cards.angel (Thw 1, 1) (Atk 2, 1) (HP 3)

newtype Angel = Angel (Attrs Ally)
  deriving anyclass (IsAlly, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor Angel where
  getModifiersFor _ (CardIdRef cid) (Angel attrs)
    | cid == CardId (unAllyId $ allyId attrs) = do
      mutant <- identityMatches (IdentityWithTrait Trait.Mutant) (allyController attrs)
      xmen <- identityMatches (IdentityWithTrait Trait.XMen) (allyController attrs)
      pure [ResourceCostReduction 1 | mutant || xmen]
  getModifiersFor _ _ _ = pure []

instance RunMessage Angel where
  runMessage msg (Angel attrs) = runQueueT $ Angel <$> runMessage msg attrs
