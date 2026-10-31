module Marvel.AlterEgo.AlterEgos.Logan where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.AlterEgo.Cards qualified as Cards
import Marvel.AlterEgo.Runner
import Marvel.Stats
import Marvel.GameValue
import Marvel.Hand
import Marvel.Cost
import Marvel.Criteria
import Marvel.Question
import Marvel.Obligation.Cards qualified as Obligations

logan :: AlterEgoCard Logan
logan = alterEgo Logan Cards.logan (HP $ Static 10) (HandSize 6) (Rec 6) [Obligations.pastDemons]

newtype Logan = Logan (Attrs AlterEgo)
  deriving anyclass (IsAlterEgo, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Logan where
  getAbilities a = [ability a 1 Setup IsSelf NoCost (runAbility a 1)]

instance RunMessage Logan where
  runMessage msg ae@(Logan attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> ae <$ putCardFromSetAsideIntoPlay you "35002"
    _ -> Logan <$> runMessage msg attrs
