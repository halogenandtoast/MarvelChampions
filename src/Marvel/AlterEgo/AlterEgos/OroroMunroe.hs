module Marvel.AlterEgo.AlterEgos.OroroMunroe where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Question
import Marvel.Cost
import Marvel.Criteria
import Marvel.Support.Cards qualified as Supports
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.AlterEgo.Cards qualified as Cards
import Marvel.AlterEgo.Runner
import Marvel.GameValue
import Marvel.Hand
import Marvel.Stats
import Marvel.Obligation.Cards qualified as Obligations

ororoMunroe :: AlterEgoCard OroroMunroe
ororoMunroe = alterEgo OroroMunroe Cards.ororoMunroe (HP $ Static 10) (HandSize 6) (Rec 3) [Obligations.claustrophobia]

newtype OroroMunroe = OroroMunroe (Attrs AlterEgo)
  deriving anyclass (IsAlterEgo, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities OroroMunroe where
  getAbilities a = [ability a 1 Setup NoCriteria NoCost (runAbility a 1)]

instance RunMessage OroroMunroe where
  runMessage msg a@(OroroMunroe attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> a <$ setupWeatherDeck you [Supports.clearSkies, Supports.hurricane, Supports.thunderstorm, Supports.blizzard]
    _ -> OroroMunroe <$> runMessage msg attrs
