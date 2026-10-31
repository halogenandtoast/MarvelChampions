module Marvel.AlterEgo.AlterEgos.JeanGrey where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Stats
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.AlterEgo.Cards qualified as Cards
import Marvel.AlterEgo.Runner

jeanGrey :: AlterEgoCard JeanGrey
jeanGrey = alterEgo JeanGrey Cards.jeanGrey (HP $ Static 9) (HandSize 6) (Rec 3) [Obligations.burningHunger]

newtype JeanGrey = JeanGrey (Attrs AlterEgo)
  deriving anyclass (IsAlterEgo, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities JeanGrey where
  getAbilities (JeanGrey a) =
    [ ability a 1 Setup IsSelf NoCost (runAbility a 1)
    , limitedWindowAbility a 2 (W.UsesRecovery $ IdentityWithId $ alterEgoIdentityId a) Response IsSelf NoCost (runAbility a 2)
    ]

instance RunMessage JeanGrey where
  runMessage msg e@(JeanGrey a) = runQueueT $ case msg of
    RanAbility you (isTarget a -> True) 1 _ _ -> e <$ putPhoenixForceIntoPlay you
    RanAbility you (isTarget a -> True) 2 _ _ -> e <$ addPhoenixPower you 1
    _ -> JeanGrey <$> runMessage msg a
