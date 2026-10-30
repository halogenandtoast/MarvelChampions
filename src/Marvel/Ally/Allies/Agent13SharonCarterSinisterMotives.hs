module Marvel.Ally.Allies.Agent13SharonCarterSinisterMotives where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Query
import Marvel.Question (runAbility)
import Marvel.Trait (Trait (Shield))
import Marvel.Window qualified as W

agent13SharonCarterSinisterMotives :: AllyCard Agent13SharonCarterSinisterMotives
agent13SharonCarterSinisterMotives = ally Agent13SharonCarterSinisterMotives Cards.agent13SharonCarterSinisterMotives (Thw 2, 1) (Atk 1, 1) (HP 4)

newtype Agent13SharonCarterSinisterMotives = Agent13SharonCarterSinisterMotives (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Agent13SharonCarterSinisterMotives where
  getAbilities a@(Agent13SharonCarterSinisterMotives attrs) =
    [ limitedWindowAbility a 1
        (W.WindowOneOf [W.AllyAttacked W.After (AllyWithId $ allyId attrs) AnyEnemy, W.AllyThwarted W.After (AllyWithId $ allyId attrs) AnyScheme])
        Response (OwnsThis <> SupportExists (SupportMatches [SupportWithTrait Shield, SupportWithExhaustion True])) NoCost (runAbility a 1) ]

instance RunMessage Agent13SharonCarterSinisterMotives where
  runMessage msg a@(Agent13SharonCarterSinisterMotives attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      supports <- selectList (SupportMatches [SupportWithTrait Shield, SupportWithExhaustion True])
      unless (null supports) $ chooseOne you [targeting support (ready support) | support <- supports]
      pure a
    _ -> Agent13SharonCarterSinisterMotives <$> runMessage msg attrs
