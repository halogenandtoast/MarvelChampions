module Marvel.Support.Supports.WarRoom where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Question (runAbility)
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types
import Marvel.Window qualified as W

warRoom :: SupportCard WarRoom
warRoom = support WarRoom Cards.warRoom

newtype WarRoom = WarRoom (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities WarRoom where
  getAbilities a =
    [ limitedWindowAbility a 1
        (W.EnemyDefeated W.After MinionEnemy $ AttackFromAlly AnyAlly)
        Response (OwnsThis <> SchemeExists ThwartableScheme)
        ExhaustCost (runAbility a 1)
    ]

instance RunMessage WarRoom where
  runMessage msg a@(WarRoom attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> a <$ removeThreat you attrs 1 ThwartableScheme
    _ -> WarRoom <$> runMessage msg attrs
