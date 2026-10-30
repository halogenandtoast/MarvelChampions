module Marvel.Support.Supports.EjectionProtocol where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Query
import Marvel.Trait
import Marvel.Status
import Marvel.Question (runAbility)

ejectionProtocol :: SupportCard EjectionProtocol
ejectionProtocol = support EjectionProtocol Cards.ejectionProtocol

newtype EjectionProtocol = EjectionProtocol (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities EjectionProtocol where
  getAbilities a@(EjectionProtocol attrs) = [ability a 1 HeroAction OwnsThis (DiscardCost $ toTarget attrs) (runAbility a 1)]

instance RunMessage EjectionProtocol where
  runMessage msg e@(EjectionProtocol attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      interfaces <- selectList (UpgradeWithTrait Interface <> upgradeControlledBy you)
      traverse_ exhaust interfaces
      setHitPoints you 6
      giveStatus you ToughStatus
      changeToAlterEgo you
      pure e
    _ -> EjectionProtocol <$> runMessage msg attrs
