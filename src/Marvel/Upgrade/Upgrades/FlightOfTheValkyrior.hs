module Marvel.Upgrade.Upgrades.FlightOfTheValkyrior (flightOfTheValkyrior, FlightOfTheValkyrior (..)) where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Window qualified as W

flightOfTheValkyrior :: UpgradeCard FlightOfTheValkyrior
flightOfTheValkyrior = upgrade FlightOfTheValkyrior Cards.flightOfTheValkyrior

newtype FlightOfTheValkyrior = FlightOfTheValkyrior (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities FlightOfTheValkyrior where
  getAbilities a =
    [ limitedWindowAbility a 1 (W.EnemyDefeatedWithUpgrade W.After Cards.deathGlow AnyDamage)
        Response (OwnsThis <> SchemeExists ThwartableScheme) (DiscardCost $ toTarget a) (runAbility a 1)
    ]

instance RunMessage FlightOfTheValkyrior where
  runMessage msg u@(FlightOfTheValkyrior attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> do
      removeThreat ident attrs 5 ThwartableScheme
      pure u
    _ -> FlightOfTheValkyrior <$> runMessage msg attrs
