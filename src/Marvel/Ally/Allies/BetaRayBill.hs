module Marvel.Ally.Allies.BetaRayBill (
  betaRayBill,
  BetaRayBill (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Window

betaRayBill :: AllyCard BetaRayBill
betaRayBill = ally BetaRayBill Cards.betaRayBill (Thw 1, 1) (Atk 3, 1) (HP 4)

newtype BetaRayBill = BetaRayBill (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | "Response: After Beta Ray Bill attacks and defeats a minion, remove 2 threat from
-- the main scheme."
instance HasAbilities BetaRayBill where
  getAbilities a@(BetaRayBill attrs) =
    [ limitedWindowAbility
        a
        1
        (EnemyDefeated After MinionEnemy . AttackFromAlly $ AllyWithId (allyId attrs))
        Response
        OwnsThis
        NoCost
        (RemoveThreat (toSource a) 2 MainScheme)
    ]

instance RunMessage BetaRayBill where
  runMessage msg (BetaRayBill attrs) = BetaRayBill <$> runMessage msg attrs
