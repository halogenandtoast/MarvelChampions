module Marvel.Support.Supports.ChimichangaTruck where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types
import Marvel.Window qualified as W

chimichangaTruck :: SupportCard ChimichangaTruck
chimichangaTruck = support ChimichangaTruck Cards.chimichangaTruck

newtype ChimichangaTruck = ChimichangaTruck (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities ChimichangaTruck where
  getAbilities a =
    [ limitedWindowAbility a 1 (W.UsesRecoveryAt W.After IdentityCanReady) Response OwnsThis ExhaustCost (runAbility a 1)
    ]

instance RunMessage ChimichangaTruck where
  runMessage msg s@(ChimichangaTruck attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ [you | W.UsedRecovery you <- windows] ready
      pure s
    _ -> ChimichangaTruck <$> runMessage msg attrs
