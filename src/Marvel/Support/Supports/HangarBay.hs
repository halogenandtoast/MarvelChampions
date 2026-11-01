module Marvel.Support.Supports.HangarBay where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query ()
import Marvel.Ref
import Marvel.Question
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types
import Marvel.Window qualified as W

hangarBay :: SupportCard HangarBay
hangarBay = support HangarBay Cards.hangarBay

newtype HangarBay = HangarBay (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities HangarBay where
  getAbilities a = [limitedWindowAbility a 1 (W.AllyDefendsAgainst W.After (Marvel.Matchers.ExhaustedAlly <> AllyCanReady) AnyEnemy)
    Response OwnsThis ExhaustCost (runAbility a 1)]

instance RunMessage HangarBay where
  runMessage msg a@(HangarBay attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget a -> True) 1 windows _ -> do
      for_ [aid | W.AllyDefendedAgainst aid _ <- windows] ready
      pure a
    _ -> HangarBay <$> runMessage msg attrs
