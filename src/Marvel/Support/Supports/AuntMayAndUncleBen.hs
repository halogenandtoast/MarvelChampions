module Marvel.Support.Supports.AuntMayAndUncleBen where

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
import Marvel.Payment
import Marvel.Card
import Marvel.Question (runAbility)

auntMayAndUncleBen :: SupportCard AuntMayAndUncleBen
auntMayAndUncleBen = support AuntMayAndUncleBen Cards.auntMayAndUncleBen

newtype AuntMayAndUncleBen = AuntMayAndUncleBen (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities AuntMayAndUncleBen where
  getAbilities a =
    [ ability a 1 Action (OwnsThis <> SelfMatches HeroIdentity) (Costs [ExhaustCost, DiscardTopOfDeckCardsCost 2 2]) (runAbility a 1)
    , ability a 2 Action (OwnsThis <> SelfMatches AlterEgoIdentity) (Costs [ExhaustCost, DiscardTopOfDeckCardsCost 3 3]) (runAbility a 2)
    ]

instance RunMessage AuntMayAndUncleBen where
  runMessage msg e@(AuntMayAndUncleBen attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) _ _ payment -> do
      for_ [c | PlayerCard c <- paymentDiscards payment, toBaseCardCode (toCardCode c) `elem` ["31003", "31004", "31005", "31006", "31007", "31008", "31009", "31010", "31011", "31012", "31013"]] $ addToHand you
      pure e
    _ -> AuntMayAndUncleBen <$> runMessage msg attrs
