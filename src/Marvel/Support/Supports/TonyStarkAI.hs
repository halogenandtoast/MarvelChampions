module Marvel.Support.Supports.TonyStarkAI where

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
import Marvel.Card.Def
import Marvel.Question (runAbility)

tonyStarkAI :: SupportCard TonyStarkAI
tonyStarkAI = support TonyStarkAI Cards.tonyStarkAI

newtype TonyStarkAI = TonyStarkAI (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities TonyStarkAI where
  getAbilities a = [ability a 1 Action OwnsThis ExhaustCost (runAbility a 1)]

instance RunMessage TonyStarkAI where
  runMessage msg e@(TonyStarkAI attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      search you (SearchIdentityDeck you $ TopOfDeck 2) AnyCard SearchDrawOne DiscardRest
      pure e
    _ -> TonyStarkAI <$> runMessage msg attrs
