module Marvel.Support.Supports.InterrogationRoom (
  interrogationRoom,
  InterrogationRoom (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types
import Marvel.Window qualified as W

interrogationRoom :: SupportCard InterrogationRoom
interrogationRoom = support InterrogationRoom Cards.interrogationRoom

newtype InterrogationRoom = InterrogationRoom (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities InterrogationRoom where
  getAbilities a =
    [ limitedWindowAbility
        a
        1
        (W.MinionDefeated W.After AnyMinion)
        Response
        OwnsThis
        ExhaustCost
        $ RemoveThreat (toSource a) 1 ThwartableScheme
    ]

instance RunMessage InterrogationRoom where
  runMessage msg (InterrogationRoom attrs) =
    InterrogationRoom <$> runMessage msg attrs
