module Marvel.AlterEgo.AlterEgos.PeterPorker where

import Marvel.Prelude
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Question
import Marvel.Window qualified as W
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.AlterEgo.Cards qualified as Cards
import Marvel.AlterEgo.Runner
import Marvel.GameValue
import Marvel.Hand
import Marvel.Stats
import Marvel.Obligation.Cards qualified as Obligations

peterPorker :: AlterEgoCard PeterPorker
peterPorker = alterEgo PeterPorker Cards.peterPorker (HP $ Static 12) (HandSize 6) (Rec 5) [Obligations.iReallyWantAHotDog]

newtype PeterPorker = PeterPorker (Attrs AlterEgo)
  deriving anyclass (IsAlterEgo, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities PeterPorker where
  getAbilities a@(PeterPorker attrs) =
    [ label "$label.cartoonPower" $ limitedWindowAbility a 1
        (W.UsesRecovery $ IdentityWithId $ alterEgoIdentityId attrs)
        Response IsSelf NoCost (runAbility a 1)
    ]

instance RunMessage PeterPorker where
  runMessage msg h@(PeterPorker attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> h <$ placeCounters you 1
    _ -> PeterPorker <$> runMessage msg attrs
