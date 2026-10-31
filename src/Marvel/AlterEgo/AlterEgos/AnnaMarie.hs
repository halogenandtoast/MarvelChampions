module Marvel.AlterEgo.AlterEgos.AnnaMarie where

import Marvel.Prelude
import Marvel.Card.Code
import Marvel.Ability
import Marvel.AlterEgo.Cards qualified as Cards
import Marvel.AlterEgo.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.Question
import Marvel.Ref
import Marvel.Stats
import Marvel.Window qualified as W

annaMarie :: AlterEgoCard AnnaMarie
annaMarie = alterEgo AnnaMarie Cards.annaMarie (HP $ Static 11) (HandSize 6) (Rec 3) [Obligations.deadlyTouch]

newtype AnnaMarie = AnnaMarie (Attrs AlterEgo)
  deriving anyclass (IsAlterEgo, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities AnnaMarie where
  getAbilities a@(AnnaMarie attrs) =
    [ ability a 1 Setup NoCriteria NoCost (runAbility a 1)
    , limitedWindowAbility a 2 (W.IdentityChangedToForm W.After $ IdentityWithId $ alterEgoIdentityId attrs)
        ForcedResponse IsSelf NoCost (runAbility a 2)
    ]

instance RunMessage AnnaMarie where
  runMessage msg a@(AnnaMarie attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) n _ _ | n `elem` [1, 2] -> a <$ setAsideTouched you
    _ -> AnnaMarie <$> runMessage msg attrs
