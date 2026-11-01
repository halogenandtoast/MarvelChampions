module Marvel.Hero.Heroes.IronheartVersion2 where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Matchers ()
import Marvel.Question (runAbility)
import Marvel.Resource.Types ()
import Marvel.Damage ()
import Marvel.Hero.Cards qualified as Cards
import Marvel.Hero.Runner
import Marvel.Card.Code
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Stats

ironheartVersion2 :: HeroCard IronheartVersion2
ironheartVersion2 = hero IronheartVersion2 Cards.ironheartVersion2 (HP $ Static 10) (HandSize 5) (Thw 2) (Atk 2) (Def 3)

newtype IronheartVersion2 = IronheartVersion2 (Attrs Hero)
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities IronheartVersion2 where
  getAbilities a = [label "Level Up!" $ ability a 1 Action (IsSelf) (IdentityCountersCost 6) (runAbility a 1)]

instance RunMessage IronheartVersion2 where
  runMessage msg h@(IronheartVersion2 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      ready you
      becomeTough you
      swapIdentityVersion you "29003a" "29003b"
      pure h
    _ -> IronheartVersion2 <$> runMessage msg attrs
