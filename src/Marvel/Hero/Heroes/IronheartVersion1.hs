module Marvel.Hero.Heroes.IronheartVersion1 where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Question (runAbility)
import Marvel.Resource.Types
import Marvel.Damage
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

ironheartVersion1 :: HeroCard IronheartVersion1
ironheartVersion1 = hero IronheartVersion1 Cards.ironheartVersion1 (HP $ Static 10) (HandSize 4) (Thw 1) (Atk 2) (Def 3)

newtype IronheartVersion1 = IronheartVersion1 (Attrs Hero)
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities IronheartVersion1 where
  getAbilities a = [label "Level Up!" $ ability a 1 Action (IsSelf) (IdentityCountersCost 6) (runAbility a 1)]

instance RunMessage IronheartVersion1 where
  runMessage msg h@(IronheartVersion1 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      ready you
      swapIdentityVersion you "29002a" "29002b"
      pure h
    _ -> IronheartVersion1 <$> runMessage msg attrs
