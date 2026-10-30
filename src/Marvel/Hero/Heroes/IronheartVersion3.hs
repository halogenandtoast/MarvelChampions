module Marvel.Hero.Heroes.IronheartVersion3 where

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

ironheartVersion3 :: HeroCard IronheartVersion3
ironheartVersion3 = hero IronheartVersion3 Cards.ironheartVersion3 (HP $ Static 10) (HandSize 6) (Thw 3) (Atk 2) (Def 3)

newtype IronheartVersion3 = IronheartVersion3 (Attrs Hero)
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities IronheartVersion3 where
  getAbilities a = [label "Maximum Efficiency" $ ability a 1 HeroAction (IsSelf <> EnemyExists AnyEnemy) (IdentityCountersCost 1) (runAbility a 1)]

instance RunMessage IronheartVersion3 where
  runMessage msg h@(IronheartVersion3 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      chooseDamage you attrs (toDamage 2 FromAbility) AnyEnemy
      pure h
    _ -> IronheartVersion3 <$> runMessage msg attrs
