module Marvel.Minion.Minions.Bonebreaker where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Window qualified as W
import Marvel.Trait qualified as Trait
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

bonebreaker :: MinionCard Bonebreaker
bonebreaker = minion Bonebreaker Cards.bonebreaker (Sch 1) (Atk 3) (HP 5)

newtype Bonebreaker = Bonebreaker (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Bonebreaker where
  getAbilities (Bonebreaker attrs) = [windowAbility attrs 1 (W.EngagesMinion W.After AnyIdentity $ MinionWithId $ minionId attrs) ForcedResponse NoCost (runAbility attrs 1)]

instance RunMessage Bonebreaker where
  runMessage msg m@(Bonebreaker attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      let you = minionEngagedIdentity attrs
      minions <- selectList (MinionWithTrait Trait.Reaver <> MinionEngagedWith (IdentityWithId you))
      dealIndirectDamage you attrs (fromIntegral $ length minions)
      pure m
    _ -> Bonebreaker <$> runMessage msg attrs
