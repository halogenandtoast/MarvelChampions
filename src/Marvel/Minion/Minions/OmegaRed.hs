module Marvel.Minion.Minions.OmegaRed where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Status
import Marvel.Window qualified as W
import Marvel.Hand
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

omegaRed :: MinionCard OmegaRed
omegaRed = minion OmegaRed Cards.omegaRed (Sch 1) (Atk 2) (HP 8)

newtype OmegaRed = OmegaRed (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities OmegaRed where
  getAbilities (OmegaRed a) =
    [windowAbility a 1 (W.EnemyAttacked W.When (EnemyWithId $ EnemyMinionId $ minionId a) AnyIdentity) ForcedInterrupt NoCost (runAbility a 1)]

instance RunMessage OmegaRed where
  runMessage msg e@(OmegaRed attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ [you | W.EnemyAttack _ you <- windows] \you -> do
        characters <- selectList (CharacterControlledBy $ IdentityWithId you)
        damageCharacters attrs (toDamage 1 FromAbility) characters
      pure e
    _ -> OmegaRed <$> runMessage msg attrs
