module Marvel.Minion.Minions.ExodusMagneto where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Attack (attackValue)
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Deck ()
import Marvel.Discard ()
import Marvel.Entity ()
import Marvel.Game.Source
import Marvel.GameValue ()
import Marvel.Hand ()
import Marvel.Id ()
import Marvel.Keyword ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier ()
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Query ()
import Marvel.Question ()
import Marvel.Ref ()
import Marvel.Resource.Types ()
import Marvel.Stats ()
import Marvel.Status ()
import Marvel.Trait ()
import Marvel.Window qualified as W
import Marvel.Identity.Types ()

import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

exodusMagneto :: MinionCard ExodusMagneto
exodusMagneto = minion ExodusMagneto Cards.exodusMagneto (Sch 2) (Atk 2) (HP 6)

newtype ExodusMagneto = ExodusMagneto (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities ExodusMagneto where
  getAbilities (ExodusMagneto a) = [limitedWindowAbility a 1 (W.EnemyAttacked W.After (EnemyWithId $ EnemyMinionId $ minionId a) AnyIdentity)
    ForcedResponse NoCriteria NoCost (runAbility a 1)]

instance RunMessage ExodusMagneto where
  runMessage msg e@(ExodusMagneto attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ (concatMap (\case W.EnemyAttack enemy you | enemy == EnemyMinionId (minionId attrs) -> [(enemy,you)]; W.EnemyAttackAlly enemy _ you | enemy == EnemyMinionId (minionId attrs) -> [(enemy,you)]; _ -> []) windows) $ \(enemy, you) -> do
        n <- maybe 0 attackValue <$> getEnemyAttack enemy
        discardTopOfDeck you n
      pure e
    _ -> ExodusMagneto <$> runMessage msg attrs
