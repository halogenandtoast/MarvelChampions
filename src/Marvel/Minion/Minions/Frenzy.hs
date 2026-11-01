module Marvel.Minion.Minions.Frenzy where

import Marvel.Prelude
import Marvel.Ability
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

frenzy :: MinionCard Frenzy
frenzy = minion Frenzy Cards.frenzy (Sch 2) (Atk 2) (HP 4)

newtype Frenzy = Frenzy (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Frenzy where
  getAbilities (Frenzy a) = [limitedWindowAbility a 1 (W.EnemyAttacked W.After (EnemyWithId $ EnemyMinionId $ minionId a) AnyIdentity)
    ForcedResponse NoCriteria NoCost (runAbility a 1)]

instance RunMessage Frenzy where
  runMessage msg e@(Frenzy attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ (concatMap (\case W.EnemyAttack enemy you | enemy == EnemyMinionId (minionId attrs) -> [(enemy,you)]; W.EnemyAttackAlly enemy _ you | enemy == EnemyMinionId (minionId attrs) -> [(enemy,you)]; _ -> []) windows) $ \(_enemy, you) -> do
        discardTopOfDeck you 2
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      you <- getBoostedPlayer enemy
      discardTopOfDeck you 4
      pure e
    _ -> Frenzy <$> runMessage msg attrs
