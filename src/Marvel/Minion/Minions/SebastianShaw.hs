module Marvel.Minion.Minions.SebastianShaw where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Deck ()
import Marvel.Discard ()
import Marvel.Entity ()
import Marvel.Game.Source ()
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

sebastianShaw :: MinionCard SebastianShaw
sebastianShaw = minion SebastianShaw Cards.sebastianShaw (Sch 1) (Atk 2) (HP 5)

newtype SebastianShaw = SebastianShaw (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities SebastianShaw where
  getAbilities (SebastianShaw a) = [limitedWindowAbility a 1 (W.EnemyWasAttacked W.After $ EnemyWithId $ EnemyMinionId $ minionId a)
    ForcedResponse NoCriteria NoCost (runAbility a 1)]

instance RunMessage SebastianShaw where
  runMessage msg e@(SebastianShaw attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      dealBoost attrs
      lastingModifiers attrs (MinionEntity $ MinionWithId $ minionId attrs) [CannotBeAttacked] False
      pure e
    _ -> SebastianShaw <$> runMessage msg attrs
