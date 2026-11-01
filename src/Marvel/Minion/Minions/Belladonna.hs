module Marvel.Minion.Minions.Belladonna where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query ()
import Marvel.Question
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Window qualified as W
import Marvel.Trait qualified as Trait ()
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

belladonna :: MinionCard Belladonna
belladonna = minion Belladonna Cards.belladonna (Sch 2) (Atk 3) (HP 5)

newtype Belladonna = Belladonna (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Belladonna where
  getAbilities (Belladonna attrs) =
    [windowAbility attrs 1 (W.CharacterDefeatedByEnemyAttack W.After $ EnemyWithId $ EnemyMinionId $ minionId attrs)
      ForcedResponse NoCost (runAbility attrs 1)]

instance RunMessage Belladonna where
  runMessage msg m@(Belladonna attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      placeThreatOnEach attrs 2 EncounterMainScheme
      pure m
    _ -> Belladonna <$> runMessage msg attrs
