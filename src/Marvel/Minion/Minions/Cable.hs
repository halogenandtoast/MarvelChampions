module Marvel.Minion.Minions.Cable where

import Marvel.Prelude
import Data.List (delete)
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Status
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Payment
import Marvel.Query
import Marvel.Resource
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Keyword qualified as Keyword
import {-# SOURCE #-} Marvel.Game (getPlayerVictoryDisplay, getVictoryDisplay, sideSchemesInVictoryDisplay)

import Marvel.Question (runAbility, Choice (Pay))
import Marvel.Ability
import Marvel.Modifier
import Marvel.Entity
import Marvel.Card
import Marvel.Ref
import Marvel.Message.Lifted
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Types
import Marvel.Stats
import Marvel.GameValue
import Marvel.Hand
import Marvel.Minion.Runner

cable40032 :: MinionCard Cable40032
cable40032 = minion Cable40032 Cards.cable40032 (Sch 2) (Atk 2) (HP 5)

newtype Cable40032 = Cable40032 (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Cable40032 where
  runMessage msg a@(Cable40032 attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ [eid | W.EventPlayed _ eid _ <- windows] cancelEventEffects
      dealDamage (minionId attrs) attrs (toDamage 1 FromAbility)
      pure a
    _ -> Cable40032 <$> runMessage msg attrs

instance HasAbilities Cable40032 where
  getAbilities a@(Cable40032 attrs) =
    [limitedWindowAbility a 1 (W.PlaysEvent W.When AnyIdentity $ CardWithTrait Trait.Psionic) ForcedInterrupt NoCriteria NoCost (runAbility a 1)]
