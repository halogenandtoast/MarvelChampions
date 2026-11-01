module Marvel.Minion.Minions.LadyDeathstrike where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query ()
import Marvel.Question
import Marvel.Ref
import Marvel.Status ()
import Marvel.Window qualified as W
import Marvel.Hand ()
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

ladyDeathstrike :: MinionCard LadyDeathstrike
ladyDeathstrike = minion LadyDeathstrike Cards.ladyDeathstrike (Sch 1) (Atk 2) (HP 6)

newtype LadyDeathstrike = LadyDeathstrike (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities LadyDeathstrike where
  getAbilities (LadyDeathstrike a) =
    [windowAbility a 1 (W.EnemyAttackedAndDamaged (EnemyWithId $ EnemyMinionId $ minionId a) AnyCharacter) ForcedResponse NoCost (runAbility a 1)]

instance RunMessage LadyDeathstrike where
  runMessage msg e@(LadyDeathstrike attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ [character | W.EnemyAttacksAndDamages _ character _ <- windows] \character -> do
        controller <- getCharacterOwner character
        for_ controller (\you -> discardFrom you RandomFromHand 1)
      pure e
    _ -> LadyDeathstrike <$> runMessage msg attrs
