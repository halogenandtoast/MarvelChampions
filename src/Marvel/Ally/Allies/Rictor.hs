module Marvel.Ally.Allies.Rictor where

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
import Marvel.Window qualified as W
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Domino

rictor :: AllyCard Rictor
rictor = ally Rictor Cards.rictor (Thw 1, 1) (Atk 2, 1) (HP 3)

newtype Rictor = Rictor (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Rictor where
  getAbilities a@(Rictor attrs) = [limitedWindowAbility a 1
    (W.AllyAttacked W.After (AllyWithId $ allyId attrs) AnyEnemy)
    Response OwnsThis NoCost (runAbility a 1)]

instance RunMessage Rictor where
  runMessage msg a@(Rictor attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> a <$ discardFromDeck you 1 attrs
    WithDiscarded (isTarget attrs -> True) FromDeck cards -> do
      n <- discardedResourceCount (allyController attrs) cards
      enemies <- selectList (MinionEngagedWith (IdentityWithId $ allyController attrs))
      dealDamageToEach (map EnemyMinionId enemies) attrs (toDamage n FromAbility)
      pure a
    _ -> Rictor <$> runMessage msg attrs
