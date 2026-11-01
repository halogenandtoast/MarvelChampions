module Marvel.Ally.Allies.Feral where
import Marvel.Prelude
import Marvel.Card
import Marvel.Game.Source ()
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Domino
import Marvel.Deck ()
import Marvel.Discard ()
import Marvel.Hand ()
import Marvel.Id ()
import Marvel.Identity.Types
import Marvel.Matchers
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Query
import Marvel.Question
import Marvel.Resource.Types ()
import Marvel.Trait qualified as Trait ()
import Marvel.Window qualified as W
import Marvel.Ability
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.GameValue ()
import Marvel.Stats
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner

feral :: AllyCard Feral
feral = ally Feral Cards.feral (Thw 2, 1) (Atk 2, 1) (HP 3)
newtype Feral = Feral (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Feral where
  getAbilities a@(Feral attrs) = [limitedWindowAbility a 1
    (W.AllyThwarted W.After (AllyWithId $ allyId attrs) AnyScheme) Response OwnsThis NoCost (runAbility a 1)]

instance RunMessage Feral where
  runMessage msg a@(Feral attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> a <$ discardFromDeck you 1 attrs
    WithDiscarded (isTarget attrs -> True) FromDeck cards -> do
      n <- discardedResourceCount (allyController attrs) cards
      villains <- selectList VillainEnemy
      chooseOne (allyController attrs) [targeting (enemyRef enemy) (dealDamage enemy attrs $ toDamage n FromAbility) | enemy <- villains]
      pure a
    _ -> Feral <$> runMessage msg attrs
