module Marvel.Ally.Allies.Cannonball where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Window qualified as W
import Marvel.Damage
import Marvel.Trait qualified as Trait
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner

cannonball :: AllyCard Cannonball
cannonball = ally Cannonball Cards.cannonball (Thw 2, 2) (Atk 2, 2) (HP 2)

newtype Cannonball = Cannonball (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Cannonball where
  getAbilities a@(Cannonball attrs) = [limitedWindowAbility a 1
    (W.AllyWouldTakeDamageOf W.When (AllyWithId $ allyId attrs) FromConsequential)
    Interrupt (OwnsThis <> ExtendedCardExists (InHandOf You $ BasicCardMatches $ CardWithTrait Trait.Aerial)) NoCost (runAbility a 1)]

instance RunMessage Cannonball where
  runMessage msg a@(Cannonball attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      cards <- selectList (InHandOf (IdentityWithId you) $ BasicCardMatches $ CardWithTrait Trait.Aerial)
      preventAllyDamage (allyId attrs) (fromIntegral $ length cards)
      pure a
    _ -> Cannonball <$> runMessage msg attrs
