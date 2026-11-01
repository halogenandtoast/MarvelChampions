module Marvel.Ally.Allies.Dogpool where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.Id ()
import Marvel.Keyword ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query ()
import Marvel.Ref
import Marvel.Trait qualified as Trait ()
import Marvel.Window qualified as W
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Stats ()

dogpool :: AllyCard Dogpool
dogpool = ally Dogpool Cards.dogpool (Thw 0, 0) (Atk 1, 0) (HP 4)

newtype Dogpool = Dogpool (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Dogpool where
  getAbilities a@(Dogpool attrs) =
    [limitedWindowAbility a 1 (W.AllyDefeated W.When $ AllyWithId $ allyId attrs)
      ForcedInterrupt OwnsThis NoCost (runAbility a 1)]


instance RunMessage Dogpool where
  runMessage msg e@(Dogpool attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      chooseDamage you attrs (toDamage 1 FromAbility) AnyEnemy
      pure e
    _ -> Dogpool <$> runMessage msg attrs
