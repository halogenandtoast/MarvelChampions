module Marvel.Ally.Allies.TChallaShuri where
import Marvel.Prelude
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Question ()
import Marvel.Window qualified as W
import Marvel.Trait qualified as T
import Marvel.Damage ()
import Marvel.Game.Source ()
import Marvel.BlackPantherShuri.Helpers

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.BlackPantherShuri.Cards qualified as Cards
import Marvel.Ally.Runner

tChallaShuri :: AllyCard TChallaShuri
tChallaShuri = ally TChallaShuri Cards.tChallaShuri (Thw 2, 1) (Atk 2, 1) (HP 3)
newtype TChallaShuri = TChallaShuri (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance RunMessage TChallaShuri where
  runMessage msg u@(TChallaShuri attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> u <$ choosePantherSpecial you
    _ -> TChallaShuri <$> runMessage msg attrs

instance HasAbilities TChallaShuri where
  getAbilities u@(TChallaShuri attrs) = [limitedWindowAbility u 1 (W.WindowOneOf [W.AllyAttacked W.After (AllyWithId $ allyId attrs) AnyEnemy, W.AllyThwarted W.After (AllyWithId $ allyId attrs) AnyScheme, W.AllyDefendsAgainst W.After (AllyWithId $ allyId attrs) AnyEnemy]) HeroResponse (OwnsThis <> UpgradeExists (UpgradeWithTrait T.BlackPanther <> UpgradeControlledBy You <> UpgradeWithResolvableSpecial)) NoCost (runAbility u 1)]
