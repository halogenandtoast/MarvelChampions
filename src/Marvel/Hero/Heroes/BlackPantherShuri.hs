module Marvel.Hero.Heroes.BlackPantherShuri where
import Marvel.Prelude
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Question (runAbility)
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
import Marvel.Hero.Runner
import Marvel.GameValue
import Marvel.Hand
import Marvel.Stats

blackPantherShuri :: HeroCard BlackPantherShuri
blackPantherShuri = hero BlackPantherShuri Cards.blackPantherShuri (HP $ Static 11) (HandSize 5) (Thw 2) (Atk 1) (Def 2)
newtype BlackPantherShuri = BlackPantherShuri (Attrs Hero)
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance RunMessage BlackPantherShuri where
  runMessage msg u@(BlackPantherShuri attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> u <$ choosePantherSpecial you
    _ -> BlackPantherShuri <$> runMessage msg attrs

instance HasAbilities BlackPantherShuri where
  getAbilities u@(BlackPantherShuri _attrs) = [limitedWindowAbility u 1 (W.WindowOneOf [W.MakesBasicAttack W.After You, W.MakesBasicThwart W.After You, W.MakesBasicDefense W.After You AnyEnemy]) Response (UpgradeExists (UpgradeWithTrait T.BlackPanther <> UpgradeControlledBy You <> UpgradeWithResolvableSpecial)) NoCost (runAbility u 1)]
