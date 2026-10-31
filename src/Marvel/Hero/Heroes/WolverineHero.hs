module Marvel.Hero.Heroes.WolverineHero where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Hero.Cards qualified as Cards
import Marvel.Hero.Runner
import Marvel.Stats
import Marvel.GameValue
import Marvel.Hand
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Phase
import Marvel.Question
import Marvel.Window qualified as W

wolverineHero :: HeroCard WolverineHero
wolverineHero = hero WolverineHero Cards.wolverineHero (HP $ Static 10) (HandSize 5) (Thw 2) (Atk 2) (Def 2)

newtype WolverineHero = WolverineHero (Attrs Hero)
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities WolverineHero where
  getAbilities a = [label "$label.healingFactor" $ limitedWindowAbility a 1 (W.PhaseBegins W.After PlayerPhase) Response (IsSelf <> SelfMatches (IdentityWithAnyDamage <> IdentityCanBeHealed)) NoCost (runAbility a 1)]

instance RunMessage WolverineHero where
  runMessage msg h@(WolverineHero attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> h <$ heal you 2
    _ -> WolverineHero <$> runMessage msg attrs
