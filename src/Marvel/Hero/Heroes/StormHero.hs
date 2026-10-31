module Marvel.Hero.Heroes.StormHero where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Question
import Marvel.Cost
import Marvel.Criteria
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Hero.Cards qualified as Cards
import Marvel.Hero.Runner
import Marvel.GameValue
import Marvel.Hand
import Marvel.Stats

stormHero :: HeroCard StormHero
stormHero = hero StormHero Cards.stormHero (HP $ Static 10) (HandSize 5) (Thw 1) (Atk 2) (Def 1)

newtype StormHero = StormHero (Attrs Hero)
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities StormHero where
  getAbilities a = [label "$label.weatherControl" $ limitedAbility a 1 (PerRound 1) Action IsSelf NoCost (runAbility a 1)]

instance RunMessage StormHero where
  runMessage msg h@(StormHero attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> h <$ swapWeather you
    _ -> StormHero <$> runMessage msg attrs
