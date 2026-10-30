module Marvel.Hero.Heroes.GhostSpider where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card (CardMatcher (AnyCard))
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Hero.Cards qualified as Cards
import Marvel.Hero.Runner
import Marvel.GameValue
import Marvel.Hand
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.Stats
import Marvel.Window qualified as W

ghostSpider :: HeroCard GhostSpider
ghostSpider = hero GhostSpider Cards.ghostSpider (HP $ Static 10) (HandSize 5) (Thw 1) (Atk 2) (Def 3)

newtype GhostSpider = GhostSpider (Attrs Hero)
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance HasAbilities GhostSpider where
  getAbilities a =
    [ label "Dizzying Reflexes" $ limited (PerPhase 1) $
        limitedWindowAbility a 1 (W.ResolvesEventAbility W.After You AnyCard)
          Response (IsSelf <> Exhausted) NoCost (runAbility a 1)
    ]

instance RunMessage GhostSpider where
  runMessage msg h@(GhostSpider attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> h <$ ready you
    _ -> GhostSpider <$> runMessage msg attrs
