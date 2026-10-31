module Marvel.Hero.Heroes.GambitHero where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Cost
import Marvel.Criteria
import Marvel.Effect.Types
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Hero.Cards qualified as Cards
import Marvel.Hero.Runner
import Marvel.GameValue
import Marvel.Hand
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Stats
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W

gambitHero :: HeroCard GambitHero
gambitHero = hero GambitHero Cards.gambitHero (HP $ Static 9) (HandSize 5) (Thw 1) (Atk 2) (Def 3)

newtype GambitHero = GambitHero (Attrs Hero)
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities GambitHero where
  getAbilities a@(GambitHero attrs) =
    [ label "$label.chargeDeCard" $ limitedAbility a 1 (PerRound 1) Action IsSelf NoCost (runAbility a 1)
    , label "$label.throwDeCard" $ limitedWindowAbility a 2
        (W.PlaysEvent W.When (IdentityWithId $ heroIdentityId attrs) (CardWithTrait Trait.Attack))
        Interrupt (IsSelf <> SelfMatches (IdentityWithCounters $ AtLeast $ Static 1)) NoCost (runAbility a 2)
    ]

instance RunMessage GambitHero where
  runMessage msg h@(GambitHero attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> h <$ placeCounters you 1
    RanAbility you (isTarget attrs -> True) 2 windows _ -> do
      n <- getCountersOn you
      for_ [eid | W.EventPlayed _ eid _ <- windows] \eid ->
        chooseOne you [labeled (countLabel "discardCounters" k) do
          removeCounters you k
          createEffect Cards.gambitHero attrs (EventEntity eid)
          cardSpecific attrs "chargedEvent" (eid, k)
          | k <- [1 .. min 3 n]]
      pure h
    _ -> GambitHero <$> runMessage msg attrs

newtype GambitHeroEffect = GambitHeroEffect (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

gambitHeroEffect :: CardEffect GambitHeroEffect
gambitHeroEffect = effect GambitHeroEffect Cards.gambitHero

instance RunMessage GambitHeroEffect where
  runMessage msg e@(GambitHeroEffect attrs) = runQueueT $ case msg of
    CardSpecific target "chargedEvent" (fromJSON -> Success (eid, n))
      | target == refocus (effectSource attrs), effectMatcher attrs == EventEntity eid ->
          pure $ GambitHeroEffect attrs {effectModifiers = [DamageDealtModifier n, ChargedWithCounters n]}
    EventMessage eid ResolvedEvent | effectMatcher attrs == EventEntity eid -> e <$ disableEffect (effectId attrs)
    _ -> GambitHeroEffect <$> runMessage msg attrs
