module Marvel.Hero.Heroes.SpiderWoman (
  spiderWoman,
  spiderWomanEffect,
  SpiderWoman (..),
  SpiderWomanEffect (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Aspect
import Marvel.Card.Def
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Effect.Types
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Hero.Cards qualified as Cards
import Marvel.Hero.Runner
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Stats
import Marvel.Window qualified as W

spiderWoman :: HeroCard SpiderWoman
spiderWoman =
  hero
    SpiderWoman
    Cards.spiderWoman
    (HP $ Static 11)
    (HandSize 5)
    (Thw 1)
    (Atk 1)
    (Def 1)

-- | "Superhuman Agility" — Interrupt: When you play an aspect card, Spider-Woman gets +1
-- THW, +1 ATK, and +1 DEF until the end of the round. (Limit once per round for each
-- aspect.) One ability for each aspect, each once a round.
instance HasAbilities SpiderWoman where
  getAbilities (SpiderWoman a) =
    [ label "\"Superhuman Agility\"" $
        limited (PerRound 1) $
          limitedWindowAbility
            a
            n
            (W.PlaysCard W.When (IdentityWithId $ heroIdentityId a) (CardWithAspect aspect))
            Interrupt
            IsSelf
            NoCost
            (runAbility a n)
    | (n, aspect) <- zip [1 ..] [Aggression, Justice, Leadership, Protection, Pool]
    ]

newtype SpiderWoman = SpiderWoman (Attrs Hero)
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance RunMessage SpiderWoman where
  runMessage msg h@(SpiderWoman attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget attrs -> True) n _ _ | n <= 5 -> do
      createEffect Cards.spiderWoman attrs (IdentityEntity $ IdentityWithId ident)
      pure h
    _ -> SpiderWoman <$> runMessage msg attrs

-- | +1 THW, +1 ATK and +1 DEF until the end of the round.
newtype SpiderWomanEffect = SpiderWomanEffect (Attrs Effect)
  deriving anyclass (IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

spiderWomanEffect :: CardEffect SpiderWomanEffect
spiderWomanEffect =
  effectWith
    SpiderWomanEffect
    Cards.spiderWoman
    ((modifiersL .~ [ThwartModifier 1, AttackModifier 1, DefenseModifier 1]) . (endsL ?~ DisableAtEndOfRound))

instance RunMessage SpiderWomanEffect where
  runMessage msg (SpiderWomanEffect attrs) = SpiderWomanEffect <$> runMessage msg attrs
