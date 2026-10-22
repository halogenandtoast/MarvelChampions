module Marvel.Hero.Heroes.Hulk where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Hero.Cards qualified as Cards
import Marvel.Hero.Runner
import Marvel.Identity.Types
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Projection
import Marvel.Question
import Marvel.Queue
import Marvel.Ref
import Marvel.Stats
import Marvel.Window qualified as W

hulk :: HeroCard Hulk
hulk =
  hero
    Hulk
    Cards.hulk
    (HP $ Static 18)
    (HandSize 4)
    (Thw 0)
    (Atk 3)
    (Def 3)

instance HasAbilities Hulk where
  getAbilities (Hulk a) =
    [ label "Enraged" $
        limitedWindowAbility
          a
          1
          (W.TurnEnds W.When $ IdentityWithId $ heroIdentityId a)
          ForcedInterrupt
          IsSelf
          NoCost
          (runAbility a 1)
    ]

newtype Hulk = Hulk (Attrs Hero)
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance RunMessage Hulk where
  runMessage msg h@(Hulk attrs) = case msg of
    -- "discard your hand"
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> do
      hand <- project PlayerIdentityHand ident
      pushAll $ map (DiscardedCard . PlayerCard) (unHand hand)
      pure h
    _ -> Hulk <$> runMessage msg attrs
