module Marvel.Hero.Heroes.Quicksilver where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Hero.Cards qualified as Cards
import Marvel.Hero.Runner
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.Stats
import Marvel.Window qualified as W

quicksilver :: HeroCard Quicksilver
quicksilver =
  hero
    Quicksilver
    Cards.quicksilver
    (HP $ Static 9)
    (HandSize 5)
    (Thw 1)
    (Atk 1)
    (Def 1)

-- | Super Speed — Response: After you use one of Quicksilver's basic powers (THW, ATK,
-- or DEF), ready him. (Limit once per phase.) A stunned attack or confused thwart is no
-- use of the power (FAQ): its windows are not opened.
instance HasAbilities Quicksilver where
  getAbilities (Quicksilver a) =
    [ label "Super Speed" $
        limited (PerPhase 1) $
          limitedWindowAbility
            a
            1
            ( W.WindowOneOf
                [ W.MakesBasicAttack W.After you
                , W.MakesBasicThwart W.After you
                , W.HeroDefended W.After you AnyEnemy
                ]
            )
            Response
            (IsSelf <> Exhausted)
            NoCost
            $ runAbility a 1
    ]
   where
    you = IdentityWithId $ heroIdentityId a

newtype Quicksilver = Quicksilver (Attrs Hero)
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance RunMessage Quicksilver where
  runMessage msg h@(Quicksilver attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      ready (heroIdentityId attrs)
      pure h
    _ -> Quicksilver <$> runMessage msg attrs
