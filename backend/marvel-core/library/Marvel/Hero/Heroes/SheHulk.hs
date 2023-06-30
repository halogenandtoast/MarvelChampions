module Marvel.Hero.Heroes.SheHulk where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Hero.Cards qualified as Cards
import Marvel.Hero.Runner
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Stats
import Marvel.Window qualified as W

sheHulk :: HeroCard SheHulk
sheHulk =
  hero
    SheHulk
    Cards.sheHulk
    (HP $ Static 15)
    (HandSize 4)
    (Thw 1)
    (Atk 3)
    (Def 2)

instance HasAbilities SheHulk where
  getAbilities (SheHulk a) =
    [ label "Do You Even Lift?" $
        limitedWindowAbility
          a
          1
          (W.IdentityChangedToForm W.After $ IdentityWithId $ heroIdentityId a)
          Response
          IsSelf
          NoCost
          (ChooseDamage (toRef a) (toDamage 2 FromAbility) AnyEnemy)
    ]

newtype SheHulk = SheHulk (Attrs Hero)
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance RunMessage SheHulk where
  runMessage msg (SheHulk attrs) = SheHulk <$> runMessage msg attrs
