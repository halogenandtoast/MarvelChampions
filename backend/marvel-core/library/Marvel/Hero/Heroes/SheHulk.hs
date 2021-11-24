module Marvel.Hero.Heroes.SheHulk where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Hero.Attrs
import Marvel.Hero.Cards qualified as Cards
import Marvel.Matchers
import Marvel.Message
import Marvel.Question
import Marvel.Source
import Marvel.Stats
import Marvel.Target
import Marvel.Window qualified as W

sheHulk :: HeroCard SheHulk
sheHulk = hero
  SheHulk
  Cards.sheHulk
  (HP $ Static 15)
  (HandSize 4)
  (Thw 1)
  (Atk 3)
  (Def 2)

instance HasAbilities SheHulk where
  getAbilities a =
    [ label "Do You Even Lift?" $ limitedWindowAbility
        a
        1
        (W.IdentityChangedToForm W.After $ IdentityWithId $ toId a)
        Response
        IsSelf
        NoCost
        (ChooseDamage (toSource a) 2 AnyEnemy)
    ]

newtype SheHulk = SheHulk HeroAttrs
  deriving anyclass IsHero
  deriving newtype (Show, Eq, HasStartingHP, HasHandSize, ToJSON, FromJSON, IsSource, IsTarget, HasCardCode, Entity)

instance RunMessage SheHulk where
  runMessage msg (SheHulk attrs) = SheHulk <$> runMessage msg attrs
