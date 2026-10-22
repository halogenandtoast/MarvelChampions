module Marvel.Hero.Heroes.BlackWidow where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Def
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
import Marvel.Trait
import Marvel.Window qualified as W

blackWidow :: HeroCard BlackWidow
blackWidow =
  hero
    BlackWidow
    Cards.blackWidow
    (HP $ Static 9)
    (HandSize 5)
    (Thw 2)
    (Atk 2)
    (Def 2)

-- | Widowmaker, as errata'd: after you resolve the ability of a Preparation card you
-- control, deal 1 damage to an enemy. It is not an attack.
instance HasAbilities BlackWidow where
  getAbilities a@(BlackWidow attrs) =
    [ label "Widowmaker" $
        limitedWindowAbility
          a
          1
          (W.ResolvesAbility W.After (IdentityWithId $ heroIdentityId attrs) (CardWithTrait Preparation))
          Response
          (EnemyExists DamageableEnemy)
          NoCost
          (ChooseDamage (toSource a) (toDamage 1 FromAbility) AnyEnemy)
    ]

newtype BlackWidow = BlackWidow (Attrs Hero)
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance RunMessage BlackWidow where
  runMessage msg (BlackWidow attrs) = BlackWidow <$> runMessage msg attrs
