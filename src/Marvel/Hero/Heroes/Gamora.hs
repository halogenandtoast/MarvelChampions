module Marvel.Hero.Heroes.Gamora (
  gamora,
  Gamora (..),
) where

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
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W

gamora :: HeroCard Gamora
gamora =
  hero
    Gamora
    Cards.gamora
    (HP $ Static 10)
    (HandSize 5)
    (Thw 2)
    (Atk 2)
    (Def 2)

-- | Finesse: after you play an Attack event, remove 1 threat from a scheme. Precision:
-- after you play a Thwart event, deal 1 damage to an enemy. Each once per phase.
instance HasAbilities Gamora where
  getAbilities a@(Gamora attrs) =
    [ label "Finesse" $
        limited (PerPhase 1) $
          limitedWindowAbility
            a
            1
            (afterYouPlay Trait.Attack)
            Response
            (IsSelf <> SchemeExists ThwartableScheme)
            NoCost
            (RemoveThreat (toSource a) 1 ThwartableScheme)
    , label "Precision" $
        limited (PerPhase 1) $
          limitedWindowAbility
            a
            2
            (afterYouPlay Trait.Thwart)
            Response
            (IsSelf <> EnemyExists DamageableEnemy)
            NoCost
            (ChooseDamage (toSource a) (toDamage 1 FromAbility) AnyEnemy)
    ]
   where
    afterYouPlay t =
      W.PlaysEvent W.After (IdentityWithId $ heroIdentityId attrs) (CardWithType EventType <> CardWithTrait t)

newtype Gamora = Gamora (Attrs Hero)
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance RunMessage Gamora where
  runMessage msg (Gamora attrs) = Gamora <$> runMessage msg attrs
