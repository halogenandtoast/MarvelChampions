module Marvel.Hero.Heroes.AntManGiant where

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
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Stats
import Marvel.Window qualified as W

antManGiant :: HeroCard AntManGiant
antManGiant =
  hero
    AntManGiant
    Cards.antManGiant
    (HP $ Static 12)
    (HandSize 4)
    (Thw 1)
    (Atk 3)
    (Def 3)

-- | Giant Nuisance — Response: After you change to this form, deal 1 damage to an enemy.
instance HasAbilities AntManGiant where
  getAbilities (AntManGiant a) =
    [ label "Giant Nuisance" $
        limitedWindowAbility a 1 (W.IdentityChangedToForm W.After $ IdentityWithId $ heroIdentityId a) Response IsSelf NoCost $
          ChooseDamage (toSource a) (toDamage 1 FromAbility) AnyEnemy
    ]

newtype AntManGiant = AntManGiant (Attrs Hero)
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance RunMessage AntManGiant where
  runMessage msg (AntManGiant attrs) = AntManGiant <$> runMessage msg attrs
