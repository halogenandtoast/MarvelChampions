module Marvel.Hero.Heroes.AntManTiny where

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
import Marvel.Question
import Marvel.Ref
import Marvel.Stats
import Marvel.Window qualified as W

antManTiny :: HeroCard AntManTiny
antManTiny =
  hero
    AntManTiny
    Cards.antManTiny
    (HP $ Static 12)
    (HandSize 5)
    (Thw 2)
    (Atk 1)
    (Def 2)

-- | Puny Pest — Response: After you change to this form, remove 1 threat from a scheme.
instance HasAbilities AntManTiny where
  getAbilities (AntManTiny a) =
    [ label "Puny Pest" $
        limitedWindowAbility a 1 (W.IdentityChangedToForm W.After $ IdentityWithId $ heroIdentityId a) Response IsSelf NoCost $
          RemoveThreat (toSource a) 1 ThwartableScheme
    ]

newtype AntManTiny = AntManTiny (Attrs Hero)
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance RunMessage AntManTiny where
  runMessage msg (AntManTiny attrs) = AntManTiny <$> runMessage msg attrs
