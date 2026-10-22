module Marvel.Hero.Heroes.Thor where

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
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Stats
import Marvel.Window qualified as W

thor :: HeroCard Thor
thor =
  hero
    Thor
    Cards.thor
    (HP $ Static 14)
    (HandSize 4)
    (Thw 1)
    (Atk 2)
    (Def 2)

instance HasAbilities Thor where
  getAbilities (Thor a) =
    [ label "\"Have at thee!\"" $
        limited (PerPhase 1) $
          limitedWindowAbility
            a
            1
            (W.EngagesMinion W.After (IdentityWithId ident) AnyMinion)
            Response
            IsSelf
            NoCost
            (Run [IdentityMessage ident $ DrawCards FromDeck 2])
    ]
   where
    ident = heroIdentityId a

newtype Thor = Thor (Attrs Hero)
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance RunMessage Thor where
  runMessage msg (Thor attrs) = Thor <$> runMessage msg attrs
