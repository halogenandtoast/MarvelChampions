module Marvel.Hero.Heroes.CaptainAmerica where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Hero.Cards qualified as Cards
import Marvel.Hero.Runner
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Stats

captainAmerica :: HeroCard CaptainAmerica
captainAmerica =
  hero
    CaptainAmerica
    Cards.captainAmerica
    (HP $ Static 11)
    (HandSize 5)
    (Thw 2)
    (Atk 2)
    (Def 2)

newtype CaptainAmerica = CaptainAmerica (Attrs Hero)
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance HasAbilities CaptainAmerica where
  getAbilities (CaptainAmerica a) =
    [ label "\"I Can Do This All Day!\"" $
        limitedAbility a 1 (PerRound 1) Action (IsSelf <> Exhausted) (DiscardHandCardsCost 1 (Just 1)) ReadyIdentity
    ]

instance RunMessage CaptainAmerica where
  runMessage msg (CaptainAmerica attrs) = CaptainAmerica <$> runMessage msg attrs
