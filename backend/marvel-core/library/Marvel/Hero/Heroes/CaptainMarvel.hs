module Marvel.Hero.Heroes.CaptainMarvel where

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
import Marvel.Resource
import Marvel.Stats

captainMarvel :: HeroCard CaptainMarvel
captainMarvel =
  hero
    CaptainMarvel
    Cards.captainMarvel
    (HP $ Static 12)
    (HandSize 5)
    (Thw 2)
    (Atk 2)
    (Def 1)

newtype CaptainMarvel = CaptainMarvel (Attrs Hero)
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance HasAbilities CaptainMarvel where
  getAbilities (CaptainMarvel a) =
    [ label "Rechannel" $
        limitedAbility
          a
          1
          (PerRound 1)
          Action
          IsSelf
          (ResourceCost (Just Energy) <> HealCost 1)
          (Run [IdentityMessage (heroIdentityId a) (DrawCards FromDeck 1)])
    ]

instance RunMessage CaptainMarvel where
  runMessage msg (CaptainMarvel attrs) = CaptainMarvel <$> runMessage msg attrs
