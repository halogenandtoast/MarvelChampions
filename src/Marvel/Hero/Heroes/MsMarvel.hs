module Marvel.Hero.Heroes.MsMarvel where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Def
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
import Marvel.Queue
import Marvel.Ref
import Marvel.Stats
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W

msMarvel :: HeroCard MsMarvel
msMarvel =
  hero
    MsMarvel
    Cards.msMarvel
    (HP $ Static 10)
    (HandSize 5)
    (Thw 1)
    (Atk 1)
    (Def 1)

instance HasAbilities MsMarvel where
  getAbilities a@(MsMarvel attrs) =
    [ label "Morphogenetics" $
        limitedWindowAbility
          a
          1
          ( W.PlaysEvent W.After (IdentityWithId $ heroIdentityId attrs) $
              CardMatchesAny $ map CardWithTrait [Trait.Attack, Trait.Thwart, Trait.Defense]
          )
          Response
          IsSelf
          ExhaustCost
          (runAbility a 1)
    ]

newtype MsMarvel = MsMarvel (Attrs Hero)
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance RunMessage MsMarvel where
  runMessage msg a@(MsMarvel attrs) = case msg of
    RanAbility _ (isTarget a -> True) 1 windows _ -> do
      pushAll [ReturnToHand (EventRef eid) | W.EventPlayed _ eid _ <- windows]
      pure a
    _ -> MsMarvel <$> runMessage msg attrs
