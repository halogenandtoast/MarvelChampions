module Marvel.Hero.Heroes.AngelHero where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Def
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Matchers
import Marvel.Question
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Hero.Cards qualified as Cards
import Marvel.Hero.Runner
import Marvel.GameValue
import Marvel.Hand
import Marvel.Stats

angelHero :: HeroCard AngelHero
angelHero = hero AngelHero Cards.angelHero (HP $ Static 12) (HandSize 5) (Thw 2) (Atk 1) (Def 2)

newtype AngelHero = AngelHero (Attrs Hero)
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities AngelHero where
  getAbilities a@(AngelHero attrs) =
    [ label "Angel of Life" $ limited (PerPhase 1) $
        limitedWindowAbility a 1 (W.PlaysEvent W.After (IdentityWithId $ heroIdentityId attrs) (CardWithTrait Trait.Aerial))
          Response IsSelf NoCost (runAbility a 1)
    ]

instance RunMessage AngelHero where
  runMessage msg h@(AngelHero attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> h <$ drawCards you 1
    _ -> AngelHero <$> runMessage msg attrs
