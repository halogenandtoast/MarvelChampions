module Marvel.Hero.Heroes.SpiderHam where

import Marvel.Prelude
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Payment ()
import Marvel.Question
import Marvel.Resource ()
import Marvel.Window qualified as W
import Marvel.Ability
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

spiderHam :: HeroCard SpiderHam
spiderHam = hero SpiderHam Cards.spiderHam (HP $ Static 12) (HandSize 5) (Thw 2) (Atk 1) (Def 1)

newtype SpiderHam = SpiderHam (Attrs Hero)
  deriving anyclass (IsHero)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor SpiderHam where
  getModifiersFor _ target (SpiderHam attrs) = pure [IdentityCountersAsResources | isTarget attrs target]

instance HasAbilities SpiderHam where
  getAbilities a@(SpiderHam attrs) =
    [ label "$label.spiderNonsense" $ limitedWindowAbility a 1
        (W.IdentityDamaged W.After $ IdentityWithId $ heroIdentityId attrs)
        Response IsSelf NoCost (runAbility a 1)
    ]

instance RunMessage SpiderHam where
  runMessage msg h@(SpiderHam attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> h <$ placeCounters you 1
    _ -> SpiderHam <$> runMessage msg attrs
