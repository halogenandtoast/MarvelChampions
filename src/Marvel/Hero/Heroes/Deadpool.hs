module Marvel.Hero.Heroes.Deadpool where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Hero.Cards qualified as Cards
import Marvel.Hero.Runner
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.Stats
import Marvel.Window qualified as W

deadpool :: HeroCard Deadpool
deadpool = hero Deadpool Cards.deadpool (HP $ Static 9) (HandSize 5) (Thw 2) (Atk 2) (Def 1)

newtype Deadpool = Deadpool (Attrs Hero)
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Deadpool where
  getAbilities a@(Deadpool attrs) =
    [ label "$label.theRegeneratinDegenerate" $
        limitedWindowAbility a 1 (W.IdentityWouldBeDefeated $ IdentityWithId $ heroIdentityId attrs)
          ForcedInterrupt IsSelf NoCost (runAbility a 1)
    ]

instance RunMessage Deadpool where
  runMessage msg h@(Deadpool attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      cancelMatchingMessage (== IdentityMessage you IdentityDefeated)
      setHitPoints you 1
      changeToAlterEgo you
      addAccelerationToken
      pure h
    _ -> Deadpool <$> runMessage msg attrs
