module Marvel.Minion.Minions.Warbringer where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Game.Source ()
import Marvel.Hand
import Marvel.Identity.Types (Field (PlayerIdentityHand))
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner
import Marvel.Projection
import Marvel.Resource
import Marvel.Window qualified as W

warbringer :: MinionCard Warbringer
warbringer = minion Warbringer Cards.warbringer (Sch 1) (Atk 3) (HP 5)

newtype Warbringer = Warbringer (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Warbringer where
  getAbilities (Warbringer a) =
    [ windowAbility a 1 (W.EnemyAttacked W.When (EnemyWithId $ toEnemyId a) AnyIdentity)
        ForcedInterrupt NoCost (RunAbility (toTarget a) 1)
    ]

instance RunMessage Warbringer where
  runMessage msg m@(Warbringer attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ [you | W.EnemyAttack _ you <- windows] \you -> do
        hand <- projectMap PlayerIdentityHand unHand you
        changeAttack (minionId attrs) (length $ filter (elem Wild . printedResources . getCardDef) hand)
        attackGains (minionId attrs) Overkill
      pure m
    _ -> Warbringer <$> runMessage msg attrs
