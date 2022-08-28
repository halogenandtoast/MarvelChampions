module Marvel.Minion.Minions.YonRogg
  ( yonRogg
  , YonRogg(..)
  ) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Entity
import Marvel.Hp
import Marvel.Matchers
import Marvel.Message
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Window.Types

yonRogg :: MinionCard YonRogg
yonRogg = minion YonRogg Cards.yonRogg (Sch 2) (Atk 3) (HP 5)

newtype YonRogg = YonRogg (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsSource, IsTarget)

instance HasAbilities YonRogg where
  getAbilities (YonRogg a) =
    [ windowAbility
          a
          1
          (EnemyAttacked After (EnemyWithId $ EnemyMinionId $ minionId a) You)
          ForcedResponse
          NoCost
        $ RunAbility (toTarget a) 1
    ]

instance RunMessage YonRogg where
  runMessage msg e@(YonRogg attrs) = case msg of
    RanAbility target 1 _ _ | isTarget attrs target -> do
      mThePsycheMagnitron <- selectOne
        $ SideSchemeIs SideSchemes.thePsycheMagnitron
      for_ (traceShowId mThePsycheMagnitron) $ \thePsycheMagnitron ->
        push $ SideSchemeMessage thePsycheMagnitron $ SideSchemePlaceThreat 1
      pure e
    _ -> YonRogg <$> runMessage msg attrs
