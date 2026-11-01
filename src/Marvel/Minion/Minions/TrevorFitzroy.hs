module Marvel.Minion.Minions.TrevorFitzroy where

import Marvel.Prelude
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Question
import Marvel.Query
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Window qualified as W
import Marvel.Ability
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

trevorFitzroy :: MinionCard TrevorFitzroy
trevorFitzroy = minion TrevorFitzroy Cards.trevorFitzroy (Sch 2) (Atk 3) (HP 5)

newtype TrevorFitzroy = TrevorFitzroy (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities TrevorFitzroy where
  getAbilities a@(TrevorFitzroy attrs) =
    [limitedWindowAbility a 1 (W.AllyDefeatedByAttackOf W.After $ EnemyWithId $ EnemyMinionId $ minionId attrs)
      ForcedResponse NoCriteria NoCost (runAbility a 1)]

instance RunMessage TrevorFitzroy where
  runMessage msg a@(TrevorFitzroy attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      let you = minionEngagedIdentity attrs
      portal <- selectOne $ SideSchemeIs SideSchemes.portalThroughTime
      case portal of
        Just sid -> placeThreat sid 2
        Nothing -> searchEverywhereAndRevealEncounterCard you $ CardIs SideSchemes.portalThroughTime
      pure a
    _ -> TrevorFitzroy <$> runMessage msg attrs
