module Marvel.SideScheme.SideSchemes.Nc48033 where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Boost ()
import Marvel.Phase ()
import Marvel.Ally.Types ()
import Marvel.Minion.Types ()
import Marvel.Nightcrawler.Helpers ()
import Marvel.Minion.Cards ()
import Marvel.SideScheme.Cards ()
import Marvel.Hand ()
import Marvel.Id ()
import Marvel.Keyword ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Query ()
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Stats ()
import Marvel.Status ()
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.SideScheme.Types
import Marvel.SideScheme.Cards qualified as Cards

nc48033 :: SideSchemeCard Nc48033
nc48033 = sideScheme Nc48033 Cards.nc48033 (Static 2)

newtype Nc48033 = Nc48033 (Attrs SideScheme)
  deriving anyclass (HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsSideScheme Nc48033 where
  sideSchemeAbilities a = [limitedWindowAbility a 1
    (W.MinionHasSchemed W.After $ NotMinion $ MinionWithTrait Trait.Elite)
    ForcedResponse NoCriteria NoCost (runAbility a 1)]

instance RunMessage Nc48033 where
  runMessage msg a@(Nc48033 attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ [mid | W.SchemedByMinion mid <- windows] $ \mid -> do
        you <- fromMaybe <$> getActivePlayerId <*> getMinionEngagedPlayer mid
        cardSpecific GameRef "ncDealMinionAndPass" (mid, you)
      pure a
    _ -> Nc48033 <$> runMessage msg attrs
