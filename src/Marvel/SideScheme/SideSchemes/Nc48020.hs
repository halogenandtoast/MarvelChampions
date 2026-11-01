module Marvel.SideScheme.SideSchemes.Nc48020 where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source ()
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
import Marvel.Query
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Stats ()
import Marvel.Status ()
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.SideScheme.Types
import Marvel.SideScheme.Cards qualified as Cards

nc48020 :: SideSchemeCard Nc48020
nc48020 = sideScheme Nc48020 Cards.nc48020 (Static 5)

newtype Nc48020 = Nc48020 (Attrs SideScheme)
  deriving anyclass (HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsSideScheme Nc48020 where
  sideSchemeAbilities a =
    [limitedWindowAbility a 1 (W.HeroDefendedWithoutDamage W.After $ IdentityWithTrait Trait.XMen)
      Response NoCriteria NoCost (runAbility a 1)
    ,limitedWindowAbility a 2 (W.AllyDefendedWithoutDamage W.After $ AllyWithTrait Trait.XMen)
      Response NoCriteria NoCost (runAbility a 2)]

instance RunMessage Nc48020 where
  runMessage msg a@(Nc48020 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) idx _ _ | idx `elem` [1,2] -> do
      thwartSchemeAs you (sideSchemeId attrs) attrs 1
      pure a
    SideSchemeMessage sid DefeatSideScheme | sid == sideSchemeId attrs -> do
      enemies <- selectList AnyEnemy
      for_ enemies $ \enemy -> giveStatus enemy StunnedStatus >> giveStatus enemy ConfusedStatus
      Nc48020 <$> runMessage msg attrs
    _ -> Nc48020 <$> runMessage msg attrs
