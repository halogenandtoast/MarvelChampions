module Marvel.Upgrade.Upgrades.MartialArtsTraining where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Cost
import Marvel.Criteria
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Query ()
import Marvel.Question
import Marvel.Payment ()
import Marvel.Resource ()
import Marvel.Trait qualified as Trait ()
import Marvel.Window qualified as W
import Marvel.Keyword qualified as Keyword ()
import Marvel.Card
import Marvel.Entity
import Marvel.GameValue ()
import Marvel.Hand ()
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Stats ()
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

martialArtsTraining :: UpgradeCard MartialArtsTraining
martialArtsTraining = upgrade MartialArtsTraining Cards.martialArtsTraining

newtype MartialArtsTraining = MartialArtsTraining (Attrs Upgrade)
  deriving anyclass (IsUpgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor MartialArtsTraining where
  getModifiersFor _ (IdentityRef you) (MartialArtsTraining a) | you == upgradeController a = do
    psylocke <- identityMatches (HeroIdentity <> IdentityWithTitle "Psylocke") you
    pure [DefenseModifier 1 | psylocke]
  getModifiersFor _ _ _ = pure []

instance HasAbilities MartialArtsTraining where
  getAbilities (MartialArtsTraining a) = [limitedWindowAbility a 1 (W.HeroDefended W.After You AnyEnemy) HeroResponse (OwnsThis <> SelfMatches (IdentityWithTitle "Psylocke")) (DiscardCost $ toTarget a) (runAbility a 1)]

instance RunMessage MartialArtsTraining where
  runMessage msg e@(MartialArtsTraining a) = runQueueT $ case msg of
    RanAbility you (isTarget a -> True) 1 _ _ -> do
      ready you
      pure e
    _ -> MartialArtsTraining <$> runMessage msg a
