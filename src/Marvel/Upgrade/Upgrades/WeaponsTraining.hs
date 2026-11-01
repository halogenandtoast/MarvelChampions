module Marvel.Upgrade.Upgrades.WeaponsTraining where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Cost
import Marvel.Criteria
import Marvel.Id
import Marvel.Matchers
import Marvel.Query
import Marvel.Question
import Marvel.Payment
import Marvel.Resource
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Keyword qualified as Keyword
import Marvel.Card
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Stats
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

weaponsTraining :: UpgradeCard WeaponsTraining
weaponsTraining = upgrade WeaponsTraining Cards.weaponsTraining

newtype WeaponsTraining = WeaponsTraining (Attrs Upgrade)
  deriving anyclass (IsUpgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor WeaponsTraining where
  getModifiersFor _ (IdentityRef you) (WeaponsTraining a) | you == upgradeController a = do
    psylocke <- identityMatches (HeroIdentity <> IdentityWithTitle "Psylocke") you
    pure [KeywordModifier (Keyword.Retaliate 1) | psylocke]
  getModifiersFor _ _ _ = pure []

instance HasAbilities WeaponsTraining where
  getAbilities (WeaponsTraining a) = [limitedWindowAbility a 1 (W.IdentityAttackResolved You) HeroResponse (OwnsThis <> SelfMatches (IdentityWithTitle "Psylocke")) (DiscardCost $ toTarget a) (runAbility a 1)]

instance RunMessage WeaponsTraining where
  runMessage msg e@(WeaponsTraining a) = runQueueT $ case msg of
    RanAbility you (isTarget a -> True) 1 _ _ -> do
      weapons <- selectList (UpgradeControlledBy (IdentityWithId you) <> UpgradeWithTrait Trait.Weapon)
      traverse_ ready weapons
      pure e
    _ -> WeaponsTraining <$> runMessage msg a
