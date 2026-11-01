module Marvel.Upgrade.Upgrades.MentalParalysis where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Entity
import Marvel.GameValue ()
import Marvel.Hand ()
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Stats ()
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

mentalParalysis :: UpgradeCard MentalParalysis
mentalParalysis = upgrade MentalParalysis Cards.mentalParalysis

newtype MentalParalysis = MentalParalysis (Attrs Upgrade)

  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsUpgrade MentalParalysis where
  upgradeTargets _ = Just . map toTarget <$> selectList (NotMinion $ MinionWithTrait Trait.Elite)

instance HasAbilities MentalParalysis where
  getAbilities (MentalParalysis a) =
    [limitedWindowAbility a 1 (W.IdentityChangedToForm W.After $ IdentityWithId $ upgradeController a) ForcedResponse (OwnsThis <> InAlterEgoForm) NoCost (runAbility a 1)]

instance RunMessage MentalParalysis where
  runMessage msg e@(MentalParalysis a) = runQueueT $ case msg of
    RanAbility _ (isTarget a -> True) 1 _ _ -> e <$ removeFromPlay a
    _ -> MentalParalysis <$> runMessage msg a

instance HasModifiersFor MentalParalysis where
  getModifiersFor _ target (MentalParalysis a) = pure [CannotActivate | Just target == (enemyRef <$> upgradeAttachedEnemy a)]
