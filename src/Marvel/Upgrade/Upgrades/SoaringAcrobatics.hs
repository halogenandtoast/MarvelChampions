module Marvel.Upgrade.Upgrades.SoaringAcrobatics where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query ()
import Marvel.Question
import Marvel.Ref
import Marvel.Window qualified as W
import Marvel.Trait qualified as Trait
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

soaringAcrobatics :: UpgradeCard SoaringAcrobatics
soaringAcrobatics = upgrade SoaringAcrobatics Cards.soaringAcrobatics
newtype SoaringAcrobatics = SoaringAcrobatics (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities SoaringAcrobatics where
  getAbilities a@(SoaringAcrobatics attrs) = [limitedWindowAbility a 1
    (W.WindowOneOf [W.MakesBasicAttack W.When you, W.MakesBasicThwart W.When you,
      W.MakesBasicDefense W.When you AnyEnemy, W.UsesRecoveryAt W.When you,
      W.AllyAttacked W.When allies AnyEnemy, W.AllyThwarted W.When allies AnyScheme])
    HeroInterrupt OwnsThis ExhaustCost (runAbility a 1)]
    where
      you = IdentityWithId (upgradeController attrs) <> IdentityWithTrait Trait.Aerial
      allies = AllyControlledBy (IdentityWithId $ upgradeController attrs) <> AllyWithTrait Trait.Aerial

instance RunMessage SoaringAcrobatics where
  runMessage msg a@(SoaringAcrobatics attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ (headMay [you | window <- windows, you <- case window of
        W.MadeBasicAttack you -> [you]
        W.MadeBasicThwart you -> [you]
        W.MadeBasicDefense you _ -> [you]
        W.UsedRecovery you -> [you]
        _ -> []]) $ \you -> addToBasicPower you 1
      for_ (headMay [aid | window <- windows, aid <- case window of
        W.AllyAttack aid _ -> [aid]
        W.AllyThwart aid _ -> [aid]
        _ -> []]) $ \aid -> addToAllyBasicPower aid 1
      pure a
    _ -> SoaringAcrobatics <$> runMessage msg attrs
