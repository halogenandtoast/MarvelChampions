module Marvel.Upgrade.Upgrades.War where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

war :: UpgradeCard War
war = upgrade War Cards.war

newtype War = War (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities War where
  getAbilities a = [ability a 1 HeroAction OwnsThis ExhaustCost (runAbility a 1)]

instance RunMessage War where
  runMessage msg e@(War attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      discardTopOfEncounterDeckFor 1 attrs
      pure e
    WithDiscarded (isTarget attrs -> True) FromEncounterDeck cards -> do
      let amount = fromIntegral $ sum [length $ cdBoostIcons $ getCardDef card | card <- cards]
      damageIdentity (upgradeController attrs) attrs (toDamage amount FromAbility)
      cardSpecific attrs "discardPlayer" ()
      pure e
    CardSpecific (isTarget attrs -> True) "discardPlayer" _ -> do
      discardFromDeck (upgradeController attrs) 1 attrs
      pure e
    WithDiscarded (isTarget attrs -> True) FromDeck cards -> do
      for_ cards $ \card -> do
        let amount = fromIntegral $ max 0 $ fromMaybe 0 $ cdCost $ getCardDef card
        chooseDamage (upgradeController attrs) attrs (toDamage amount FromAbility) AnyEnemy
      pure e
    _ -> War <$> runMessage msg attrs
