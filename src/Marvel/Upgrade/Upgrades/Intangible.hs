module Marvel.Upgrade.Upgrades.Intangible where

import Marvel.Prelude
import Marvel.Question
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Query ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Window qualified as W
intangible :: UpgradeCard Intangible
intangible = upgrade Intangible Cards.intangible

newtype Intangible = Intangible (Attrs Upgrade)
  deriving anyclass (IsUpgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

dense :: UpgradeCard Intangible
dense = upgrade Intangible Cards.dense

instance HasModifiersFor Intangible where
  getModifiersFor _ (IdentityRef you) (Intangible a) | you == upgradeController a = do
    hero <- identityMatches HeroIdentity you
    pure $ if toCardCode a == toCardCode Cards.dense
      then [AttackModifier 2 | hero] <> [DefenseModifier 2 | hero]
      else [CannotAttack, CannotDefend, AttackDamageReduction 2]
  getModifiersFor _ _ _ = pure []

instance HasAbilities Intangible where
  getAbilities (Intangible a) =
    [ limitedWindowAbility a 1 (W.MassFormChangedTo W.After $ UpgradeWithId $ upgradeId a)
        Response OwnsThis NoCost (runAbility a 1)
    | toCardCode a == toCardCode Cards.dense
    ]

instance RunMessage Intangible where
  runMessage msg u@(Intangible attrs) = runQueueT $ case msg of
    CardSpecific (isTarget attrs -> True) "flipMassForm" _ -> do
      massFormChanged (upgradeController attrs) (upgradeId attrs)
      pure $ Intangible attrs {upgradeCardDef = if toCardCode attrs == toCardCode Cards.dense then Cards.intangible else Cards.dense}
    RanAbility you (isTarget attrs -> True) 1 _ _ -> u <$ drawCards you 1
    _ -> Intangible <$> runMessage msg attrs
