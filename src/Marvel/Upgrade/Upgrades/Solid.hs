module Marvel.Upgrade.Upgrades.Solid where

import Marvel.Prelude
import Marvel.Card.Side
import Marvel.Payment
import Marvel.Game.Source
import Marvel.Hand
import Marvel.Stats
import Marvel.Status
import Marvel.Id
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Projection
import Marvel.Identity.Types (Field (PlayerIdentityDefended))

solid :: UpgradeCard Solid
solid = upgrade Solid Cards.solid

newtype Solid = Solid (Attrs Upgrade)
  deriving anyclass (IsUpgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Solid where
  getAbilities (Solid a) =
    [limitedWindowAbility a 1 (W.WindowOneOf [W.IdentityAttackResolved (IdentityWithId $ upgradeController a), W.HeroDefended W.After (IdentityWithId $ upgradeController a) AnyEnemy])
      (if toCardCode a == toCardCode Cards.phased then ForcedResponse else Response)
      OwnsThis NoCost (runAbility a 1)
    ] <> [ability a 2 HeroResource (OwnsThis <> PayingFor (CardWithType EventType <> CardMatchesAny [CardWithTrait Trait.Attack, CardWithTrait Trait.Defense])) ExhaustCost (Pay $ ResourcePayment Physical) | toCardCode a == toCardCode Cards.solid]

instance HasModifiersFor Solid where
  getModifiersFor _ (IdentityRef you) (Solid a) | you == upgradeController a && toCardCode a == toCardCode Cards.phased = do
    defending <- isIdentityDefending you
    pure [CannotTakeDamage | defending]
  getModifiersFor _ _ _ = pure []

instance RunMessage Solid where
  runMessage msg e@(Solid attrs) = runQueueT $ case msg of
    CardSpecific (isTarget attrs -> True) "flipMassForm" _ -> do
      massFormChanged (upgradeController attrs) (upgradeId attrs)
      pure $ Solid attrs {upgradeCardDef = if toCardCode attrs == toCardCode Cards.solid then Cards.phased else Cards.solid}
    RanAbility you (isTarget attrs -> True) 1 _ _ -> e <$ flipMassForm you (upgradeId attrs)
    _ -> Solid <$> runMessage msg attrs

phased :: UpgradeCard Solid
phased = upgrade Solid Cards.phased
