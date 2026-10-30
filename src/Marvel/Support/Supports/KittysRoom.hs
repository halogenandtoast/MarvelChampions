module Marvel.Support.Supports.KittysRoom where

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
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types
import Marvel.Upgrade.Cards qualified as Upgrades

kittysRoom :: SupportCard KittysRoom
kittysRoom = support KittysRoom Cards.kittysRoom

newtype KittysRoom = KittysRoom (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities KittysRoom where
  getAbilities (KittysRoom a) =
    [ability a 1 AlterEgoAction (OwnsThis <> AnyCriteria [UpgradeExists (UpgradeIs Upgrades.phased <> UpgradeControlledBy (IdentityWithId $ supportController a)), SelfMatches IdentityWithAnyDamage]) ExhaustCost (runAbility a 1)]

instance RunMessage KittysRoom where
  runMessage msg e@(KittysRoom attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      solid <- isInMassForm you Upgrades.solid
      if solid then heal you 2 else drawCards you 1
      pure e
    _ -> KittysRoom <$> runMessage msg attrs
