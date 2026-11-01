module Marvel.Ally.Allies.Lockheed where

import Marvel.Prelude
import Marvel.Card.Side ()
import Marvel.Payment ()
import Marvel.Game.Source ()
import Marvel.Hand ()
import Marvel.Stats
import Marvel.Status ()
import Marvel.Id ()
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.GameValue ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query ()
import Marvel.Question
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Trait qualified as Trait ()
import Marvel.Window qualified as W
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Upgrade.Cards qualified as Upgrades

lockheed :: AllyCard Lockheed
lockheed = ally Lockheed Cards.lockheed (Thw 1, 1) (Atk 1, 1) (HP 2)

newtype Lockheed = Lockheed (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Lockheed where
  getAbilities (Lockheed a) =
    [limitedWindowAbility a 1 W.EntersPlayThis Response (OwnsThis <> AnyCriteria [UpgradeExists (UpgradeIs Upgrades.solid <> UpgradeControlledBy (IdentityWithId $ allyController a)) <> EnemyExists AnyEnemy, UpgradeExists (UpgradeIs Upgrades.phased <> UpgradeControlledBy (IdentityWithId $ allyController a)) <> SchemeExists ThwartableScheme]) NoCost (runAbility a 1)]

instance RunMessage Lockheed where
  runMessage msg e@(Lockheed attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      solid <- isInMassForm you Upgrades.solid
      if solid then chooseDamage you attrs (toDamage 2 FromAbility) AnyEnemy else removeThreat you attrs 2 ThwartableScheme
      pure e
    _ -> Lockheed <$> runMessage msg attrs
