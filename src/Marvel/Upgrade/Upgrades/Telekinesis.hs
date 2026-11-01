module Marvel.Upgrade.Upgrades.Telekinesis where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ability.Types qualified as Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.GameValue ()
import Marvel.Hand ()
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Query ()
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Stats ()
import Marvel.Trait qualified as Trait ()
import Marvel.Window qualified as W ()
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

telekinesis :: UpgradeCard Telekinesis
telekinesis = upgrade Telekinesis Cards.telekinesis

newtype Telekinesis = Telekinesis (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Telekinesis where
  getAbilities a = [subtype Ability.Attack $ ability a 1 HeroAction OwnsThis (ExhaustCost <> MultiResourceCost [Just Mental, Just Mental]) (runAbility a 1)]

instance RunMessage Telekinesis where
  runMessage msg e@(Telekinesis a) = runQueueT $ case msg of
    RanAbility you (isTarget a -> True) 1 _ _ -> e <$ chooseDamage you a (toDamage 3 $ FromPlayerAttack you) AttackableEnemy
    _ -> Telekinesis <$> runMessage msg a
