module Marvel.Upgrade.Upgrades.Telepathy where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ability.Types qualified as Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
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

telepathy :: UpgradeCard Telepathy
telepathy = upgrade Telepathy Cards.telepathy

newtype Telepathy = Telepathy (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Telepathy where
  getAbilities a = [subtype Ability.Thwart $ ability a 1 HeroAction OwnsThis (ExhaustCost <> MultiResourceCost [Just Mental, Just Mental]) (runAbility a 1)]

instance RunMessage Telepathy where
  runMessage msg e@(Telepathy a) = runQueueT $ case msg of
    RanAbility you (isTarget a -> True) 1 _ _ -> e <$ removeThreat you a 2 ThwartableScheme
    _ -> Telepathy <$> runMessage msg a
