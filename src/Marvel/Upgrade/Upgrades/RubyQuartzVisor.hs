module Marvel.Upgrade.Upgrades.RubyQuartzVisor where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue ()
import Marvel.Hand ()
import Marvel.Id ()
import Marvel.Keyword ()
import Marvel.Matchers ()
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Query ()
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Stats ()
import Marvel.Window qualified as W ()
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

rubyQuartzVisor :: UpgradeCard RubyQuartzVisor
rubyQuartzVisor = upgrade RubyQuartzVisor Cards.rubyQuartzVisor

newtype RubyQuartzVisor = RubyQuartzVisor (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities RubyQuartzVisor where
  getAbilities a =
    [ (ability a 1 HeroResource (OwnsThis <> PayingForAbility "33001a" 1) ExhaustCost (Pay $ ResourcePayment Energy))
        {abilityChoices = [Pay (ResourcePayment Energy), runAbility a 1]}
    ]

instance RunMessage RubyQuartzVisor where
  runMessage msg u@(RubyQuartzVisor attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      scope <- getActiveCost
      cardSpecific you "cyclopsVisor" scope
      pure u
    _ -> RubyQuartzVisor <$> runMessage msg attrs
