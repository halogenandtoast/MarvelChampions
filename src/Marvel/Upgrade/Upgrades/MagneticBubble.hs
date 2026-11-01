module Marvel.Upgrade.Upgrades.MagneticBubble where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Deck ()
import Marvel.Discard ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.GameValue ()
import Marvel.Hand ()
import Marvel.Id ()
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Query ()
import Marvel.Question
import Marvel.Ref
import Marvel.Resource.Types ()
import Marvel.Stats ()
import Marvel.Status ()
import Marvel.Trait ()
import Marvel.Window qualified as W
import Marvel.Identity.Types ()

import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

magneticBubble :: UpgradeCard MagneticBubble
magneticBubble = upgrade MagneticBubble Cards.magneticBubble

newtype MagneticBubble = MagneticBubble (Attrs Upgrade)
  deriving anyclass (IsUpgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities MagneticBubble where
  getAbilities (MagneticBubble a) = [limitedWindowAbility a 1
    (W.WouldTakeDamage (IdentityWithId $ upgradeController a) FromAnyDamageSource AnyValue)
    ForcedInterrupt OwnsThis NoCost (runAbility a 1)]

instance HasModifiersFor MagneticBubble where
  getModifiersFor _ (IdentityRef you) _ = do
    magneto <- identityMatches (HeroIdentity <> IdentityWithTitle "Magneto") you
    pure [KeywordModifier (Retaliate 1) | magneto]
  getModifiersFor _ _ _ = pure []

instance RunMessage MagneticBubble where
  runMessage msg (MagneticBubble attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      n <- maybe 0 damageAmount <$> pendingDamage you
      preventDamage you n
      when (upgradeUses attrs + n >= 6) $ removeFromPlay attrs
      pure $ MagneticBubble attrs {upgradeUses = upgradeUses attrs + n}
    _ -> MagneticBubble <$> runMessage msg attrs
