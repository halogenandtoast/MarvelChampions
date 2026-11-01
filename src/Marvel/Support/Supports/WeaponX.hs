module Marvel.Support.Supports.WeaponX where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.Id ()
import Marvel.Matchers ()
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query ()
import Marvel.Question
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Stats ()
import Marvel.Keyword ()
import Marvel.Window qualified as W ()
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types

weaponX :: SupportCard WeaponX
weaponX = support WeaponX Cards.weaponX

newtype WeaponX = WeaponX (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities WeaponX where
  getAbilities (WeaponX a) =
    [ability a 1 AlterEgoAction OwnsThis (ExhaustCost <> DamageCost 1) (runAbility a 1)]

instance RunMessage WeaponX where
  runMessage msg e@(WeaponX attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      discardFromDeckUntil you HeroSpecificCard attrs
      pure e
    WithDiscardedMatch (isTarget attrs -> True) _ card -> do
      for_ (onlyPlayerCards [card]) (addToHand $ supportController attrs)
      pure e
    _ -> WeaponX <$> runMessage msg attrs
