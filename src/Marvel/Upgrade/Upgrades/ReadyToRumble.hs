module Marvel.Upgrade.Upgrades.ReadyToRumble (
  readyToRumble,
  ReadyToRumble (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Window

readyToRumble :: UpgradeCard ReadyToRumble
readyToRumble = upgrade ReadyToRumble Cards.readyToRumble

newtype ReadyToRumble = ReadyToRumble (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Play under any player's control. Max 1 per player (the def). Hero Response: After you
-- change form (a flip or an energy form change), discard this card → ready your hero.
instance HasAbilities ReadyToRumble where
  getAbilities a@(ReadyToRumble attrs) =
    [ limitedWindowAbility
        a
        1
        (IdentityChangedAnyForm After (IdentityWithId $ upgradeController attrs))
        HeroResponse
        OwnsThis
        (DiscardCost $ toTarget a)
        (runAbility a 1)
    ]

instance RunMessage ReadyToRumble where
  runMessage msg u@(ReadyToRumble attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      ready (upgradeController attrs)
      pure u
    _ -> ReadyToRumble <$> runMessage msg attrs
