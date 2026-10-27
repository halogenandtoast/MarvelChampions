module Marvel.Upgrade.Upgrades.CargoHold (
  cargoHold,
  CargoHold (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

cargoHold :: UpgradeCard CargoHold
cargoHold = upgrade CargoHold Cards.cargoHold

newtype CargoHold = CargoHold (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Action: Exhaust Cargo Hold → heal 1 damage from a friendly character. If you
-- control the Milano, heal 1 damage from your identity. A friendly character with
-- damage is the target the ability needs (RR Target); the second sentence follows the
-- first, your identity healed again if it was the one chosen.
instance HasAbilities CargoHold where
  getAbilities a =
    [ability a 1 HeroAction (OwnsThis <> CharacterExists damagedFriend) ExhaustCost (runAbility a 1)]

damagedFriend :: CharacterMatcher
damagedFriend = CharacterControlledBy AnyIdentity <> CharacterWithAnyDamage

instance RunMessage CargoHold where
  runMessage msg u@(CargoHold attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      characters <- selectList damagedFriend
      chooseOne you [targeting (characterRef c) (heal c 1) | c <- characters]
      whenM (controlsMilano you) $ heal you 1
      pure u
    _ -> CargoHold <$> runMessage msg attrs
