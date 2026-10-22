module Marvel.Upgrade.Upgrades.CaptainAmericasHelmet (
  captainAmericasHelmet,
  CaptainAmericasHelmet (..),
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

captainAmericasHelmet :: UpgradeCard CaptainAmericasHelmet
captainAmericasHelmet = upgrade CaptainAmericasHelmet Cards.captainAmericasHelmet

newtype CaptainAmericasHelmet = CaptainAmericasHelmet (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities CaptainAmericasHelmet where
  getAbilities a@(CaptainAmericasHelmet attrs) =
    [ limitedWindowAbility
        a
        1
        (IdentityWouldBeDefeated $ IdentityWithId $ upgradeController attrs)
        Interrupt
        (OwnsThis <> InHeroForm)
        NoCost
        $ runAbility a 1
    ]

-- | The defeat is taken off the queue: the hit point dial is set to 1 instead.
instance RunMessage CaptainAmericasHelmet where
  runMessage msg a@(CaptainAmericasHelmet attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget a -> True) 1 _ _ -> do
      cancelMatchingMessage (== IdentityMessage ident IdentityDefeated)
      setHitPoints ident 1
      discardUpgrade (upgradeId attrs)
      pure a
    _ -> CaptainAmericasHelmet <$> runMessage msg attrs
