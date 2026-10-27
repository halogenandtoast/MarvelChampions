module Marvel.Upgrade.Upgrades.TooStubbornToDie (
  tooStubbornToDie,
  TooStubbornToDie (..),
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

tooStubbornToDie :: UpgradeCard TooStubbornToDie
tooStubbornToDie = upgrade TooStubbornToDie Cards.tooStubbornToDie

newtype TooStubbornToDie = TooStubbornToDie (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | "Hero Interrupt: When Drax would be defeated, instead set his hit point dial to 4,
-- change him to alter-ego form, and remove this card from the game."
instance HasAbilities TooStubbornToDie where
  getAbilities a@(TooStubbornToDie attrs) =
    [ limitedWindowAbility
        a
        1
        (IdentityWouldBeDefeated $ IdentityWithId $ upgradeController attrs)
        HeroInterrupt
        OwnsThis
        NoCost
        (runAbility a 1)
    ]

instance RunMessage TooStubbornToDie where
  runMessage msg u@(TooStubbornToDie attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> do
      cancelMatchingMessage (== IdentityMessage ident IdentityDefeated)
      setHitPoints ident 4
      changeToAlterEgo ident
      removeFromGame attrs
      pure u
    _ -> TooStubbornToDie <$> runMessage msg attrs
