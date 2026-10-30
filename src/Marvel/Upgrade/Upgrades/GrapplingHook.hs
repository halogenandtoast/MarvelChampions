module Marvel.Upgrade.Upgrades.GrapplingHook (
  grapplingHook,
  GrapplingHook (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card
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

grapplingHook :: UpgradeCard GrapplingHook
grapplingHook = upgrade GrapplingHook Cards.grapplingHook

newtype GrapplingHook = GrapplingHook (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities GrapplingHook where
  getAbilities a@(GrapplingHook attrs) =
    [ limitedWindowAbility
        a
        1
        ( EncounterCardRevealedBy
            When
            (IdentityWithId $ upgradeController attrs)
            (EncounterCardMatching $ CardWithType TreacheryType)
        )
        HeroInterrupt
        (OwnsThis <> CanCancelRevealedEncounterCard)
        (DiscardCost $ toTarget a)
        (runAbility a 1)
    ]

-- | The treachery is discarded instead of resolving: nothing of it resolves, surge
-- included.
instance RunMessage GrapplingHook where
  runMessage msg u@(GrapplingHook attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 [EncounterCardRevealed _ _ card] _ -> do
      flip insteadOf (discardCard (EncounterCard card)) \case
        RevealedEncounterCard _ card' -> card' == card
        RevealedEncounterCardFrom _ _ card' -> card' == card
        _ -> False
      pure u
    _ -> GrapplingHook <$> runMessage msg attrs
