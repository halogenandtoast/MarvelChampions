module Marvel.Upgrade.Upgrades.CosmicWard (
  cosmicWard,
  CosmicWard (..),
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

cosmicWard :: UpgradeCard CosmicWard
cosmicWard = upgrade CosmicWard Cards.cosmicWard

newtype CosmicWard = CosmicWard (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Forced Interrupt: When you reveal a treachery card, cancel its "When Revealed"
-- effects and discard it. Then, discard Cosmic Ward.
instance HasAbilities CosmicWard where
  getAbilities a@(CosmicWard attrs) =
    [ limitedWindowAbility
        a
        1
        (EncounterCardRevealedBy When (IdentityWithId $ upgradeController attrs) (EncounterCardMatching $ CardWithType TreacheryType))
        ForcedInterrupt
        OwnsThis
        NoCost
        (runAbility a 1)
    ]

-- | The card is discarded in place of resolving: none of its "When Revealed" (its incite
-- and surge among them) happens.
instance RunMessage CosmicWard where
  runMessage msg u@(CosmicWard attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 [EncounterCardRevealed _ _ card] _ -> do
      insteadOf (\case RevealedEncounterCard _ card' -> card' == card; _ -> False) $
        discardCard (EncounterCard card)
      discardUpgrade (upgradeId attrs)
      pure u
    _ -> CosmicWard <$> runMessage msg attrs
