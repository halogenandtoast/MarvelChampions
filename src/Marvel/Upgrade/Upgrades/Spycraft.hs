module Marvel.Upgrade.Upgrades.Spycraft (
  spycraft,
  Spycraft (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Queue
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Window

spycraft :: UpgradeCard Spycraft
spycraft = upgrade Spycraft Cards.spycraft

newtype Spycraft = Spycraft (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Spycraft where
  getAbilities a@(Spycraft attrs) =
    [ limitedWindowAbility
        a
        1
        (EncounterCardRevealedBy When (IdentityWithId $ upgradeController attrs) AnyEncounterCard)
        Interrupt
        OwnsThis
        (DiscardCost $ toTarget a)
        (runAbility a 1)
    ]

-- | The card is discarded instead of resolving, and another revealed in its place.
instance RunMessage Spycraft where
  runMessage msg u@(Spycraft attrs) = case msg of
    RanAbility _ (isTarget attrs -> True) 1 [EncounterCardRevealed ident card] _ -> do
      replaceMatchingMessage (const [DiscardedCard (EncounterCard card), DrawAndRevealEncounterCard ident]) \case
        RevealedEncounterCard _ card' -> card' == card
        _ -> False
      pure u
    _ -> Spycraft <$> runMessage msg attrs
