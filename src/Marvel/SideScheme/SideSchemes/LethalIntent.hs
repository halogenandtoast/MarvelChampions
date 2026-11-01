module Marvel.SideScheme.SideSchemes.LethalIntent (
  lethalIntent,
  LethalIntent (..),
) where

import Marvel.Prelude

import Marvel.Card
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref ()
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.Trait

lethalIntent :: SideSchemeCard LethalIntent
lethalIntent = sideScheme LethalIntent Cards.lethalIntent (PerPlayer 2)

newtype LethalIntent = LethalIntent (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | When Revealed: discard cards from the top of the encounter deck until a Technique
-- attachment is discarded, and reveal it (for the player revealing this card, the
-- active one).
instance RunMessage LethalIntent where
  runMessage msg s@(LethalIntent attrs) = runQueueT $ case msg of
    SideSchemeMessage ident RevealSideScheme | ident == sideSchemeId attrs -> do
      discardUntil FromEncounterDeck (CardWithType AttachmentType <> CardWithTrait Technique) attrs
      pure s
    WithDiscardedMatch (isTarget attrs -> True) _ (EncounterCard card) -> do
      you <- getActivePlayerId
      revealEncounterCard you card
      pure s
    _ -> LethalIntent <$> runMessage msg attrs
