-- | The minion of the Enchantress modular set (The Mad Titan's Shadow).
module Marvel.Minion.Minions.Enchantress (
  enchantress,
  Enchantress (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Card.Def (CardMatcher (CardIs))
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

enchantress :: MinionCard Enchantress
enchantress = minion Enchantress Cards.enchantress (Sch 2) (Atk 1) (HP 5)

newtype Enchantress = Enchantress (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | When Revealed: search the encounter deck, discard pile, and set-aside area for a
-- copy of Seduced and attach it to your identity (put into play: its own "attach to
-- your identity"). Shuffle.
instance RunMessage Enchantress where
  runMessage msg (Enchantress attrs) = runQueueT $ case msg of
    MinionMessage ident (RevealMinion you) | ident == minionId attrs -> do
      searchEverywhereAndPutIntoPlayEncounterCard you (CardIs Attachments.seduced)
      Enchantress <$> runMessage msg attrs
    _ -> Enchantress <$> runMessage msg attrs
