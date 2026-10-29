-- | The minion of the Enchantress modular set (The Mad Titan's Shadow).
module Marvel.Minion.Minions.Enchantress (
  enchantress,
  Enchantress (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Card.Def (CardMatcher (CardIs, CardInEncounterSet), cdEncounterSet)
import Marvel.EncounterSet qualified as EncounterSet
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
      -- Valkyrie's nemesis copy searches its own associated encounter set.
      -- The modular copy retains the generic Seduced search across printings.
      let associated = case cdEncounterSet (minionCardDef attrs) of
            Just EncounterSet.ValkyrieNemesis -> CardInEncounterSet EncounterSet.ValkyrieNemesis
            _ -> mempty
      searchEverywhereAndPutIntoPlayEncounterCard you (CardIs Attachments.seduced <> associated)
      Enchantress <$> runMessage msg attrs
    _ -> Enchantress <$> runMessage msg attrs
