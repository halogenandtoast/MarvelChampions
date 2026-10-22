module Marvel.Minion.Minions.ZolasMutate (
  zolasMutate,
  ZolasMutate (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner
import Marvel.Trait

zolasMutate :: MinionCard ZolasMutate
zolasMutate = minion ZolasMutate Cards.zolasMutate (Sch 1) (Atk 2) (HP 5)

newtype ZolasMutate = ZolasMutate (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | When Revealed: cards are discarded from the top of the encounter deck until a Tech
-- attachment is, and it is attached here. [star] Boost: this card is shuffled into the
-- encounter deck (not discarded).
instance RunMessage ZolasMutate where
  runMessage msg e@(ZolasMutate attrs) = runQueueT $ case msg of
    MinionMessage ident (RevealMinion _) | ident == minionId attrs -> do
      discardUntil FromEncounterDeck (CardWithType AttachmentType <> CardWithTrait Tech) attrs
      ZolasMutate <$> runMessage msg attrs
    WithDiscardedMatch (isTarget attrs -> True) _ (EncounterCard card) -> do
      attachEncounterCard card (minionId attrs)
      pure e
    Boost (RevealedAsBoost target _) | isTarget attrs target -> do
      shuffleIntoEncounterDeck [MkEncounterCard (CardId $ unMinionId $ minionId attrs) (getCardDef attrs)]
      pure e
    _ -> ZolasMutate <$> runMessage msg attrs
