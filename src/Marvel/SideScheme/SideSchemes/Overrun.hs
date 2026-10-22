module Marvel.SideScheme.SideSchemes.Overrun (
  overrun,
  Overrun (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.GameValue
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.Card
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Trait (Trait (Goblin))

overrun :: SideSchemeCard Overrun
overrun = sideScheme Overrun Cards.overrun (PerPlayer 1)

newtype Overrun = Overrun (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | When Defeated: in player order, each player discards 2 encounter cards and puts each
-- Goblin minion of them into play engaged with them. Each discards from the deck as it is
-- then: one that empties it stops there, and the next discards from the deck made anew
-- (RR "Fully Resolve"). It is done before the scheme leaves play.
instance RunMessage Overrun where
  runMessage msg s@(Overrun attrs) = runQueueT $ case msg of
    SideSchemeMessage ident DefeatSideScheme | ident == sideSchemeId attrs -> do
      cardSpecific attrs "discard" =<< getPlayers
      pure s
    CardSpecific (isTarget attrs -> True) "discard" (fromJSON -> Success (players :: [IdentityId])) -> case players of
      [] -> pure s
      p : rest -> do
        discarded <- take 2 <$> getEncounterDeck
        discardTopOfEncounterDeck 2
        for_ (filter isGoblinMinion discarded) (putEncounterCardIntoPlay p)
        cardSpecific attrs "discard" rest
        pure s
    _ -> Overrun <$> runMessage msg attrs

isGoblinMinion :: EncounterCard -> Bool
isGoblinMinion card = cdCardType (getCardDef card) == MinionType && Goblin `member` cdTraits (getCardDef card)
