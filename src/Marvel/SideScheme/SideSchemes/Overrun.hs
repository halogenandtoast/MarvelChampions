module Marvel.SideScheme.SideSchemes.Overrun (
  overrun,
  Overrun (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.GameValue
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.Card
import Marvel.Game.Source
import Marvel.Trait (Trait (Goblin))

overrun :: SideSchemeCard Overrun
overrun = sideScheme Overrun Cards.overrun (PerPlayer 1)

newtype Overrun = Overrun (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | When Defeated: in player order, each player discards 2 encounter cards and puts each
-- Goblin minion of them into play engaged with them.
-- ponytail: the cards each will discard are read at once, as the deck is when it is
-- defeated; a deck emptied partway reshuffles only after
instance RunMessage Overrun where
  runMessage msg s@(Overrun attrs) = runQueueT $ case msg of
    SideSchemeMessage ident DefeatSideScheme | ident == sideSchemeId attrs -> do
      players <- getPlayers
      deck <- getEncounterDeck
      for_ (zip [0 ..] players) \(i, p) -> do
        discardTopOfEncounterDeck 2
        for_ (filter isGoblinMinion $ take 2 $ drop (2 * i) deck) (putEncounterCardIntoPlay p)
      pure s
    _ -> Overrun <$> runMessage msg attrs

isGoblinMinion :: EncounterCard -> Bool
isGoblinMinion card = cdCardType (getCardDef card) == MinionType && Goblin `member` cdTraits (getCardDef card)
