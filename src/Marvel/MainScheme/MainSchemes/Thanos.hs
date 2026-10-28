module Marvel.MainScheme.MainSchemes.Thanos (
  theInfinityStones,
  TheInfinityStones (..),
  balanceTheScales,
  BalanceTheScales (..),
) where

import Marvel.Prelude

import Marvel.Card
import Marvel.Deck
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Identity.Types (Field (PlayerIdentityDeck))
import Marvel.MainScheme.Cards qualified as Cards
import Marvel.MainScheme.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Projection
import Marvel.Query
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as SideSchemes

theInfinityStones :: MainSchemeCard TheInfinityStones
theInfinityStones = mainScheme TheInfinityStones Cards.theInfinityStones (PerPlayer 12) (PerPlayer 0) (PerPlayer 1)

newtype TheInfinityStones = TheInfinityStones (Attrs MainScheme)
  deriving anyclass (IsMainScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | 1A has no setup of its own: the Infinity Gauntlet's Setup keyword attaches it and
-- makes the stone deck. 1B When Revealed: put the top card of the Infinity Stone deck
-- into play, then search the encounter deck for Sanctuary and reveal it.
instance RunMessage TheInfinityStones where
  runMessage msg ms@(TheInfinityStones attrs) = runQueueT $ case msg of
    MainSchemeMessage ident RevealMainScheme | ident == mainSchemeId attrs -> do
      you <- selectJust FirstPlayer
      putTopInfinityStoneIntoPlay you
      searchForAndRevealScheme SideSchemes.sanctuary
      pure ms
    AdvanceMainScheme ((== mainSchemeId attrs) -> True) -> ms <$ nextMainScheme (mainSchemeId attrs)
    _ -> TheInfinityStones <$> runMessage msg attrs

balanceTheScales :: MainSchemeCard BalanceTheScales
balanceTheScales = mainScheme BalanceTheScales Cards.balanceTheScales (PerPlayer 12) (PerPlayer 0) (PerPlayer 1)

newtype BalanceTheScales = BalanceTheScales (Attrs MainScheme)
  deriving anyclass (IsMainScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | When Revealed: each player shuffles their discard pile into their deck, then removes
-- the top half of their deck (rounded down) from the game. Completed: the players lose.
instance RunMessage BalanceTheScales where
  runMessage msg ms@(BalanceTheScales attrs) = runQueueT $ case msg of
    MainSchemeMessage ident RevealMainScheme | ident == mainSchemeId attrs -> do
      players <- getPlayers
      for_ players shuffleDiscardIntoDeck
      -- the decks as they are once shuffled
      cardSpecific attrs "balanceTheScales" ()
      pure ms
    CardSpecific (isTarget attrs -> True) "balanceTheScales" _ -> do
      players <- getPlayers
      for_ players \p -> do
        deck <- projectMap PlayerIdentityDeck unDeck p
        for_ (take (length deck `div` 2) deck) removeFromGame
      pure ms
    AdvanceMainScheme ((== mainSchemeId attrs) -> True) -> ms <$ gameOver Lost
    _ -> BalanceTheScales <$> runMessage msg attrs
