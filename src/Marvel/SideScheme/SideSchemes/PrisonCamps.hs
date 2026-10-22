module Marvel.SideScheme.SideSchemes.PrisonCamps (
  prisonCamps,
  PrisonCamps (..),
) where

import Marvel.Prelude

import Marvel.Card
import Marvel.Deck
import Marvel.Discard
import Marvel.Entity
import Marvel.GameValue
import Marvel.Identity.Types (Field (PlayerIdentityDeck, PlayerIdentityDiscard))
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Projection
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

prisonCamps :: SideSchemeCard PrisonCamps
prisonCamps = sideScheme PrisonCamps Cards.prisonCamps (PerPlayer 3)

newtype PrisonCamps = PrisonCamps (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | When Defeated: the player who defeated this scheme searches their deck and discard
-- pile for an ally, puts it into play, and shuffles their deck.
instance RunMessage PrisonCamps where
  runMessage msg s@(PrisonCamps attrs) = runQueueT $ case msg of
    SideSchemeMessage ident DefeatSideScheme | ident == sideSchemeId attrs -> do
      you <- getDefeatedBy attrs
      deck <- projectMap PlayerIdentityDeck unDeck you
      discard <- projectMap PlayerIdentityDiscard unDiscard you
      let allies = filter (cardMatch (CardWithType AllyType)) (deck <> discard)
      focusing (PlayerCard <$> allies) $
        chooseOne you $
          [labeled "$label.noMatchingCardsFound" (pure ()) | null allies]
            <> [ targeting c do
                  removeFromGame (CardIdRef $ pcCardId c)
                  putIntoPlay you c
               | c <- allies
               ]
      shuffleDeck you
      pure s
    _ -> PrisonCamps <$> runMessage msg attrs
