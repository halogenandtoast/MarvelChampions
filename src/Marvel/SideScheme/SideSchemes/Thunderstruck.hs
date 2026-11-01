module Marvel.SideScheme.SideSchemes.Thunderstruck (
  thunderstruck,
  Thunderstruck (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.Status ()

thunderstruck :: SideSchemeCard Thunderstruck
thunderstruck = sideScheme Thunderstruck Cards.thunderstruck (Static 5)

newtype Thunderstruck = Thunderstruck (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Thunderball's side scheme: it cannot leave play while he is in play, and leaves the game
-- with him (his signature scheme, set up by Breakout).
--
-- Gamma Blast — Forced Response: after threat is placed here, if there is 10 or more threat here,
-- stun each friendly character. Remove all but 3 threat from this scheme.
instance RunMessage Thunderstruck where
  runMessage msg s@(Thunderstruck attrs) = runQueueT $ case msg of
    SideSchemeMessage ident (SideSchemePlaceThreat n) | ident == sideSchemeId attrs -> do
      Thunderstruck attrs' <- Thunderstruck <$> runMessage msg attrs
      when (n > 0 && sideSchemeThreat attrs' >= 10) do
        characters <- selectList (CharacterControlledBy AnyIdentity)
        for_ characters \c -> giveStatus c StunnedStatus
        cardSpecific attrs "allButThree" ()
      pure (Thunderstruck attrs')
    CardSpecific (isTarget attrs -> True) "allButThree" _ -> do
      removeSideSchemeThreat (sideSchemeId attrs) (subtractNatural 3 (sideSchemeThreat attrs))
      pure s
    _ -> Thunderstruck <$> runMessage msg attrs
