module Marvel.Treachery.Treacheries.KlawsVengeance
  ( klawsVengeance
  , KlawsVengeance(..)
  ) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Choice
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message
import Marvel.Query
import Marvel.Question
import Marvel.Queue
import Marvel.Source
import Marvel.Target
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

klawsVengeance :: TreacheryCard KlawsVengeance
klawsVengeance = treachery KlawsVengeance Cards.klawsVengeance

newtype KlawsVengeance = KlawsVengeance (Attrs Treachery)
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsSource, IsTarget)

instance RunMessage KlawsVengeance where
  runMessage msg t@(KlawsVengeance attrs) = case msg of
    TreacheryMessage ident msg' | treacheryId attrs == ident -> case msg' of
      RevealTreachery identityId -> do
        isHero <- identityMatches HeroIdentity identityId
        if isHero
          then do
            villainId <- selectJust ActiveVillain
            push $ VillainMessage villainId $ VillainAttacks identityId
          else push $ IdentityMessage identityId $ DiscardFrom
            RandomFromHand
            1
            Nothing
        pure t
      _ -> KlawsVengeance <$> runMessage msg attrs
    AllyMessage _ (AllyDamaged _ _) -> do
      player <- getActivePlayerId
      t <$ pushChoice player (PlaceThreat (toSource attrs) 1 MainScheme)
    IdentityMessage identityId (IdentityDamaged _ _) -> do
      t <$ pushChoice identityId (PlaceThreat (toSource attrs) 1 MainScheme)
    _ -> KlawsVengeance <$> runMessage msg attrs
