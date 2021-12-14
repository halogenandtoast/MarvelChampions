module Marvel.Treachery.Treacheries.KlawsVengeance
  ( klawsVengeance
  , KlawsVengeance(..)
  )
where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message
import Marvel.Question
import Marvel.Query
import Marvel.Queue
import Marvel.Source
import Marvel.Target
import Marvel.Treachery.Attrs
import Marvel.Treachery.Cards qualified as Cards

klawsVengeance :: TreacheryCard KlawsVengeance
klawsVengeance = treachery KlawsVengeance Cards.klawsVengeance

newtype KlawsVengeance = KlawsVengeance TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage KlawsVengeance where
  runMessage msg t@(KlawsVengeance attrs) = case msg of
    TreacheryMessage treacheryId msg' | toId attrs == treacheryId ->
      case msg' of
        RevealTreachery ident -> do
          isHero <- identityMatches HeroIdentity ident
          if isHero
            then do
              villainId <- selectJust ActiveVillain
              push $ VillainMessage villainId $ VillainAttacks ident
            else push $ IdentityMessage ident $ DiscardFrom RandomFromHand 1 Nothing
          pure t
        _ -> KlawsVengeance <$> runMessage msg attrs
    AllyMessage _ (AllyDamaged _ _) -> do
      player <- getActivePlayerId
      t <$ pushChoice player (PlaceThreat (toSource attrs) 1 MainScheme)
    IdentityMessage ident (IdentityDamaged _ _) -> do
      t <$ pushChoice ident (PlaceThreat (toSource attrs) 1 MainScheme)
    _ -> KlawsVengeance <$> runMessage msg attrs
