module Marvel.Treachery.Treacheries.KlawsVengeance (
  klawsVengeance,
  KlawsVengeance (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

klawsVengeance :: TreacheryCard KlawsVengeance
klawsVengeance = treachery KlawsVengeance Cards.klawsVengeance

newtype KlawsVengeance = KlawsVengeance (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage KlawsVengeance where
  runMessage msg t@(KlawsVengeance attrs) = runQueueT $ case msg of
    TreacheryMessage ident msg' | treacheryId attrs == ident -> case msg' of
      RevealTreachery identityId -> do
        isHero <- identityMatches HeroIdentity identityId
        if isHero
          then do
            traverse_ (\villainId -> enemyAttacks villainId identityId) =<< selectOne ActiveVillain
          else discardFrom identityId RandomFromHand 1
        pure t
      _ -> KlawsVengeance <$> runMessage msg attrs
    AllyMessage _ (AllyDamaged _ _) -> t <$ placeThreatOnEach attrs 1 MainScheme
    IdentityMessage _ (IdentityDamaged _ _) -> t <$ placeThreatOnEach attrs 1 MainScheme
    _ -> KlawsVengeance <$> runMessage msg attrs
