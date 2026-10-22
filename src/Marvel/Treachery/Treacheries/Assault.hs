module Marvel.Treachery.Treacheries.Assault (
  assault,
  Assault (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

assault :: TreacheryCard Assault
assault = treachery Assault Cards.assault

newtype Assault = Assault (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage Assault where
  runMessage msg t@(Assault attrs) = runQueueT $ case msg of
    TreacheryMessage ident msg' | treacheryId attrs == ident -> case msg' of
      RevealTreachery identityId -> do
        isHero <- identityMatches HeroIdentity identityId
        if isHero
          then do
            villainId <- selectJust ActiveVillain
            enemyAttacks villainId identityId
            pure t
          else pure . Assault $ attrs & surgeL .~ True
      _ -> Assault <$> runMessage msg attrs
    _ -> Assault <$> runMessage msg attrs
