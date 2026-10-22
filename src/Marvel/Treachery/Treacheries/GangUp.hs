module Marvel.Treachery.Treacheries.GangUp (
  gangUp,
  GangUp (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

gangUp :: TreacheryCard GangUp
gangUp = treachery GangUp Cards.gangUp

newtype GangUp = GangUp (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage GangUp where
  runMessage msg t@(GangUp attrs) = runQueueT $ case msg of
    TreacheryMessage ident msg' | treacheryId attrs == ident -> case msg' of
      RevealTreachery identityId -> do
        isHero <- identityMatches HeroIdentity identityId
        if isHero
          then do
            -- with no villain in play (as between one Kang and the next), the minions
            villain <- selectOne ActiveVillain
            minions <- selectList $ MinionEngagedWith $ IdentityWithId identityId
            for_ villain (`enemyAttacks` identityId)
            for_ minions (`enemyAttacks` identityId)
            pure t
          else pure . GangUp $ attrs & surgeL .~ True
      _ -> GangUp <$> runMessage msg attrs
    _ -> GangUp <$> runMessage msg attrs
