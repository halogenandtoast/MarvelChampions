module Marvel.Treachery.Treacheries.GangUp
  ( gangUp
  , GangUp(..)
  ) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message
import Marvel.Query
import Marvel.Queue
import Marvel.Source
import Marvel.Target
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

gangUp :: TreacheryCard GangUp
gangUp = treachery GangUp Cards.gangUp

newtype GangUp = GangUp (Attrs Treachery)
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsSource, IsTarget)

instance RunMessage GangUp where
  runMessage msg t@(GangUp attrs) = case msg of
    TreacheryMessage ident msg' | treacheryId attrs == ident -> case msg' of
      RevealTreachery identityId -> do
        isHero <- identityMatches HeroIdentity identityId
        if isHero
          then do
            villainId <- selectJust ActiveVillain
            minions <- selectList $ MinionEngagedWith $ IdentityWithId
              identityId
            pushAll
              $ VillainMessage villainId (VillainAttacks identityId)
              : map (`MinionMessage` MinionAttacks identityId) minions
            pure t
          else pure . GangUp $ attrs & surgeL .~ True
      _ -> GangUp <$> runMessage msg attrs
    _ -> GangUp <$> runMessage msg attrs
