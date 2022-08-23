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
import Marvel.Treachery.Types
import Marvel.Treachery.Cards qualified as Cards

gangUp :: TreacheryCard GangUp
gangUp = treachery GangUp Cards.gangUp

newtype GangUp = GangUp TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage GangUp where
  runMessage msg t@(GangUp attrs) = case msg of
    TreacheryMessage treacheryId msg' | toId attrs == treacheryId ->
      case msg' of
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
