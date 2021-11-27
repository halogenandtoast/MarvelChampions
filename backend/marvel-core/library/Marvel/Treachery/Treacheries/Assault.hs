module Marvel.Treachery.Treacheries.Assault
  ( assault
  , Assault(..)
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
import Marvel.Treachery.Attrs
import Marvel.Treachery.Cards qualified as Cards

assault :: TreacheryCard Assault
assault = treachery Assault Cards.assault

newtype Assault = Assault TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage Assault where
  runMessage msg t@(Assault attrs) = case msg of
    TreacheryMessage treacheryId msg' | toId attrs == treacheryId ->
      case msg' of
        RevealTreachery identityId -> do
          isHero <- identityMatches HeroIdentity identityId
          if isHero
            then do
              villainId <- selectJust ActiveVillain
              push $ VillainMessage villainId $ VillainAttacks identityId
              pure t
            else pure . Assault $ attrs & surgeL .~ True
        _ -> Assault <$> runMessage msg attrs
    _ -> Assault <$> runMessage msg attrs
