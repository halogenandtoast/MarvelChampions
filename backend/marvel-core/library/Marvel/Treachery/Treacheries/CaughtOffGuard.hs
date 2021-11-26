module Marvel.Treachery.Treacheries.CaughtOffGuard
  ( caughtOffGuard
  , CaughtOffGuard(..)
  ) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message
import Marvel.Query
import Marvel.Question
import Marvel.Queue
import Marvel.Source
import Marvel.Target
import Marvel.Treachery.Attrs
import Marvel.Treachery.Cards qualified as Cards

caughtOffGuard :: TreacheryCard CaughtOffGuard
caughtOffGuard = treachery CaughtOffGuard Cards.caughtOffGuard

newtype CaughtOffGuard = CaughtOffGuard TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage CaughtOffGuard where
  runMessage msg t@(CaughtOffGuard attrs) = case msg of
    TreacheryMessage treacheryId msg' | toId attrs == treacheryId ->
      case msg' of
        RevealTreachery identityId -> do
          supports <-
            selectMap SupportTarget $ SupportControlledBy $ IdentityWithId
              identityId
          upgrades <-
            selectMap UpgradeTarget $ UpgradeControlledBy $ IdentityWithId
              identityId
          if null supports && null upgrades
            then push $ Surge identityId
            else chooseOne
              identityId
              ([ TargetLabel support [DiscardTarget support]
               | support <- supports
               ]
              <> [ TargetLabel upgrade [DiscardTarget upgrade]
                 | upgrade <- upgrades
                 ]
              )
          pure t
        _ -> CaughtOffGuard <$> runMessage msg attrs
    _ -> CaughtOffGuard <$> runMessage msg attrs
