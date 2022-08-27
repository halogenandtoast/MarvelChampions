module Marvel.Treachery.Treacheries.FalseAlarm
  ( falseAlarm
  , FalseAlarm(..)
  ) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message
import Marvel.Queue
import Marvel.Source
import Marvel.Target
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

falseAlarm :: TreacheryCard FalseAlarm
falseAlarm = treachery FalseAlarm Cards.falseAlarm

newtype FalseAlarm = FalseAlarm (Attrs Treachery)
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsSource, IsTarget)

instance RunMessage FalseAlarm where
  runMessage msg t@(FalseAlarm attrs) = case msg of
    TreacheryMessage ident msg' | ident == treacheryId attrs -> case msg' of
      RevealTreachery identityId -> do
        isConfused <- identityMatches ConfusedIdentity identityId
        if not isConfused
          then do
            push $ IdentityMessage identityId IdentityConfused
            pure t
          else pure . FalseAlarm $ attrs & surgeL .~ True
      _ -> FalseAlarm <$> runMessage msg attrs
    _ -> FalseAlarm <$> runMessage msg attrs
