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
import Marvel.Treachery.Attrs
import Marvel.Treachery.Cards qualified as Cards

falseAlarm :: TreacheryCard FalseAlarm
falseAlarm = treachery FalseAlarm Cards.falseAlarm

newtype FalseAlarm = FalseAlarm TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage FalseAlarm where
  runMessage msg t@(FalseAlarm attrs) = case msg of
    TreacheryMessage tid msg' | tid == toId attrs -> case msg' of
      RevealTreachery ident -> do
        isConfused <- identityMatches ConfusedIdentity ident
        if not isConfused
          then do
            push $ IdentityMessage ident IdentityConfused
            pure t
          else pure . FalseAlarm $ attrs & surgeL .~ True
      _ -> FalseAlarm <$> runMessage msg attrs
    _ -> FalseAlarm <$> runMessage msg attrs
