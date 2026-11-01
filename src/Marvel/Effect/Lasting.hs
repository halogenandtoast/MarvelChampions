module Marvel.Effect.Lasting (Lasting (..)) where

import Marvel.Prelude
import Marvel.Effect.Types
import Marvel.Message.Lifted
import Marvel.Ref

newtype Lasting = Lasting (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance RunMessage Lasting where
  runMessage msg (Lasting attrs) = runQueueT $ Lasting <$> runMessage msg attrs
