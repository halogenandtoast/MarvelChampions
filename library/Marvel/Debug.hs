module Marvel.Debug where

import Marvel.Prelude

newtype DebugLogger = DebugLogger (forall a . Show a => a -> IO ())

class HasDebugLogger a where
  debugLogger :: a -> Maybe DebugLogger

debug
  :: (MonadReader env m, MonadIO m, HasDebugLogger env, Show a) => a -> m ()
debug a = do
  mlogger <- asks debugLogger
  maybe (pure ()) (\(DebugLogger f) -> liftIO $ f a) mlogger
