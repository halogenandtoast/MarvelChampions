module Marvel.Debug where

import Marvel.Prelude

newtype DebugLogger = DebugLogger (forall a . Show a => a -> IO ())

class HasDebugLogger env where
  debugL :: Lens' env (Maybe DebugLogger)

debug
  :: (MonadReader env m, MonadIO m, HasDebugLogger env, Show a) => a -> m ()
debug a = do
  mlogger <- asks $ view debugL
  maybe (pure ()) (\(DebugLogger f) -> liftIO $ f a) mlogger
