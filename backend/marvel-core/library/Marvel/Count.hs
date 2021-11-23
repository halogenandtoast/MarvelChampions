module Marvel.Count where

import Marvel.Prelude

class Count a where
  data QueryCount a
  type Backend a
  selectCount :: MonadReader (Backend a) m => QueryCount a -> a -> m Natural
