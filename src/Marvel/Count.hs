module Marvel.Count where

import Marvel.Prelude

class Count m a where
  data QueryCount a
  selectCount :: QueryCount a -> a -> m Natural
