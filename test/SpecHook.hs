module SpecHook (hook) where

import Test.Hspec

-- | Every example plays its own game, so they run side by side, one per core.
hook :: Spec -> Spec
hook = parallel
