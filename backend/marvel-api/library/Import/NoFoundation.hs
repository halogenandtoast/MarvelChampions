{-# LANGUAGE CPP #-}

module Import.NoFoundation (
  module Import.NoFoundation,
  module Import,
) where

import Prelude as Import

import Database.Persist.Postgresql as Import
import Model as Import
import Settings as Import
import Yesod.Core as Import
import Yesod.Core.Types as Import (loggerSet)
import Yesod.Persist.Core as Import

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x : _) = Just x
