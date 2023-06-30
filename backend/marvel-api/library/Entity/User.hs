{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}

module Entity.User where

import Data.Text (Text)
import Database.Persist.TH
import Prelude

mkPersist
  sqlSettings
  [persistLowerCase|
User json sql=users
    username Text
    email Text
    passwordDigest Text
    UniqueUsername username
    UniqueEmail email
|]

deriving stock instance Show User
