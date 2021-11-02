{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Model
  ( module X
  ) where

import Database.Persist.Postgresql.JSON ()

import Entity.Marvel.Deck as X
import Entity.Marvel.Game as X
import Entity.Marvel.MarvelDBDecklist as X
import Entity.Marvel.Player as X
import Entity.User as X
