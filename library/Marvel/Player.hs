module Marvel.Player where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Hp
import Marvel.Identity

data Player = Player
  { playerIdentity :: PlayerIdentity
  , playerHitPoints :: HP
  }
  deriving stock (Show, Eq)

instance HasCardCode Player where
  toCardCode = toCardCode . playerIdentity

instance Entity Player where
  type EntityId Player = IdentityId
  toId = toId . playerIdentity

newPlayer :: PlayerIdentity -> Player
newPlayer ident = Player ident (startingHP ident)
