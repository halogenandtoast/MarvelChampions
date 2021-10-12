module Marvel.Identity
  ( module Marvel.Identity
  , module X
  ) where

import Marvel.Prelude

import Marvel.AlterEgo
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Hero
import Marvel.Hp
import Marvel.Identity.Attrs as X

-- | Player Identity
-- An Identity is either a Hero or an alter ego
data PlayerIdentity = HeroSide Hero | AlterEgoSide AlterEgo
  deriving stock (Show, Eq, Generic)

lookupAlterEgo :: CardCode -> IdentityId -> Maybe PlayerIdentity
lookupAlterEgo code ident =
  AlterEgoSide <$> (lookup code allAlterEgos <*> Just ident)

instance HasStartingHP PlayerIdentity where
  startingHP = defaultHasStartingHP

instance HasCardCode PlayerIdentity where
  toCardCode = toCardCode . toIdentityAttrs

instance HasIdentityAttrs PlayerIdentity where
  toIdentityAttrs = genericToIdentityAttrs

instance Entity PlayerIdentity where
  type EntityId PlayerIdentity = IdentityId
  toId = toId . toIdentityAttrs
