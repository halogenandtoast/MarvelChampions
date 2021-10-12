module Marvel.Identity.Attrs where

import Marvel.Prelude

import GHC.Generics
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Hp

newtype IdentityId = IdentityId UUID
  deriving newtype (Show, Eq, Random, Hashable)

data IdentityAttrs = IdentityAttrs
  { identityId :: IdentityId
  , identityCardCode :: CardCode
  , identityAttrsStartingHP :: HP
  }
  deriving stock (Show, Eq)

instance HasStartingHP IdentityAttrs where
  startingHP = identityAttrsStartingHP

instance HasCardCode IdentityAttrs where
  toCardCode = identityCardCode

instance Entity IdentityAttrs where
  type EntityId IdentityAttrs = IdentityId
  toId = identityId

class HasIdentityAttrs a where
  toIdentityAttrs :: a -> IdentityAttrs

instance HasIdentityAttrs IdentityAttrs where
  toIdentityAttrs = id

genericToIdentityAttrs
  :: (HasIdentityAttrs' (Rep a), Generic a) => a -> IdentityAttrs
genericToIdentityAttrs = toIdentityAttrs' . from

class HasIdentityAttrs' f where
  toIdentityAttrs' :: f p -> IdentityAttrs

instance HasIdentityAttrs' f => HasIdentityAttrs' (M1 i c f) where
  toIdentityAttrs' = toIdentityAttrs' . unM1

instance (HasIdentityAttrs' l, HasIdentityAttrs' r) => HasIdentityAttrs' (l :+: r) where
  toIdentityAttrs' = \case
    L1 x -> toIdentityAttrs' x
    R1 x -> toIdentityAttrs' x

instance HasIdentityAttrs c => HasIdentityAttrs' (K1 i c) where
  toIdentityAttrs' = toIdentityAttrs . unK1
