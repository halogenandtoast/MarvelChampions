module Marvel.Identity.Attrs where

import Marvel.Prelude

import GHC.Generics
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Entity
import Marvel.Hp

newtype IdentityId = IdentityId UUID
  deriving newtype (Show, Eq, Random, Hashable, ToJSON, FromJSON)

data IdentityAttrs = IdentityAttrs
  { identityAttrsId :: IdentityId
  , identityAttrsCardDef :: CardDef
  , identityAttrsStartingHP :: HP
  , identityAttrsMaxHP :: HP
  , identityAttrsCurrentHP :: HP
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance HasStartingHP IdentityAttrs where
  startingHP = identityAttrsStartingHP

instance HasCardCode IdentityAttrs where
  toCardCode = toCardCode . identityAttrsCardDef

instance Entity IdentityAttrs where
  type EntityId IdentityAttrs = IdentityId
  toId = identityAttrsId

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
