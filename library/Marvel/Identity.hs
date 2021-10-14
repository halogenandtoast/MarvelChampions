module Marvel.Identity
  ( module Marvel.Identity
  , module X
  ) where

import Marvel.Prelude

import Marvel.AlterEgo
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Entity
import Marvel.Hero
import Marvel.Hp
import Marvel.Message
import Marvel.Identity.Attrs as X

-- | Player Identity
-- An Identity is either a Hero or an alter ego
data PlayerIdentity = HeroSide Hero | AlterEgoSide AlterEgo
  deriving stock (Show, Eq, Generic)

instance RunMessage PlayerIdentity where
  runMessage msg = \case
    HeroSide x -> HeroSide <$> runMessage msg x
    AlterEgoSide x -> AlterEgoSide <$> runMessage msg x

lookupAlterEgo :: CardDef -> IdentityId -> Maybe PlayerIdentity
lookupAlterEgo cardDef ident =
  AlterEgoSide <$> (lookup (toCardCode cardDef) allAlterEgos <*> Just ident)

instance HasStartingHP PlayerIdentity where
  startingHP = defaultHasStartingHP

instance HasCardCode PlayerIdentity where
  toCardCode = toCardCode . toIdentityAttrs

instance HasIdentityAttrs PlayerIdentity where
  toIdentityAttrs = genericToIdentityAttrs

instance Entity PlayerIdentity where
  type EntityId PlayerIdentity = IdentityId
  toId = toId . toIdentityAttrs
