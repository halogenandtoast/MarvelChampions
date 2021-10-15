module Marvel.Identity.Attrs
  ( module Marvel.Identity.Attrs
  , module X
  ) where

import Marvel.Prelude

import GHC.Generics
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Card.PlayerCard
import Marvel.Entity
import {-# SOURCE #-} Marvel.Game
import Marvel.Hp
import Marvel.Id as X
import Marvel.Message
import Marvel.Queue

data IdentityAttrs = IdentityAttrs
  { identityAttrsId :: IdentityId
  , identityAttrsCardDef :: CardDef
  , identityAttrsStartingHP :: HP
  , identityAttrsMaxHP :: HP
  , identityAttrsCurrentHP :: HP
  , identityAttrsDeck :: [PlayerCard]
  , identityAttrsPassed :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

defaultAttrs :: IdentityId -> CardDef -> HP -> IdentityAttrs
defaultAttrs ident cardDef hp = IdentityAttrs
  { identityAttrsId = ident
  , identityAttrsCardDef = cardDef
  , identityAttrsStartingHP = hp
  , identityAttrsCurrentHP = hp
  , identityAttrsMaxHP = hp
  , identityAttrsDeck = []
  , identityAttrsPassed = False
  }

deckL :: Lens' IdentityAttrs [PlayerCard]
deckL = lens identityAttrsDeck \m x -> m { identityAttrsDeck = x }

passedL :: Lens' IdentityAttrs Bool
passedL = lens identityAttrsPassed \m x -> m { identityAttrsPassed = x }

takeTurn :: MonadGame env m => IdentityAttrs -> m IdentityAttrs
takeTurn attrs = do
  pushAll $ map (IdentityMessage $ toId attrs) [TakeAction, CheckIfPassed]
  pure attrs

runIdentityMessage
  :: MonadGame env m => IdentityMessage -> IdentityAttrs -> m IdentityAttrs
runIdentityMessage msg attrs@IdentityAttrs {..} = case msg of
  BeginTurn -> takeTurn attrs
  CheckIfPassed -> if identityAttrsPassed then pure attrs else takeTurn attrs
  SetDeck cards -> pure $ attrs & deckL .~ cards
  TakeAction -> pure $ attrs & passedL .~ True

instance RunMessage IdentityAttrs where
  runMessage msg attrs = case msg of
    IdentityMessage ident msg' | ident == toId attrs ->
      runIdentityMessage msg' attrs
    _ -> pure attrs

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
