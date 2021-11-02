module Marvel.Identity
  ( module Marvel.Identity
  , module X
  ) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.AlterEgo
import Marvel.AlterEgo.Attrs
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Card.PlayerCard
import Marvel.Card.Side
import Marvel.Entity
import Marvel.Hero
import Marvel.Identity.Attrs as X
import Marvel.Message

data PlayerIdentitySide = HeroSide Hero | AlterEgoSide AlterEgo
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Player Identity
-- An Identity is either a Hero or an alter ego
data PlayerIdentity = PlayerIdentity
  { playerIdentitySide :: PlayerIdentitySide
  , playerIdentitySides :: HashMap Side PlayerIdentitySide
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

createIdentity :: PlayerIdentitySide -> PlayerIdentitySide -> PlayerIdentity
createIdentity alterEgoSide heroSide = PlayerIdentity
  { playerIdentitySide = alterEgoSide
  , playerIdentitySides = fromList [(A, heroSide), (B, alterEgoSide)]
  }

setDeck :: [PlayerCard] -> PlayerIdentity -> PlayerIdentity
setDeck deck = identityAttrsL . deckL .~ deck

instance RunMessage PlayerIdentity where
  runMessage msg player = case msg of
    IdentityMessage ident (ChangedToForm side) | ident == toId player -> do
      let
        newSide =
          fromMaybe (error "stuff") $ lookup side (playerIdentitySides player)
      pure $ player { playerIdentitySide = newSide }
    other -> case playerIdentitySide player of
      HeroSide x -> do
        newSide <- HeroSide <$> runMessage other x
        pure $ player { playerIdentitySide = newSide }
      AlterEgoSide x -> do
        newSide <- AlterEgoSide <$> runMessage other x
        pure $ player { playerIdentitySide = newSide }

lookupAlterEgo :: CardDef -> IdentityId -> Maybe PlayerIdentitySide
lookupAlterEgo cardDef ident =
  AlterEgoSide <$> (lookup (toCardCode cardDef) allAlterEgos <*> Just ident)

lookupHero :: CardDef -> IdentityId -> Maybe PlayerIdentitySide
lookupHero cardDef ident =
  HeroSide <$> (lookup (toCardCode cardDef) allHeroes <*> Just ident)

instance HasStartingHP PlayerIdentity where
  startingHP = startingHP . view identityAttrsL

instance HasCardCode PlayerIdentity where
  toCardCode = toCardCode . view identityAttrsL

instance HasAbilities PlayerIdentity where
  getAbilities x = case playerIdentitySide x of
    HeroSide a -> getAbilities a
    AlterEgoSide a -> getAbilities a

instance HasIdentityAttrs PlayerIdentity where
  identityAttrsL = lens
    (view identityAttrsL . playerIdentitySide)
    \m x ->
      m { playerIdentitySide = set identityAttrsL x (playerIdentitySide m) }

instance HasIdentityAttrs PlayerIdentitySide where
  identityAttrsL = genericToIdentityAttrs

instance Entity PlayerIdentity where
  type EntityId PlayerIdentity = IdentityId
  type EntityAttrs PlayerIdentity = IdentityAttrs
  toId = toId . toAttrs
  toAttrs = view identityAttrsL
