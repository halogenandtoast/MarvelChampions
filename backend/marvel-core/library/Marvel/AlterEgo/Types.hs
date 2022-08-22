module Marvel.AlterEgo.Types
  ( module Marvel.AlterEgo.Types
  , module X
  ) where

import Marvel.Prelude

import Data.Typeable
import Marvel.Ability.Type
import Marvel.Card.Builder
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Card.Side
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Hp as X
import Marvel.Id as X
import Marvel.Message
import Marvel.Modifier
import Marvel.Queue
import Marvel.Source
import Marvel.Stats
import Marvel.Target
import Marvel.Trait
import Text.Show qualified

data AlterEgo = forall a . IsAlterEgo a => AlterEgo a

instance Show AlterEgo where
  show (AlterEgo a) = show a

instance ToJSON AlterEgo where
  toJSON (AlterEgo a) = toJSON a

instance Eq AlterEgo where
  AlterEgo (a :: a) == AlterEgo (b :: b) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

data SomeAlterEgoCard = forall a . IsAlterEgo a => SomeAlterEgoCard
  (AlterEgoCard a)

liftAlterEgoCard :: (forall a . AlterEgoCard a -> b) -> SomeAlterEgoCard -> b
liftAlterEgoCard f (SomeAlterEgoCard a) = f a

someAlterEgoCardCode :: SomeAlterEgoCard -> CardCode
someAlterEgoCardCode = liftAlterEgoCard cbCardCode

instance HasTraits AlterEgo where
  getTraits = pure . cdTraits . getCardDef

instance HasStartingHP AlterEgo where
  startingHP = startingHP . toAttrs

instance HasHandSize AlterEgo where
  handSize = handSize . toAttrs

instance HasCardCode AlterEgo where
  toCardCode = toCardCode . toAttrs

instance HasCardDef AlterEgo where
  getCardDef = getCardDef . toAttrs

instance IsSource AlterEgo where
  toSource = toSource . toAttrs

instance Entity AlterEgo where
  type EntityId AlterEgo = IdentityId
  type EntityAttrs AlterEgo = AlterEgoAttrs
  toId = toId . toAttrs
  toAttrs (AlterEgo a) = toAttrs a

instance HasModifiersFor AlterEgo where
  getModifiersFor source target (AlterEgo a) = getModifiersFor source target a

alterEgo
  :: (AlterEgoAttrs -> a)
  -> CardDef
  -> HP GameValue
  -> HandSize
  -> Rec
  -> [CardDef]
  -> CardBuilder IdentityId a
alterEgo f cardDef hp hSize recovery obligations = CardBuilder
  { cbCardCode = cdCardCode cardDef
  , cbCardBuilder = \ident -> f $ AlterEgoAttrs
    { alterEgoIdentityId = ident
    , alterEgoBaseHandSize = hSize
    , alterEgoBaseRecovery = recovery
    , alterEgoHeroForms = [A]
    , alterEgoStartingHP = hp
    , alterEgoCardDef = cardDef
    , alterEgoObligations = obligations
    }
  }

class (Typeable a, Show a, Eq a, ToJSON a, FromJSON a, Entity a, EntityAttrs a ~ AlterEgoAttrs, EntityId a ~ IdentityId, HasModifiersFor a, HasAbilities a, RunMessage a, IsSource a) => IsAlterEgo a

type AlterEgoCard a = CardBuilder IdentityId a

data AlterEgoAttrs = AlterEgoAttrs
  { alterEgoIdentityId :: IdentityId
  , alterEgoBaseHandSize :: HandSize
  , alterEgoBaseRecovery :: Rec
  , alterEgoHeroForms :: [Side]
  , alterEgoStartingHP :: HP GameValue
  , alterEgoCardDef :: CardDef
  , alterEgoObligations :: [CardDef]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance HasCardCode AlterEgoAttrs where
  toCardCode = toCardCode . alterEgoCardDef

instance HasCardDef AlterEgoAttrs where
  getCardDef = alterEgoCardDef

instance IsSource AlterEgoAttrs where
  toSource = IdentitySource . alterEgoIdentityId

instance IsTarget AlterEgoAttrs where
  toTarget = IdentityTarget . alterEgoIdentityId

instance HasHandSize AlterEgoAttrs where
  handSize = alterEgoBaseHandSize

instance HasStartingHP AlterEgoAttrs where
  startingHP = alterEgoStartingHP

instance Entity AlterEgoAttrs where
  type EntityId AlterEgoAttrs = IdentityId
  type EntityAttrs AlterEgoAttrs = AlterEgoAttrs
  toId = alterEgoIdentityId
  toAttrs = id

instance RunMessage AlterEgoAttrs where
  runMessage msg a = case msg of
    IdentityMessage ident (SideMessage msg') | ident == alterEgoIdentityId a ->
      case msg' of
        Recovered -> do
          push
            (IdentityMessage ident
            $ IdentityHealed
            . unRec
            $ alterEgoBaseRecovery a
            )
          pure a
        _ -> pure a
    _ -> pure a
