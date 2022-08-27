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
  type Id AlterEgo = IdentityId
  data Attrs AlterEgo = AlterEgoAttrs
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
  data Field AlterEgo :: Type -> Type where
    AlterEgoIdentityId :: Field AlterEgo IdentityId
    AlterEgoBaseHandSize :: Field AlterEgo HandSize
    AlterEgoBaseRecovery :: Field AlterEgo Rec
    AlterEgoHeroForms :: Field AlterEgo [Side]
    AlterEgoStartingHP :: Field AlterEgo (HP GameValue)
    AlterEgoCardDef :: Field AlterEgo CardDef
    AlterEgoObligations :: Field AlterEgo [CardDef]
  toId = alterEgoIdentityId . toAttrs
  toAttrs (AlterEgo a) = toAlterEgoAttrs a
  field fld a =
    let AlterEgoAttrs {..} = toAttrs a
    in
      case fld of
        AlterEgoIdentityId -> alterEgoIdentityId
        AlterEgoBaseHandSize -> alterEgoBaseHandSize
        AlterEgoBaseRecovery -> alterEgoBaseRecovery
        AlterEgoHeroForms -> alterEgoHeroForms
        AlterEgoStartingHP -> alterEgoStartingHP
        AlterEgoCardDef -> alterEgoCardDef
        AlterEgoObligations -> alterEgoObligations

instance HasModifiersFor AlterEgo where
  getModifiersFor source target (AlterEgo a) = getModifiersFor source target a

alterEgo
  :: (Attrs AlterEgo -> a)
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

class (Typeable a, Show a, Eq a, ToJSON a, FromJSON a, HasModifiersFor a, HasAbilities a, RunMessage a, IsSource a) => IsAlterEgo a where
  toAlterEgoAttrs :: a -> Attrs AlterEgo
  default toAlterEgoAttrs :: Coercible a (Attrs AlterEgo) => a -> Attrs AlterEgo
  toAlterEgoAttrs = coerce

type AlterEgoCard a = CardBuilder IdentityId a

instance HasCardCode (Attrs AlterEgo) where
  toCardCode = toCardCode . alterEgoCardDef

instance HasCardDef (Attrs AlterEgo) where
  getCardDef = alterEgoCardDef

instance IsSource (Attrs AlterEgo) where
  toSource = IdentitySource . alterEgoIdentityId

instance IsTarget (Attrs AlterEgo) where
  toTarget = IdentityTarget . alterEgoIdentityId

instance HasHandSize (Attrs AlterEgo) where
  handSize = alterEgoBaseHandSize

instance HasStartingHP (Attrs AlterEgo) where
  startingHP = alterEgoStartingHP

instance RunMessage (Attrs AlterEgo) where
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
