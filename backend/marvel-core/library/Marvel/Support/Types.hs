module Marvel.Support.Types where

import Marvel.Prelude

import Data.Typeable
import Marvel.Ability.Types
import Marvel.Card.Builder
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Entity
import Marvel.Id hiding (SupportId)
import Marvel.Id as X (SupportId)
import Marvel.Message
import Marvel.Modifier
import Marvel.Queue
import Marvel.Source
import Marvel.Target
import Text.Show qualified

data Support = forall a . IsSupport a => Support a

instance Show Support where
  show (Support a) = show a

instance ToJSON Support where
  toJSON (Support a) = toJSON a

instance Eq Support where
  Support (a :: a) == Support (b :: b) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

data SomeSupportCard = forall a . IsSupport a => SomeSupportCard
  (SupportCard a)

liftSupportCard :: (forall a . SupportCard a -> b) -> SomeSupportCard -> b
liftSupportCard f (SomeSupportCard a) = f a

someSupportCardCode :: SomeSupportCard -> CardCode
someSupportCardCode = liftSupportCard cbCardCode

instance Entity Support where
  type Id Support = SupportId
  data Attrs Support = SupportAttrs
    { supportId :: SupportId
    , supportCardDef :: CardDef
    , supportController :: IdentityId
    , supportExhausted :: Bool
    , supportUses :: Natural
    , supportDiscardIfNoUses :: Bool
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)
  data Field Support :: Type -> Type where
    SupportId :: Field Support SupportId
    SupportCardDef :: Field Support CardDef
    SupportController :: Field Support IdentityId
    SupportExhausted :: Field Support Bool
    SupportUses :: Field Support Natural
    SupportDiscardIfNoUses :: Field Support Bool
  field fld s = let SupportAttrs {..} = toAttrs s in case fld of
    SupportId -> supportId
    SupportCardDef -> supportCardDef
    SupportController -> supportController
    SupportExhausted -> supportExhausted
    SupportUses -> supportUses
    SupportDiscardIfNoUses -> supportDiscardIfNoUses
  toId = supportId . toAttrs
  toAttrs (Support a) = toSupportAttrs a

instance RunMessage Support where
  runMessage msg (Support a) = Support <$> runMessage msg a

instance Exhaustable Support where
  isExhausted = supportExhausted . toAttrs

instance IsSource Support where
  toSource = SupportSource . toId

instance HasAbilities Support where
  getAbilities (Support a) = getAbilities a

instance HasModifiersFor Support where
  getModifiersFor source target (Support a) = getModifiersFor source target a

getSupportController :: Support -> IdentityId
getSupportController = supportController . toAttrs

getSupportUses :: Support -> Natural
getSupportUses = supportUses . toAttrs

class (Typeable a, Show a, Eq a, ToJSON a, FromJSON a, HasModifiersFor a, HasAbilities a, RunMessage a) => IsSupport a where
  toSupportAttrs :: a -> Attrs Support
  default toSupportAttrs :: Coercible a (Attrs Support) => a -> Attrs Support
  toSupportAttrs = coerce

type SupportCard a = CardBuilder (IdentityId, SupportId) a

exhaustedL :: Lens' (Attrs Support) Bool
exhaustedL = lens supportExhausted $ \m x -> m { supportExhausted = x }

usesL :: Lens' (Attrs Support) Natural
usesL = lens supportUses $ \m x -> m { supportUses = x }

discardIfNoUsesL :: Lens' (Attrs Support) Bool
discardIfNoUsesL =
  lens supportDiscardIfNoUses $ \m x -> m { supportDiscardIfNoUses = x }

instance HasCardCode (Attrs Support) where
  toCardCode = toCardCode . supportCardDef

supportWith
  :: (Attrs Support -> a)
  -> CardDef
  -> (Attrs Support -> Attrs Support)
  -> CardBuilder (IdentityId, SupportId) a
supportWith f cardDef g = support (f . g) cardDef

support
  :: (Attrs Support -> a) -> CardDef -> CardBuilder (IdentityId, SupportId) a
support f cardDef = CardBuilder
  { cbCardCode = cdCardCode cardDef
  , cbCardBuilder = \(ident, mid) -> f $ SupportAttrs
    { supportId = mid
    , supportCardDef = cardDef
    , supportController = ident
    , supportExhausted = False
    , supportUses = 0
    , supportDiscardIfNoUses = False
    }
  }
instance IsSource (Attrs Support) where
  toSource = SupportSource . supportId

instance IsTarget (Attrs Support) where
  toTarget = SupportTarget . supportId

instance RunMessage (Attrs Support) where
  runMessage msg a = case msg of
    SupportMessage ident msg' | ident == supportId a -> case msg' of
      ReadiedSupport -> do
        pure $ a & exhaustedL .~ False
      ExhaustedSupport -> do
        pure $ a & exhaustedL .~ True
      SpendSupportUse -> do
        when
          (supportUses a == 1 && supportDiscardIfNoUses a)
          (push $ RemoveFromPlay (toTarget a))
        pure $ a & usesL -~ 1
    _ -> pure a
