module Marvel.Support.Types where

import Marvel.Prelude

import Data.Typeable
import Marvel.Ability.Type
import Marvel.Card.Builder
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Entity
import Marvel.Id
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
  type EntityId Support = SupportId
  type EntityAttrs Support = SupportAttrs
  toId = toId . toAttrs
  toAttrs (Support a) = toAttrs a

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

class (Typeable a, Show a, Eq a, ToJSON a, FromJSON a, Entity a, EntityAttrs a ~ SupportAttrs, EntityId a ~ SupportId, HasModifiersFor a, HasAbilities a, RunMessage a) => IsSupport a

type SupportCard a = CardBuilder (IdentityId, SupportId) a

data SupportAttrs = SupportAttrs
  { supportId :: SupportId
  , supportCardDef :: CardDef
  , supportController :: IdentityId
  , supportExhausted :: Bool
  , supportUses :: Natural
  , supportDiscardIfNoUses :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

exhaustedL :: Lens' SupportAttrs Bool
exhaustedL = lens supportExhausted $ \m x -> m { supportExhausted = x }

usesL :: Lens' SupportAttrs Natural
usesL = lens supportUses $ \m x -> m { supportUses = x }

discardIfNoUsesL :: Lens' SupportAttrs Bool
discardIfNoUsesL =
  lens supportDiscardIfNoUses $ \m x -> m { supportDiscardIfNoUses = x }

instance HasCardCode SupportAttrs where
  toCardCode = toCardCode . supportCardDef

supportWith
  :: (SupportAttrs -> a)
  -> CardDef
  -> (SupportAttrs -> SupportAttrs)
  -> CardBuilder (IdentityId, SupportId) a
supportWith f cardDef g = support (f . g) cardDef

support
  :: (SupportAttrs -> a) -> CardDef -> CardBuilder (IdentityId, SupportId) a
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

instance Entity SupportAttrs where
  type EntityId SupportAttrs = SupportId
  type EntityAttrs SupportAttrs = SupportAttrs
  toId = supportId
  toAttrs = id

instance IsSource SupportAttrs where
  toSource = SupportSource . toId

instance IsTarget SupportAttrs where
  toTarget = SupportTarget . toId

instance RunMessage SupportAttrs where
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
