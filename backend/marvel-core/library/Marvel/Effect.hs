module Marvel.Effect where

import Marvel.Prelude

import Data.Typeable
import Marvel.Ally.Allies
import Marvel.Card.Builder
import Marvel.Card.Code
import Marvel.Effect.Attrs
import Marvel.Entity
import Marvel.Event.Events
import Marvel.Id
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Source
import Marvel.Support.Supports
import Marvel.Upgrade.Upgrades
import Text.Show qualified

data Effect = forall a. IsEffect a => Effect a

instance Show Effect where
  show (Effect a) = show a

instance Eq Effect where
  (Effect (a :: a)) == (Effect (b :: b)) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

instance ToJSON Effect where
  toJSON (Effect a) = toJSON a

instance FromJSON Effect where
  parseJSON = withObject "Effect" $ \o -> do
    cCode <- o .: "cardCode"
    case lookup cCode allEffects of
      Nothing -> error $ "Invalid effect: " <> show cCode
      Just (SomeCardEffect (_ :: CardEffect a)) -> Effect <$> parseJSON @a (Object o)

instance RunMessage Effect where
  runMessage msg (Effect a) = Effect <$> runMessage msg a

instance Entity Effect where
  type EntityId Effect = EffectId
  type EntityAttrs Effect = EffectAttrs
  toId = toId . toAttrs
  toAttrs (Effect a) = toAttrs a

data SomeCardEffect = forall a. IsEffect a => SomeCardEffect (CardEffect a)

liftCardEffect :: (forall a . CardEffect a -> b) -> SomeCardEffect -> b
liftCardEffect f (SomeCardEffect a) = f a

someCardEffectCardCode :: SomeCardEffect -> CardCode
someCardEffectCardCode = liftCardEffect cbCardCode

allEffects :: HashMap CardCode SomeCardEffect
allEffects =
  fromList $
      [ ("01039", SomeCardEffect rocketBootsEffect)
      , ("01068", SomeCardEffect visionEffect)
      , ("01070", SomeCardEffect leadFromTheFrontEffect)
      , ("01092", SomeCardEffect helicarrierEffect)
      ]

lookupEffect :: CardCode -> Source -> EntityMatcher -> EffectId -> Effect
lookupEffect cardCode source entityMatcher effectId = case lookup cardCode allEffects of
  Just (SomeCardEffect a) -> Effect $ cbCardBuilder a (source, entityMatcher, effectId)
  Nothing -> error $ "Invalid card code for effect " <> show cardCode

instance HasModifiersFor Effect where
  getModifiersFor _ target e = do
    valid <- effectValidFor attrs target
    pure $ if valid then effectModifiers attrs else []
   where
    attrs = toAttrs e
