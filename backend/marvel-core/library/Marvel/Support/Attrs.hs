{-# LANGUAGE TemplateHaskell #-}
module Marvel.Support.Attrs where

import Marvel.Prelude

import Marvel.Card.Builder
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Entity
import Marvel.Id
import Marvel.Message
import Marvel.Source
import Marvel.Target

class IsSupport a

type SupportCard a = CardBuilder (IdentityId, SupportId) a

data SupportAttrs = SupportAttrs
  { supportId :: SupportId
  , supportCardDef :: CardDef
  , supportController :: IdentityId
  , supportExhausted :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

makeLensesWith suffixedFields ''SupportAttrs

instance HasCardCode SupportAttrs where
  toCardCode = toCardCode . supportCardDef

support
  :: (SupportAttrs -> a)
  -> CardDef
  -> CardBuilder (IdentityId, SupportId) a
support f cardDef =
  CardBuilder
    { cbCardCode = cdCardCode cardDef
    , cbCardBuilder = \(ident, mid) -> f $ SupportAttrs
      { supportId = mid
      , supportCardDef = cardDef
      , supportController = ident
      , supportExhausted = False
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

isTarget :: (Entity a, EntityAttrs a ~ SupportAttrs) => a -> Target -> Bool
isTarget a = (== toTarget (toAttrs a))

instance RunMessage SupportAttrs where
  runMessage msg a = case msg of
    SupportMessage ident msg' | ident == supportId a -> case msg' of
      ReadiedSupport -> do
        pure $ a & exhaustedL .~ False
      ExhaustedSupport -> do
        pure $ a & exhaustedL .~ True
    _ -> pure a
