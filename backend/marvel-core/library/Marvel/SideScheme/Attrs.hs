{-# LANGUAGE TemplateHaskell #-}
module Marvel.SideScheme.Attrs where

import Marvel.Prelude

import Marvel.Card.Builder
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Entity
import Marvel.GameValue
import Marvel.Id
import Marvel.Queue
import Marvel.Message
import Marvel.Source
import Marvel.Target

class IsSideScheme a

type SideSchemeCard a = CardBuilder SideSchemeId a

data SideSchemeAttrs = SideSchemeAttrs
  { sideSchemeId :: SideSchemeId
  , sideSchemeCardDef :: CardDef
  , sideSchemeThreat :: Natural
  , sideSchemeInitialThreat :: GameValue
  , sideSchemeAcceleration :: GameValue
  , sideSchemeCrisis :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

makeLensesWith (suffixedWithFields "sideScheme") ''SideSchemeAttrs

instance HasCardCode SideSchemeAttrs where
  toCardCode = toCardCode . sideSchemeCardDef

sideSchemeWith
  :: (SideSchemeAttrs -> a)
  -> CardDef
  -> GameValue
  -> GameValue
  -> (SideSchemeAttrs -> SideSchemeAttrs)
  -> CardBuilder SideSchemeId a
sideSchemeWith f cardDef initialThreat acceleration g =
  sideScheme (f . g) cardDef initialThreat acceleration

sideScheme
  :: (SideSchemeAttrs -> a)
  -> CardDef
  -> GameValue
  -> GameValue
  -> CardBuilder SideSchemeId a
sideScheme f cardDef initialThreat acceleration = CardBuilder
  { cbCardCode = cdCardCode cardDef
  , cbCardBuilder = \mid -> f $ SideSchemeAttrs
    { sideSchemeId = mid
    , sideSchemeCardDef = cardDef
    , sideSchemeInitialThreat = initialThreat
    , sideSchemeAcceleration = acceleration
    , sideSchemeThreat = 0
    , sideSchemeCrisis = False
    }
  }

instance Entity SideSchemeAttrs where
  type EntityId SideSchemeAttrs = SideSchemeId
  type EntityAttrs SideSchemeAttrs = SideSchemeAttrs
  toId = sideSchemeId
  toAttrs = id

instance IsSource SideSchemeAttrs where
  toSource = SideSchemeSource . toId

instance IsTarget SideSchemeAttrs where
  toTarget = SideSchemeTarget . toId

instance HasCardDef SideSchemeAttrs where
  getCardDef = sideSchemeCardDef

isTarget :: (Entity a, EntityAttrs a ~ SideSchemeAttrs) => a -> Target -> Bool
isTarget a = (== toTarget (toAttrs a))

instance RunMessage SideSchemeAttrs where
  runMessage msg attrs = case msg of
    SideSchemeMessage sideSchemeId msg' | sideSchemeId == toId attrs ->
      case msg' of
        SideSchemePlaceInitialThreat -> do
          n <- fromIntegral <$> fromGameValue (sideSchemeInitialThreat attrs)
          pure $ attrs & threatL .~ n
        SideSchemePlaceThreat n -> pure $ attrs & threatL +~ n
        SideSchemeThwarted _ n -> do
          when (subtractNatural n (sideSchemeThreat attrs) == 0) (push $ RemoveFromPlay (toTarget attrs))
          pure $ attrs & threatL %~ subtractNatural n
        _ -> pure attrs
    _ -> pure attrs
