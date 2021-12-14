{-# LANGUAGE TemplateHaskell #-}

module Marvel.SideScheme where

import Marvel.Prelude

import Marvel.Card
import Marvel.Entity
import Marvel.Id
import Marvel.Message
import Marvel.Modifier
import Marvel.SideScheme.Attrs
import Marvel.SideScheme.SideSchemes
import Marvel.Source
import Marvel.TH
import Marvel.Target

$(buildEntity "SideScheme")

allSideSchemes :: HashMap CardCode (SideSchemeId -> SideScheme)
allSideSchemes =
  fromList $
    map
      (toCardCode &&& cbCardBuilder)
      $(buildEntityLookupList "SideScheme")

lookupSideScheme :: CardCode -> (SideSchemeId -> SideScheme)
lookupSideScheme cardCode = case lookup cardCode allSideSchemes of
  Just f -> f
  Nothing -> error $ "Invalid card code for side scheme " <> show cardCode

instance Entity SideScheme where
  type EntityId SideScheme = SideSchemeId
  type EntityAttrs SideScheme = SideSchemeAttrs
  toId = toId . toAttrs
  toAttrs = genericToAttrs

instance RunMessage SideScheme where
  runMessage = genericRunMessage

instance HasModifiersFor SideScheme where
  getModifiersFor = genericGetModifiersFor

instance IsSource SideScheme where
  toSource = SideSchemeSource . toId

instance IsTarget SideScheme where
  toTarget = SideSchemeTarget . toId

instance IsCard SideScheme where
  toCard = toCard . toAttrs

instance HasCardDef SideScheme where
  getCardDef = getCardDef . toAttrs

isCrisis :: SideScheme -> Bool
isCrisis = sideSchemeCrisis . toAttrs

getSideSchemeThreat :: SideScheme -> Natural
getSideSchemeThreat = sideSchemeThreat . toAttrs
