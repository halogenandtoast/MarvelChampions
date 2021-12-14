{-# LANGUAGE TemplateHaskell #-}

module Marvel.MainScheme where

import Marvel.Prelude

import Marvel.Card
import Marvel.Entity
import Marvel.Id
import Marvel.MainScheme.Attrs
import Marvel.MainScheme.MainSchemes
import Marvel.Message
import Marvel.Source
import Marvel.TH

$(buildEntity "MainScheme")

allMainSchemes :: HashMap CardCode (MainSchemeId -> MainScheme)
allMainSchemes =
  fromList $
    map
      (toCardCode &&& cbCardBuilder)
      $(buildEntityLookupList "MainScheme")

lookupMainScheme :: CardCode -> MainSchemeId -> Maybe MainScheme
lookupMainScheme cardCode mainSchemeId = lookup cardCode allMainSchemes <*> pure mainSchemeId

instance Entity MainScheme where
  type EntityId MainScheme = MainSchemeId
  type EntityAttrs MainScheme = MainSchemeAttrs
  toId = toId . toAttrs
  toAttrs = genericToAttrs

instance RunMessage MainScheme where
  runMessage = genericRunMessage

instance IsSource MainScheme where
  toSource = MainSchemeSource . toId

instance IsCard MainScheme where
  toCard = toCard . toAttrs

instance HasCardDef MainScheme where
  getCardDef = getCardDef . toAttrs

getMainSchemeThreat :: MainScheme -> Natural
getMainSchemeThreat = mainSchemeThreat . toAttrs
