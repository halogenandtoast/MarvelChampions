module Marvel.Message.MainScheme where

import Marvel.Prelude
import Marvel.Ref

data MainSchemeMessage
  = MainSchemeThwarted Source Natural
  | MainSchemePlaceThreat Natural
  | MainSchemePlaceInitialThreat
  | RevealMainScheme
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
