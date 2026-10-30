module Marvel.Message.MainScheme where

import Marvel.Prelude
import Marvel.Ref

data MainSchemeMessage
  = MainSchemeThwarted Source Natural
  | MainSchemeBasicThwarted Source Natural
  | -- | threat removed, not by thwarting
    MainSchemeThreatRemoved Source Natural
  | MainSchemePlaceThreat Natural
  | MainSchemePlaceInitialThreat
  | RevealMainScheme
  | SetMainSchemeGlider Bool
  | SetMainSchemeAcceleration Natural
  | AddMainSchemeAcceleration Natural
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
