module Marvel.Message.SideScheme where

import Marvel.Prelude

import Marvel.Ref

data SideSchemeMessage
  = RevealSideScheme
  | DefeatSideScheme
  | SideSchemePlaceInitialThreat
  | SideSchemePlaceThreat Natural
  | SideSchemeThwarted Source Natural
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
