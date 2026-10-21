module Marvel.Message.SideScheme where

import Marvel.Prelude

import Marvel.Ref

data SideSchemeMessage
  = RevealSideScheme
  | DefeatSideScheme
  | SideSchemePlaceInitialThreat
  | SideSchemePlaceThreat Natural
  | SideSchemeThwarted Source Natural
  | -- | what a scheme's When Revealed does once the cards it put into play are there
    SideSchemeRevealedThen
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
