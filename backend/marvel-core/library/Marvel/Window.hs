module Marvel.Window where

import Marvel.Prelude

import Marvel.Id
import Marvel.Source

data WindowTiming = After
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype WindowMatcher = PlayThis WindowTiming
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Window = Window
  { windowTiming :: WindowTiming
  , windowType :: WindowType
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype WindowType = PlayedAlly AllyId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

windowMatches :: WindowMatcher -> Window -> Source -> Bool
windowMatches matcher w source = case matcher of
  PlayThis timing -> case windowType w of
    PlayedAlly allyId ->
      timing == windowTiming w && source == AllySource allyId
