module Marvel.Message.Effect where

import Marvel.Prelude

import Marvel.Id

data EffectMessage
  = DisableEffect
  | UsedEffect IdentityId
  | EffectChoice Natural
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
