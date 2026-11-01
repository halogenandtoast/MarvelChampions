module Marvel.Ally.Allies.MissionAlly (MissionAlly (..)) where

import Marvel.Prelude
import Data.Aeson.KeyMap qualified as KeyMap
import Marvel.Ability (HasAbilities)
import Marvel.Ally.Runner

-- | Printed ally text is blank at a mission. Preserve its physical definition,
-- owner, upgrades, damage and statuses, while running only common ally plumbing.
newtype MissionAlly = MissionAlly (Attrs Ally)
  deriving anyclass (IsAlly, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, FromJSON, HasCardCode, IsRef)

instance ToJSON MissionAlly where
  toJSON (MissionAlly attrs) = case toJSON attrs of
    Object o -> Object (KeyMap.insert "missionAlly" (Bool True) o)
    other -> other

instance RunMessage MissionAlly where
  runMessage msg (MissionAlly attrs) = MissionAlly <$> runMessage msg attrs
