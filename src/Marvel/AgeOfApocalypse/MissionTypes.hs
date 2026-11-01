module Marvel.AgeOfApocalypse.MissionTypes where

import Marvel.Prelude
import Marvel.Card.Id (CardId)
import Marvel.Card.Code
import Marvel.Card.PlayerCard.Types
import Marvel.Id

-- | The mission is in play, but separate from all players' areas. Keep the
-- physical entities here so saving, undoing and replaying preserve the attempt.
data MissionState = MissionState
  { missionCard :: CardCode
  , missionScheme :: SideSchemeId
  , missionOverseer :: CardCode
  , missionAllies :: [AllyId]
  , missionMinions :: [MinionId]
  , missionAttempts :: Natural
  , missionSettingUp :: Bool
  , missionFinished :: Bool
  , missionSucceeded :: Bool
  , missionDiscount :: Bool
  , missionPendingAlly :: Maybe CardId
  , missionDiscarded :: [PlayerCard]
  , missionParticipants :: [AllyId]
  , missionActor :: Maybe IdentityId
  , missionAssignments :: [(AllyId, PlayerCard)]
  , missionDamagePool :: Natural
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newMissionState :: CardCode -> SideSchemeId -> CardCode -> MissionState
newMissionState code sid overseer = MissionState code sid overseer [] [] 0 True False False False Nothing [] [] Nothing [] 0
