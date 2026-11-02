module Marvel.CivilWar.Helpers where

import Marvel.Prelude
import Marvel.Card
import Marvel.CivilWar.Types qualified as CW
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Question (Choice)
import Marvel.Ref

-- | The enemy leader is the villain on your board. Your own leader is on your
-- opponents' board and has no cooperative equivalent.
enemyLeaderFor :: HasGame m => IdentityId -> m (Maybe VillainId)
enemyLeaderFor you = getAreaOfTarget you >>= getActiveVillainIn

friendlyLeaderFor :: HasGame m => IdentityId -> m (Maybe VillainId)
friendlyLeaderFor you = do
  competitive <- getCompetition
  case competitive >>= \c -> (c,) <$> CW.playerTeam c you of
    Nothing -> pure Nothing
    Just (_,team) -> getActiveVillainIn $ CW.teamArea $ CW.otherTeam team

playersOnTeam :: HasGame m => CW.Team -> m [IdentityId]
playersOnTeam team = do
  competitive <- getCompetition
  players <- getAllPlayers
  pure $ case competitive of
    Nothing -> players
    Just c -> filter (`elem` CW.teamPlayers c team) players

enemyTeamPlayers :: HasGame m => IdentityId -> m [IdentityId]
enemyTeamPlayers you = do
  competitive <- getCompetition
  case competitive >>= \c -> CW.otherTeam <$> CW.playerTeam c you of
    Nothing -> getPlayers
    Just team -> playersOnTeam team

-- The first-player token still identifies the opponent making scenario choices
-- during the reply phase if their entire board has already finished.
enemyTeamChooser :: HasGame m => IdentityId -> m IdentityId
enemyTeamChooser you = do
  competitive <- getCompetition
  case competitive >>= \c -> (c,) . CW.otherTeam <$> CW.playerTeam c you of
    Nothing -> selectJust FirstPlayer
    Just (c,team) -> pure $ CW.teamFirstPlayer c team

chooseForEnemyTeam :: (ReverseQueue m, IsRef source) => IdentityId -> source -> [m Choice] -> m ()
chooseForEnemyTeam you source choices = unless (null choices) do
  chooser <- enemyTeamChooser you
  competitive <- getCompetition
  let prompt = if isJust competitive then "$label.cwEnemyTeamChooses" else "$label.cwCooperativeEnemyChoice"
  chooseOnePromptedAbout chooser prompt source choices

-- | A top-of-deck search belongs to the chooser. It is not a public reveal.
lookForEnemyTeam :: (ReverseQueue m, IsRef source) => IdentityId -> source -> [EncounterCard] -> [m Choice] -> m ()
lookForEnemyTeam you source cards choices = unless (null choices) do
  chooser <- enemyTeamChooser you
  lookingAt chooser (map EncounterCard cards) $ chooseForEnemyTeam you source choices

leaderBelongsTo :: HasGame m => VillainId -> m (Maybe CW.Team)
leaderBelongsTo vid = do
  competitive <- getCompetition
  area <- getAreaOfTarget vid
  pure $ competitive *> if area == CW.teamArea CW.Registration then Just CW.Resistance else Just CW.Registration

teamForBoard :: HasGame m => VillainId -> m (Maybe CW.Team)
teamForBoard vid = do
  competitive <- getCompetition
  area <- getAreaOfTarget vid
  pure $ competitive *> if area == CW.teamArea CW.Registration then Just CW.Registration else Just CW.Resistance
