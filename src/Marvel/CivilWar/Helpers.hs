module Marvel.CivilWar.Helpers where

import Marvel.Prelude
import Data.List (find)
import Marvel.Card
import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.Types qualified as CW
import Marvel.Game.Source
import Marvel.EncounterSet (EncounterSet)
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

-- | The defending leader's team resolves the boost card as "you". Its first
-- player remains the chooser even if their own board has already finished.
leaderAttacksLeader :: ReverseQueue m => VillainId -> VillainId -> m ()
leaderAttacksLeader attacker defender = leaderAttacksLeaderThen attacker defender (pure ())

leaderAttacksLeaderThen :: ReverseQueue m => VillainId -> VillainId -> m () -> m ()
leaderAttacksLeaderThen attacker defender afterward = do
  team <- leaderBelongsTo defender
  competition <- getCompetition
  case (competition,team) of
    (Just c,Just own) -> villainAttacksCharacterThen attacker (CW.teamFirstPlayer c own) (VillainCharacter defender) afterward
    _ -> pure ()

civilWarDef :: CardCode -> CardDef
civilWarDef code = fromMaybe (error $ "missing Civil War definition: " <> show code) $ find ((== code) . toCardCode) C.civilWarDefinitions

playersAtLeader :: HasGame m => VillainId -> m [IdentityId]
playersAtLeader vid = do
  board <- getAreaOfTarget vid
  filterM (fmap (== board) . getAreaOfTarget) =<< getAllPlayers

firstPlayerAtLeader :: HasGame m => VillainId -> m IdentityId
firstPlayerAtLeader vid = do
  competition <- getCompetition
  board <- getAreaOfTarget vid
  case competition of
    Just c -> pure $ CW.teamFirstPlayer c $ if board == CW.teamArea CW.Registration then CW.Registration else CW.Resistance
    Nothing -> fromMaybe <$> getActivePlayerId <*> (headMay <$> playersAtLeader vid)

leaderOfFamily :: HasGame m => EncounterSet -> m (Maybe VillainId)
leaderOfFamily family = do
  enemies <- concatMapM (selectList . EnemyIs)
    [def | def <- C.civilWarDefinitions, cdCardType def == VillainType, cdEncounterSet def == Just family]
  pure $ headMay [vid | EnemyVillainId vid <- enemies]

-- | Both faces keep the same attachment identity, host, and counters.
flipLeaderMassForm :: ReverseQueue m => EnemyId -> m ()
flipLeaderMassForm enemy = do
  forms <- concatMapM (\def -> selectList $ AttachmentMatches
    [AttachmentOnEnemy enemy,AttachmentIs def]) [C.cw57046a,C.cw57046b]
  traverse_ (\aid -> cardSpecific aid "cwFlipMass" ()) forms

-- | Reattaching an in-play Shield keeps the physical card and detaches its old host.
returnCaptainAmericaShield :: ReverseQueue m => IdentityId -> VillainId -> m ()
returnCaptainAmericaShield you vid = do
  shields <- selectList $ AttachmentIs C.cw56143
  case shields of
    [] -> searchEverywhereAndRevealEncounterCard you $ CardIs C.cw56143
    _ -> traverse_ (`attachTo` vid) shields

-- | The opponent searches only the receiving team's own encounter piles.
dealUnregisteredSuperForEnemyTeam :: (ReverseQueue m, IsRef source) => IdentityId -> source -> m ()
dealUnregisteredSuperForEnemyTeam you source = do
  deck <- asYou you getEncounterDeck
  discard <- asYou you getEncounterDiscard
  key <- getDeckKeyFor you
  let candidates = filter (cardMatch $ CardWithTitle "Unregistered Super") (deck <> discard)
  lookForEnemyTeam you source candidates
    [cardLabeled (toCardCode card) $ dealFoundEncounterCard you card | card <- candidates]
  fromDeck key shuffleEncounterDeck
