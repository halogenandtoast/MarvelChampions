module Marvel.CivilWar.MainSchemes where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Attack (attackDefended)
import Marvel.Card
import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.Types qualified as CW
import Marvel.Cost
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Id
import Marvel.Keyword qualified as K
import Marvel.MainScheme.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.Window qualified as W

newtype CivilWarMainScheme = CivilWarMainScheme (Attrs MainScheme)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

civilWarMainSchemeDefinitions :: [CardDef]
civilWarMainSchemeDefinitions = filter ((`elem` implementedSchemes) . toCardCode) C.civilWarDefinitions
 where
  implementedSchemes = ["56063b","56172b","56121b","56199b","56122b","57044b","57005b","56200b","56124b","57045b","56142b","57006b"]

civilWarMainSchemes :: [MainSchemeCard CivilWarMainScheme]
civilWarMainSchemes = [mainScheme CivilWarMainScheme def (PerPlayer $ if firstStage def then 7 else 9) (Static 0) (PerPlayer 1) | def <- civilWarMainSchemeDefinitions]

firstStage :: HasCardCode a => a -> Bool
firstStage card = toCardCode card `elem` (fst CW.registrationSchemes <> fst CW.resistanceSchemes)

instance IsMainScheme CivilWarMainScheme where
  mainSchemeModifiersFor _ target (CivilWarMainScheme a) = case target of
    MinionRef _ | code `elem` ["56121b","56199b"] -> pure [KeywordModifier K.Guard]
    IdentityRef _ | code `elem` ["56124b","57045b"] -> pure [CardsEnterPlayExhausted $ CardWithType AllyType]
    VillainRef vid -> do
      leader <- getActiveVillainIn =<< getAreaOfTarget a
      if leader /= Just vid then pure [] else case code of
        _ | code `elem` ["57005b","56200b"] -> pure [KeywordModifier K.Steady]
        _ | code `elem` ["56142b","57006b"] -> do
          attack <- getEnemyAttack (EnemyVillainId vid)
          pure [AttackModifier $ if attackDefended current then 1 else 2 | Just current <- [attack]]
        _ -> pure []
    _ -> pure []
   where code = toCardCode a
  mainSchemeAbilities (CivilWarMainScheme a)
    | toCardCode a `elem` ["56063b","56172b"] =
        [(windowAbility a 1 (W.EncounterCardRevealedBy W.When You (EncounterCardMatching $ CardWithType TreacheryType)) ForcedInterrupt NoCost (runAbility a 1)) {abilityLimit = EachPlayer $ PerPhase 1}]
    | toCardCode a `elem` ["56122b","57044b"] =
        [windowAbility a 2 W.AfterVillainThreatStep ForcedResponse NoCost (runAbility a 2)]
    | otherwise = []

instance RunMessage CivilWarMainScheme where
  runMessage msg e@(CivilWarMainScheme a) = runQueueT $ case msg of
    MainSchemeMessage sid MainSchemePlaceInitialThreat | sid == mainSchemeId a,firstStage a -> do
      count <- getStartingPlayerCount
      pure $ CivilWarMainScheme a {mainSchemeThreat = if count > 1 then 2 * fromIntegral count else 0}
    RanAbility _ (isTarget a -> True) 1 _ _ -> e <$ placeThreatOn a (mainSchemeId a) 1
    RanAbility _ (isTarget a -> True) 2 _ _ -> do
      here <- getAreaOfTarget a
      players <- filterM (fmap (== here) . getAreaOfTarget) =<< getAllPlayers
      traverse_ (`discardTopOfDeck` 3) players
      pure e
    AdvanceMainScheme sid | sid == mainSchemeId a -> do
      competition <- getCompetition
      case competition of
        Just c -> do
          here <- getAreaOfTarget a
          let team = if here == CW.teamArea CW.Registration then CW.Registration else CW.Resistance
              following = drop 1 $ CW.teamScenarioMainSchemes $ CW.scenarioForTeam (CW.competitiveSetup c) (CW.otherTeam team)
          if firstStage a then traverse_ (replaceMainScheme sid) (headMay following)
          else cardSpecific GameRef "cwFinishBoard" (team,CW.MainSchemeCompleted)
        Nothing -> if firstStage a then nextMainScheme sid else gameOver Lost
      pure e
    _ -> CivilWarMainScheme <$> runMessage msg a
