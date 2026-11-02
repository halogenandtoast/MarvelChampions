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

newtype CivilWarMainScheme = CivilWarMainScheme (Attrs MainScheme `With` Maybe VillainId)
  deriving newtype (Show,Eq,ToJSON,HasCardCode,IsRef)

instance FromJSON CivilWarMainScheme where
  parseJSON = withObject "CivilWarMainScheme" $ \o ->
    CivilWarMainScheme <$> (With <$> parseJSON (Object o) <*> o .:? "_withMetadata")

civilWarMainSchemeDefinitions :: [CardDef]
civilWarMainSchemeDefinitions = filter ((`elem` implementedSchemes) . toCardCode) C.civilWarDefinitions
 where
  implementedSchemes = ["56063b","56172b","56121b","56199b","56122b","57044b","57005b","56200b","56124b","57045b","56142b","57006b","56064b","56201b"]

civilWarMainSchemes :: [MainSchemeCard CivilWarMainScheme]
civilWarMainSchemes = [mainSchemeWith (\a -> CivilWarMainScheme (a `With` Nothing)) def (PerPlayer $ if firstStage def then 7 else 9) (Static 0) (PerPlayer 1) (\a -> a {mainSchemeHeldCardsFacedown = toCardCode def == "56064b"}) | def <- civilWarMainSchemeDefinitions]

firstStage :: HasCardCode a => a -> Bool
firstStage card = toCardCode card `elem` (fst CW.registrationSchemes <> fst CW.resistanceSchemes)

instance IsMainScheme CivilWarMainScheme where
  toMainSchemeAttrs (CivilWarMainScheme (a `With` _)) = a
  mainSchemeModifiersFor _ target (CivilWarMainScheme (a `With` _))
    | isTarget a target,toCardCode a == "56064b" = pure [TargetThreatReduction $ fromIntegral $ length (mainSchemeHeldCards a) + length (mainSchemeHeldEncounterCards a)]
  mainSchemeModifiersFor _ target (CivilWarMainScheme (a `With` _)) = case target of
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
  mainSchemeAbilities (CivilWarMainScheme (a `With` leader))
    | toCardCode a `elem` ["56063b","56172b"] =
        [(windowAbility a 1 (W.EncounterCardRevealedBy W.When You (EncounterCardMatching $ CardWithType TreacheryType)) ForcedInterrupt NoCost (runAbility a 1)) {abilityLimit = EachPlayer $ PerPhase 1}]
    | toCardCode a `elem` ["56122b","57044b"] =
        [windowAbility a 2 W.AfterVillainThreatStep ForcedResponse NoCost (runAbility a 2)]
    | toCardCode a == "56064b" = [windowAbility a 3 (W.AllyDefeatedByAttack W.After AnyAlly) ForcedResponse NoCost (runAbility a 3)]
    | toCardCode a == "56201b" = [windowAbility a 4 (W.VillainStatusGained W.After $ VillainWithId vid) ForcedResponse NoCost (runAbility a 4) | Just vid <- [leader]]
    | otherwise = []

instance RunMessage CivilWarMainScheme where
  runMessage msg e@(CivilWarMainScheme (a `With` leader)) = runQueueT $ case msg of
    MainSchemeMessage sid MainSchemePlaceInitialThreat | sid == mainSchemeId a -> do
      host <- getActiveVillainIn =<< getAreaOfTarget a
      updated <- if firstStage a then do
        count <- getStartingPlayerCount
        pure a {mainSchemeThreat = if count > 1 then 2 * fromIntegral count else 0}
        else runMessage msg a
      pure $ CivilWarMainScheme (updated `With` host)
    CardSpecific target key _ | isTarget a target,toCardCode a == "56064b",key `elem` ["mgHoldCard","holdEncounterCard"] -> do
      updated <- runMessage msg a
      mainSchemeStep (mainSchemeId a) (MainSchemePlaceThreat 0)
      pure $ CivilWarMainScheme (updated `With` leader)
    RanAbility _ (isTarget a -> True) 1 _ _ -> e <$ placeThreatOn a (mainSchemeId a) 1
    RanAbility _ (isTarget a -> True) 2 _ _ -> do
      here <- getAreaOfTarget a
      players <- filterM (fmap (== here) . getAreaOfTarget) =<< getAllPlayers
      traverse_ (`discardTopOfDeck` 3) players
      pure e
    RanAbility _ (isTarget a -> True) 3 windows _ -> do
      for_ [aid | W.DefeatedAlly aid (Just _) <- windows] $ \aid -> do
        original <- getAllyPhysicalCard aid
        for_ original $ \case
          PlayerCard card -> placePlayerCardUnder (mainSchemeId a) card
          EncounterCard card -> placeEncounterCardUnderMainScheme (mainSchemeId a) card
      pure e
    RanAbility _ (isTarget a -> True) 4 _ _ -> e <$ placeThreatOn a (mainSchemeId a) 1
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
    _ -> (\updated -> CivilWarMainScheme (updated `With` leader)) <$> runMessage msg a
