module Marvel.CivilWar.MainSchemes where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Attack (attackDefended)
import Marvel.Card
import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.Helpers (lookForEnemyTeam,civilWarDef)
import Marvel.CivilWar.Types qualified as CW
import Marvel.Cost
import Marvel.Damage (DamageSource (FromAbility),toDamage)
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Id
import Marvel.Keyword qualified as K
import Marvel.MainScheme.Types
import Marvel.Name (title)
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query (selectJust,selectList)
import Marvel.Question (Choice (CardLabel),runAbility)
import Marvel.Ref
import Marvel.Window qualified as W
import Marvel.Trait qualified as T

newtype CivilWarMainScheme = CivilWarMainScheme (Attrs MainScheme `With` CivilWarMainMeta)
  deriving newtype (Show,Eq,ToJSON,HasCardCode,IsRef)

data CivilWarMainMeta = CivilWarMainMeta
  { cwMainLeader :: Maybe VillainId
  , cwUnregisteredCards :: [(CardId,[IdentityId])]
  }
  deriving stock (Show,Eq,Generic)
  deriving anyclass ToJSON

emptyCivilWarMainMeta :: CivilWarMainMeta
emptyCivilWarMainMeta = CivilWarMainMeta Nothing []

instance FromJSON CivilWarMainMeta where
  parseJSON = withObject "CivilWarMainMeta" $ \o -> do
    leader <- o .:? "cwMainLeader"
    legacy <- o .:? "_withMetadata"
    CivilWarMainMeta (leader <|> legacy) <$> o .:? "cwUnregisteredCards" .!= []

instance FromJSON CivilWarMainScheme where
  parseJSON = withObject "CivilWarMainScheme" $ \o ->
    CivilWarMainScheme <$> (With <$> parseJSON (Object o) <*> parseJSON (Object o))

civilWarMainSchemeDefinitions :: [CardDef]
civilWarMainSchemeDefinitions = filter ((`elem` (implementedSchemes <> map flipCardCode implementedSchemes)) . toCardCode) C.civilWarDefinitions
 where
  implementedSchemes = ["56063b","56172b","56121b","56199b","56122b","57044b","57005b","56200b","56124b","57045b","56142b","57006b","56064b","56201b","56096b","56141b","56123b","56173b","56202b","56097b"]

civilWarMainSchemes :: [MainSchemeCard CivilWarMainScheme]
civilWarMainSchemes = [mainSchemeWith (\a -> CivilWarMainScheme (a `With` emptyCivilWarMainMeta)) def (PerPlayer $ if firstStage def then 7 else 9) (Static 0) (PerPlayer 1) (\a -> a {mainSchemeHeldCardsFacedown = toCardCode def `elem` ["56064a","56064b"]}) | def <- civilWarMainSchemeDefinitions]

firstStage :: HasCardCode a => a -> Bool
firstStage card = any (`elem` (fst CW.registrationSchemes <> fst CW.resistanceSchemes)) [toCardCode card,flipCardCode $ toCardCode card]

instance IsMainScheme CivilWarMainScheme where
  toMainSchemeAttrs (CivilWarMainScheme (a `With` _)) = a
  mainSchemeModifiersFor _ target (CivilWarMainScheme (a `With` _))
    | isTarget a target,toCardCode a == "56064b" = pure [TargetThreatReduction $ fromIntegral $ length (mainSchemeHeldCards a) + length (mainSchemeHeldEncounterCards a)]
  mainSchemeModifiersFor _ target (CivilWarMainScheme (a `With` _)) = case target of
    MinionRef _ | code `elem` ["56121b","56199b"] -> pure [KeywordModifier K.Guard]
    IdentityRef you | code == "56097b" -> do
      hero <- identityMatches (IdentityWithId you <> HeroIdentity) you
      pure [TraitModifier T.Unregistered | hero]
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
  mainSchemeAbilities (CivilWarMainScheme (a `With` meta))
    | toCardCode a `elem` ["56063b","56172b"] =
        [(windowAbility a 1 (W.EncounterCardRevealedBy W.When You (EncounterCardMatching $ CardWithType TreacheryType)) ForcedInterrupt NoCost (runAbility a 1)) {abilityLimit = EachPlayer $ PerPhase 1}]
    | toCardCode a `elem` ["56122b","57044b"] =
        [windowAbility a 2 W.AfterVillainThreatStep ForcedResponse NoCost (runAbility a 2)]
    | toCardCode a == "56064b" = [windowAbility a 3 (W.AllyDefeatedByAttack W.After AnyAlly) ForcedResponse NoCost (runAbility a 3)]
    | toCardCode a == "56201b" = [windowAbility a 4 (W.VillainStatusGained W.After $ VillainWithId vid) ForcedResponse NoCost (runAbility a 4) | Just vid <- [cwMainLeader meta]]
    | toCardCode a == "56202b" = [windowAbility a 5 W.AfterVillainThreatStep ForcedResponse NoCost (runAbility a 5)]
    | toCardCode a == "56097b" =
        [windowAbility a 6 (W.EncounterCardRevealedBy W.After AnyIdentity $ EncounterCardWithId cid) ForcedResponse NoCost (runAbility a 6)
        | (cid,heroes) <- cwUnregisteredCards meta,not (null heroes)]
    | otherwise = []

instance RunMessage CivilWarMainScheme where
  runMessage msg e@(CivilWarMainScheme (a `With` meta)) = runQueueT $ case msg of
    SetupMainScheme | cdCardCode (getCardDef a) /= gameplayCode a -> do
      when (firstStage a) $ do
        first <- firstPlayerAtScheme a
        key <- getDeckKeyFor first
        pile <- getPile key
        aside <- getSetAsideEncounterCardsForDeck key
        live <- asYou first $ selectList AnySideScheme
        inPlay <- catMaybes <$> for live (\sid -> getsGame $ \g -> case cardInPlay g (SideSchemeRef sid) of
          Just (EncounterCard card) -> Just card
          _ -> Nothing)
        competition <- getCompetition
        leader <- getActiveVillainIn =<< getAreaOfTarget a
        family <- case leader of
          Nothing -> pure Nothing
          Just vid -> getsGame $ \g -> cdEncounterSet =<< abilityCardDef g (VillainRef vid)
        let eligible card = cdCardType (getCardDef card) == SideSchemeType &&
              if isJust competition then title (cdName $ getCardDef card) == "Choosing Sides"
              else isJust family && cdEncounterSet (getCardDef card) == family
            choices = filter eligible (pileDeck pile <> pileDiscard pile <> aside <> inPlay)
        lookForEnemyTeam first a choices
          [cardLabeled (toCardCode card) $ revealEncounterCard first card | card <- choices]
        fromDeck key shuffleEncounterDeck
      cardSpecific a "cwFlipSetup" ()
      pure e
    CardSpecific (isTarget a -> True) "cwFlipSetup" _ ->
      pure $ CivilWarMainScheme (a {mainSchemeCardDef = civilWarDef $ gameplayCode a} `With` meta)
    CardSpecific GameRef "cwRevealedUnregistered" (fromJSON -> Success (cid,heroes :: [IdentityId])) | toCardCode a == "56097b" -> do
      here <- getAreaOfTarget a
      affected <- filterM (fmap (== here) . getAreaOfTarget) heroes
      pure $ CivilWarMainScheme (a `With` meta {cwUnregisteredCards = [(cid,affected) | not (null affected)] <> filter ((/= cid) . fst) (cwUnregisteredCards meta)})
    RanAbility _ (isTarget a -> True) 6 windows _ -> do
      let cards = [ecCardId card | W.EncounterCardResolved _ card <- windows]
          affected = concat [heroes | (cid,heroes) <- cwUnregisteredCards meta,cid `elem` cards]
      traverse_ (\you -> damageIdentity you a $ toDamage 2 FromAbility) affected
      pure $ CivilWarMainScheme (a `With` meta {cwUnregisteredCards = filter ((`notElem` cards) . fst) (cwUnregisteredCards meta)})
    MainSchemeMessage sid MainSchemePlaceInitialThreat | sid == mainSchemeId a -> do
      host <- getActiveVillainIn =<< getAreaOfTarget a
      updated <- if firstStage a then do
        count <- getStartingPlayerCount
        pure a {mainSchemeThreat = if count > 1 then 2 * fromIntegral count else 0}
        else runMessage msg a
      pure $ CivilWarMainScheme (updated `With` (meta {cwMainLeader = host}))
    CardSpecific target key _ | isTarget a target,toCardCode a == "56064b",key `elem` ["mgHoldCard","holdEncounterCard"] -> do
      updated <- runMessage msg a
      mainSchemeStep (mainSchemeId a) (MainSchemePlaceThreat 0)
      pure $ CivilWarMainScheme (updated `With` meta)
    MainSchemeMessage sid RevealMainScheme | sid == mainSchemeId a,toCardCode a `elem` ["56096b","56141b","56123b","56173b"] -> do
      first <- firstPlayerAtScheme a
      key <- getDeckKeyFor first
      players <- playersAtScheme a
      pile <- getPile key
      let sideSchemes = toCardCode a `elem` ["56123b","56173b"]
          pool = pileDeck pile <> [card | sideSchemes,card <- pileDiscard pile]
          kind = if sideSchemes then SideSchemeType else MinionType
          eligible = filter ((== kind) . cdCardType . getCardDef) pool
      cardSpecific a "cwSearchCards" (key,first,players,eligible,[] :: [(IdentityId,EncounterCard)])
      pure e
    CardSpecific target "cwSearchCards" (fromJSON -> Success (key :: DeckKey,first :: IdentityId,players :: [IdentityId],eligible :: [EncounterCard],found :: [(IdentityId,EncounterCard)])) | isTarget a target -> do
      case players of
        recipient : remaining | not (null eligible) ->
          lookForEnemyTeam first recipient eligible
            [cardLabeled (toCardCode card) $ cardSpecific a "cwSearchCards" (key,first,remaining,filter ((/= ecCardId card) . ecCardId) eligible,found <> [(recipient,card)]) | card <- eligible]
        _ -> fromDeck key do
          for_ found $ \(recipient,card) -> dealFoundEncounterCard recipient card
          shuffleEncounterDeck
      pure e
    RanAbility _ (isTarget a -> True) 5 _ _ -> do
      first <- firstPlayerAtScheme a
      key <- getDeckKeyFor first
      players <- playersAtScheme a
      cardSpecific a "cwJailbreakDiscard" (key,first,players,[] :: [(IdentityId,EncounterCard)])
      pure e
    CardSpecific target "cwJailbreakDiscard" (fromJSON -> Success (key :: DeckKey,first :: IdentityId,players :: [IdentityId],found :: [(IdentityId,EncounterCard)])) | isTarget a target -> do
      living <- getAllPlayers
      case filter (`elem` living) players of
        [] -> cardSpecific a "cwJailbreakPut" (first,found)
        [recipient] -> cardSpecific a "cwJailbreakOne" (key,first,recipient,[] :: [IdentityId],found)
        remaining -> do
          chooser <- firstPlayerAtScheme a
          chooseOnePromptedAbout chooser "$label.cwChooseDiscardPlayer" a
            [targeting recipient $ cardSpecific a "cwJailbreakOne" (key,first,recipient,filter (/= recipient) remaining,found) | recipient <- remaining]
      pure e
    CardSpecific target "cwJailbreakOne" (fromJSON -> Success (key :: DeckKey,first :: IdentityId,recipient :: IdentityId,remaining :: [IdentityId],found :: [(IdentityId,EncounterCard)])) | isTarget a target -> do
      cards <- take 1 . pileDeck <$> getPile key
      setActivePlayer recipient
      fromDeck key $ discardTopOfEncounterDeck 1
      let minions = [(recipient,card) | card <- cards,cdCardType (getCardDef card) == MinionType]
      cardSpecific a "cwJailbreakDiscard" (key,first,remaining,found <> minions)
      pure e
    CardSpecific target "cwJailbreakPut" (fromJSON -> Success (first :: IdentityId,found :: [(IdentityId,EncounterCard)])) | isTarget a target -> do
      living <- getAllPlayers
      case filter ((`elem` living) . fst) found of
        [] -> setActivePlayer first
        [(you,card)] -> putEncounterCardIntoPlay you card >> setActivePlayer first
        remaining -> do
          chooser <- firstPlayerAtScheme a
          chooseOnePromptedAbout chooser "$label.cwChooseMinionRecipient" a
            [fmap (CardLabel $ toCardCode card) $ targeting you $ do
              putEncounterCardIntoPlay you card
              cardSpecific a "cwJailbreakPut" (first,filter ((/= you) . fst) remaining)
            | (you,card) <- remaining]
      pure e
    RanAbility _ (isTarget a -> True) 1 _ _ -> e <$ placeThreatOn a (mainSchemeId a) 1
    RanAbility _ (isTarget a -> True) 2 _ _ -> do
      cardSpecific a "cwPlayerDeckDiscards" =<< playersAtScheme a
      pure e
    CardSpecific target "cwPlayerDeckDiscards" (fromJSON -> Success (players :: [IdentityId])) | isTarget a target -> do
      living <- getAllPlayers
      case filter (`elem` living) players of
        [] -> pure ()
        [you] -> discardTopOfDeck you 3
        remaining -> do
          first <- firstPlayerAtScheme a
          chooseOnePromptedAbout first "$label.cwChooseDiscardPlayer" a
            [targeting you $ do
              discardTopOfDeck you 3
              cardSpecific a "cwPlayerDeckDiscards" (filter (/= you) remaining)
            | you <- remaining]
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
    _ -> (\updated -> CivilWarMainScheme (updated `With` meta)) <$> runMessage msg a

playersAtScheme :: HasGame m => Attrs MainScheme -> m [IdentityId]
playersAtScheme a = do
  here <- getAreaOfTarget a
  filterM (fmap (== here) . getAreaOfTarget) =<< getAllPlayers

firstPlayerAtScheme :: HasGame m => Attrs MainScheme -> m IdentityId
firstPlayerAtScheme a = do
  competition <- getCompetition
  here <- getAreaOfTarget a
  case competition of
    Just c -> pure $ CW.teamFirstPlayer c $ if here == CW.teamArea CW.Registration then CW.Registration else CW.Resistance
    Nothing -> selectJust FirstPlayer


gameplayCode :: HasCardCode a => a -> CardCode
gameplayCode card = if code `elem` (fst CW.registrationSchemes <> snd CW.registrationSchemes <> fst CW.resistanceSchemes <> snd CW.resistanceSchemes) then code else flipCardCode code
 where code = toCardCode card
