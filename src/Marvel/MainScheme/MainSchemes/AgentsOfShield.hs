{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.MainScheme.MainSchemes.AgentsOfShield where

import Marvel.Prelude
import Marvel.Question (runAbility)
import Marvel.Ability
import Marvel.AgentsOfShield.Board
import Marvel.AgentsOfShield.EncounterCards qualified as Cards
import Marvel.AgentsOfShield.Helpers
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Difficulty
import Marvel.Entity
import Marvel.EncounterCard (genEncounterCard)
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Id
import Marvel.Keyword
import Marvel.MainScheme.MainSchemes.EscapeTheMuseum (escapeStage)
import Marvel.MainScheme.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Phase
import Marvel.Query
import Marvel.Ref
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W

newtype AosMainScheme = AosMainScheme (Attrs MainScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

aos50067b :: MainSchemeCard AosMainScheme
aos50067b = mainScheme AosMainScheme Cards.aos50067b (PerPlayer 10) (PerPlayer 2) (PerPlayer 0)

aos50087b :: MainSchemeCard AosMainScheme
aos50087b = mainScheme AosMainScheme Cards.aos50087b (PerPlayer 12) (PerPlayer 6) (PerPlayer 1)

aos50088b :: MainSchemeCard AosMainScheme
aos50088b = mainScheme AosMainScheme Cards.aos50088b (PerPlayer 10) (PerPlayer 3) (PerPlayer 1)

aos50089b :: MainSchemeCard AosMainScheme
aos50089b = mainScheme AosMainScheme Cards.aos50089b (PerPlayer 18) (PerPlayer 12) (PerPlayer 1)

aos50104b :: MainSchemeCard AosMainScheme
aos50104b = mainScheme AosMainScheme Cards.aos50104b (PerPlayer 7) (PerPlayer 1) (PerPlayer 1)

aos50130b :: MainSchemeCard AosMainScheme
aos50130b = mainScheme AosMainScheme Cards.aos50130b (PerPlayer 11) (PerPlayer 1) (PerPlayer 1)

aos50167b :: MainSchemeCard AosMainScheme
aos50167b = mainScheme AosMainScheme Cards.aos50167b (PerPlayer 12) (PerPlayer 2) (PerPlayer 2)

aos50168b :: MainSchemeCard AosMainScheme
aos50168b = mainScheme AosMainScheme Cards.aos50168b (PerPlayer 0) (PerPlayer 0) (PerPlayer 0)

aos50169b :: MainSchemeCard AosMainScheme
aos50169b = mainScheme AosMainScheme Cards.aos50169b (PerPlayer 12) (Static 0) (PerPlayer 2)

instance IsMainScheme AosMainScheme where
  mainSchemeModifiersFor = getModifiersFor
  mainSchemeAbilities (AosMainScheme a) = case toCardCode a of
    "50089" -> [windowAbility a 1 (W.EnemyAttackedAnyTarget W.Would AnyEnemy) ForcedInterrupt NoCost (runAbility a 1)]
    "50167" -> [limitedWindowAbility a 2 (W.PhaseEnds W.After PlayerPhase) Response (SelfMatches FirstPlayer) NoCost (runAbility a 2)]
    _ -> []

instance HasModifiersFor AosMainScheme where
  getModifiersFor _ target (AosMainScheme a) = do
    case toCardCode a of
      "50089" -> do
        difficulty <- getDifficulty
        pure [KeywordModifier Quickstrike | difficulty == Expert, MinionRef _ <- [target]]
      "50130" -> do
        thunderbolt <- case target of MinionRef mid -> minionMatches (MinionWithTrait Trait.Thunderbolt) mid; _ -> pure False
        pure [KeywordModifier Guard | thunderbolt]
      _ -> pure []

instance RunMessage AosMainScheme where
  runMessage msg e@(AosMainScheme a) = runQueueT $ case msg of
    SetupMainScheme -> do
      first <- selectJust FirstPlayer
      case toCardCode a of
        "50067" -> getPlayers >>= traverse_ (\you -> searchAndPutIntoPlayEncounterCard you $ CardWithType MinionType)
        "50087" -> do
          putEncounterCardIntoPlay first =<< genEncounterCard Cards.aos50090a
          difficulty <- getDifficulty
          when (difficulty == Expert) $ cardSpecific a "aosInitialAlert" ()
        "50104" -> cardSpecific GameRef "aosSetupModok" ()
        "50130" -> revealEncounterCard first =<< genEncounterCard Cards.aos50131a
        "50167" -> do
          cardSpecific GameRef "aosPrepareEvidence" ()
          for_ [Cards.aos50181a,Cards.aos50182a,Cards.aos50183a] $ \def -> do
            already <- selectAny (EnvironmentIs def)
            unless already $ putEncounterCardIntoPlay first =<< genEncounterCard def
          cardSpecific GameRef "aosInitialBoardSecrets" ()
        _ -> pure ()
      pure e
    CardSpecific (isTarget a -> True) "aosInitialAlert" _ -> do
      amount <- fromIntegral <$> fromGameValue (PerPlayer 2)
      selectOne (EnvironmentWithTitle "Alert Level") >>= traverse_ (`placeCounters` amount)
      pure e
    MainSchemeMessage sid RevealMainScheme | sid == mainSchemeId a -> do
      first <- selectJust FirstPlayer
      case toCardCode a of
        "50089" -> do
          high <- selectAny (EnvironmentIs Cards.aos50090b)
          if high then getPlayers >>= traverse_ dealEncounterCard
          else do
            env <- selectOne (EnvironmentIs Cards.aos50090a)
            for_ env $ \eid -> getCountersOn eid >>= removeCounters eid >> flipEnvironment eid
          difficulty <- getDifficulty
          when (difficulty == Expert) $ cardSpecific a "aosInitialAlert" ()
        "50168" -> do
          cardSpecific GameRef "aosMakeAccusation" ()
          cardSpecific GameRef "aosCompareAccusation" ()
          nextMainScheme sid
        "50169" -> do
          villains <- selectList AnyVillain
          traverse_ flipVillain villains
          void $ searchAndAttachEncounterCard first (CardIs Cards.aos50170) =<< selectJust ActiveVillain
          cardSpecific GameRef "aosRevealMole" ()
          cardSpecific a "aosMoleThreat" ()
        _ -> pure ()
      pure e
    CardSpecific (isTarget a -> True) "aosMoleThreat" _ -> do
      attached <- selectList (AttachmentWithTrait Trait.BoardMember)
      secrets <- sum <$> traverse getCountersOn attached
      players <- fromIntegral . length <$> getPlayers
      placeThreatOn a (mainSchemeId a) (secrets*players)
      pure e
    CardSpecific (isTarget a -> True) "aosFreeCaptive" _ -> do
      first <- selectJust FirstPlayer
      captives <- filter (cardMatch $ CardWithTitle "Rescued Captive") <$> getSetAsidePlayerCards
      players <- getPlayers
      for_ (headMay captives) $ \card -> chooseEffect first [targeting you do
        putIntoPlay you card {pcController=Just you}
        exhaust (AllyId $ unCardId $ pcCardId card)
        | you <- players]
      cardSpecific a "aosAdvanceOrRescue" ()
      pure e
    CardSpecific (isTarget a -> True) "aosAdvanceOrRescue" _ -> do
      first <- selectJust FirstPlayer
      amount <- fromIntegral <$> fromGameValue (PerPlayer 3)
      chooseOne first
        [labeled "$label.aosAdvanceExtractCaptives" (nextMainScheme $ mainSchemeId a)
        ,labeled "$label.aosRescueAnotherCaptive" (placeThreatOn a (mainSchemeId a) amount)]
      pure e
    RanAbility you (isTarget a -> True) 1 windows _ -> do
      allies <- selectList (AllyWithTitle "Rescued Captive")
      let enemies = concatMap (\case W.EnemyAttack enemy _ -> [enemy]; W.EnemyAttackAlly enemy _ _ -> [enemy]; W.EnemyAttackOnEnemy enemy _ -> [enemy]; _ -> []) windows
      for_ enemies $ \enemy -> chooseEffect you [targeting aid do
        controller <- fromMaybe you <$> getAllyControllerId aid
        redirectEnemyAttackToAlly enemy controller aid
        | aid <- allies]
      pure e
    RanAbility you (isTarget a -> True) 2 _ _ -> do
      members <- selectRefs (EnvironmentWithTrait Trait.BoardMember)
      eligible <- filterM (fmap (==0) . getCountersOn) members
      difficulty <- isJust <$> getCampaign
      chooseEffect you $ [targeting t do
        placeCounters t 2
        cardSpecific GameRef "aosGainEvidence" (if difficulty then 1 :: Natural else 2)
        | t <- eligible] <> [labeled "$label.done" $ pure ()]
      cardSpecific a "aosOfferAccusation" ()
      pure e
    CardSpecific (isTarget a -> True) "aosOfferAccusation" _ -> do
      first <- selectJust FirstPlayer
      chooseOne first [labeled "$label.aosMakeAccusation" (nextMainScheme $ mainSchemeId a),labeled "$label.done" $ pure ()]
      pure e
    AdvanceMainScheme sid | sid == mainSchemeId a, toCardCode a == "50104" -> do
      cardSpecific GameRef "aosRecruitAdaptoidEnvironment" ()
      pure $ AosMainScheme a {mainSchemeThreat=0}
    AdvanceMainScheme sid | sid == mainSchemeId a -> e <$ gameOver Lost
    AccelerateMainScheme | toCardCode a == "50067" -> do
      vid <- selectJust ActiveVillain
      stage <- getVillainStage vid
      AosMainScheme <$> runMessage msg a {mainSchemeAcceleration=Static $ fromIntegral stage}
    CheckWindows windows | toCardCode a == "50130" -> do
      let attacks = [(you,enemy) | W.Window W.After (W.PlayerAttackResolved _ you enemies) <- windows, enemy <- enemies]
      for_ attacks $ \(you,enemy) -> case enemy of
        EnemyMinionId mid -> whenM (minionMatches (MinionWithTrait Trait.Thunderbolt) mid) $ engageMinion mid you
        _ -> pure ()
      pure e
    CheckWindows _ | toCardCode a == "50089" -> do
      empty <- selectNone (AllyWithTitle "Rescued Captive")
      when empty $ gameOver Lost
      pure e
    _ | toCardCode a == "50087" -> AosMainScheme <$> escapeStage (nextMainScheme $ mainSchemeId a) msg a
      | toCardCode a == "50088" -> AosMainScheme <$> escapeStage (cardSpecific a "aosFreeCaptive" ()) msg a
      | toCardCode a == "50089" -> AosMainScheme <$> escapeStage (gameOver Won) msg a
      | otherwise -> AosMainScheme <$> runMessage msg a
