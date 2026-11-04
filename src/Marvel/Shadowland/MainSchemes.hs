module Marvel.Shadowland.MainSchemes where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Damage
import Marvel.Difficulty
import Marvel.EncounterCard (genEncounterCard)
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Id
import Marvel.MainScheme.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Shadowland.Cards qualified as Cards
import Marvel.Shadowland.Corruption
import Marvel.Shadowland.Position
import Marvel.Window qualified as W

newtype ShadowMainScheme = ShadowMainScheme (Attrs MainScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
shadowMainSchemes :: [MainSchemeCard ShadowMainScheme]
shadowMainSchemes =
  [mainScheme ShadowMainScheme Cards.street65001b (PerPlayer 7) (PerPlayer 2) (PerPlayer 1)
  ,mainScheme ShadowMainScheme Cards.street65002b (PerPlayer 8) (PerPlayer 2) (PerPlayer 1)
  ,mainScheme ShadowMainScheme Cards.street65038b (Static 0) (PerPlayer 25) (PerPlayer 1)
  ,mainScheme ShadowMainScheme Cards.street65039b (PerPlayer 9) (PerPlayer 1) (PerPlayer 1)
  ,mainScheme ShadowMainScheme Cards.street65060b (PerPlayer 6) (PerPlayer 2) (PerPlayer 1)
  ,mainScheme ShadowMainScheme Cards.street65061b (PerPlayer 9) (Static 0) (PerPlayer 1)]
instance IsMainScheme ShadowMainScheme where
  mainSchemeAbilities = getAbilities
  mainSchemeModifiersFor _ target (ShadowMainScheme a)
    | toCardCode a `elem` ["65001b","65002b"] = do
      hard <- (== Expert) <$> getDifficulty
      villains <- selectList AnyVillain
      n <- getStartingPlayerCount
      pure [modifier | hard, target `elem` map VillainRef villains, modifier <- [SchemeModifier 1,HitPointModifier (3 * fromIntegral n)]]
    | otherwise = pure []
instance HasAbilities ShadowMainScheme where
  getAbilities (ShadowMainScheme a) = case toCardCode a of
    code | code `elem` ["65001b","65002b"] -> [windowAbility a 1 (W.WindowOneOf [W.VillainWouldBeDefeatedMatching $ VillainWithCardCode code | code <- ["65012a","65013a","65014a","65015a","65016a","65017a"]]) ForcedInterrupt NoCost (runAbility a 1)]
    "65038b" -> [windowAbility a 2 (W.EnemyWouldTakeDamage (EnemyIs Cards.street65037)) ForcedInterrupt NoCost (runAbility a 2)]
    "65039b" -> [limited (EachPlayer $ PerRound 1) $ windowAbility a 3 (W.CharacterThwartsScheme W.After AnySource $ SchemeWithId $ SchemeMainSchemeId $ mainSchemeId a) ForcedResponse NoCost (runAbility a 3)]
    "65060b" -> [windowAbility a 4 (W.EnemyDefeated W.After (EnemyWithTitle "Krahllak") AnyDamage) ForcedResponse NoCost (runAbility a 4)]
    "65061b" -> [windowAbility a 5 W.AfterVillainThreatStep ForcedResponse NoCost (runAbility a 5)]
    _ -> []
instance RunMessage ShadowMainScheme where
  runMessage msg e@(ShadowMainScheme a) = runQueueT $ case msg of
    SetupMainScheme -> do
      when (toCardCode a == "65001b") do
        first <- selectJust FirstPlayer
        traverse_ (\def -> putEncounterCardIntoPlay first =<< genEncounterCard def) [Cards.street65003a,Cards.street65004a]
        whenM ((== Expert) <$> getDifficulty) $ selectOne ActiveVillain >>= traverse_ (\vid -> villainStep vid SetVillainHp)
      when (toCardCode a == "65038b") $ cardSpecific GameRef "slCreateTrapDeck" ()
      pure e
    MainSchemeMessage mid RevealMainScheme | mid == mainSchemeId a -> do
      case toCardCode a of
        "65002b" -> changePosition False >> (getAllPlayers >>= traverse_ (\you -> searchAndRevealEncounterCard you $ CardWithType MinionType))
        "65038b" -> do
          n <- fromIntegral <$> getStartingPlayerCount
          hard <- (== Expert) <$> getDifficulty
          selectOne ActiveVillain >>= traverse_ (\vid -> placeCounters vid $ (if hard then 7 else 10) * n)
        "65039b" -> do
          villains <- selectList AnyVillain
          for_ villains $ \vid -> getsGame (`cardInPlay` VillainRef vid) >>= traverse_ (\card -> setAside [card])
          hard <- (== Expert) <$> getDifficulty
          first <- selectJust FirstPlayer
          cardSpecific GameRef "slRevealChosen" (first,hard)
        "65061b" -> do
          players <- getAllPlayers
          for_ players $ \you -> do
            overpowering <- overpowered you
            if overpowering then discardFrom you RandomFromHand 1 else discardableControlledCardsMatching you AnyCard >>= chooseAndDiscardControlled you
          whenM ((== Expert) <$> getDifficulty) $ traverse_ dealEncounterCard players
        _ -> pure ()
      pure e
    RanAbility _ (isTarget a -> True) 1 windows _ -> do
      for_ [vid | W.VillainDefeatWouldOccur vid <- windows] $ \vid -> do
        code <- getsGame $ fmap toCardCode . (`cardInPlay` VillainRef vid)
        for_ code $ \c -> when (c `elem` ["65012a","65013a","65014a","65015a","65016a","65017a"]) do
          cancelVillainDefeat vid
          replaceVillain vid (flipCardCode c) Advanced
          changePosition False
          getAllPlayers >>= traverse_ dealEncounterCard
      pure e
    RanAbility you (isTarget a -> True) 2 windows _ -> do
      for_ [(vid,source,damage) | W.EnemyWouldBeDamaged (EnemyVillainId vid) source damage <- windows] $ \(vid,source,damage) -> do
        current <- getsGame $ fmap toCardCode . (`cardInPlay` VillainRef vid)
        when (current == Just "65037") do
          who <- fromMaybe you <$> getSourceController source
          cancelEnemyDamage (EnemyVillainId vid)
          removeThreat who a (damageAmount damage) (SchemeWithId $ SchemeMainSchemeId $ mainSchemeId a)
      pure e
    RanAbility you (isTarget a -> True) 3 windows _ -> do
      who <- case headMay [source | W.ThwartRemovedThreat source _ _ _ <- windows] of
        Nothing -> pure you
        Just source -> fromMaybe you <$> getSourceController source
      cardSpecific GameRef "slRevealTrap" who
      pure e
    RanAbility _ (isTarget a -> True) 4 _ _ -> nextMainScheme (mainSchemeId a) >> pure e
    RanAbility _ (isTarget a -> True) 5 _ _ -> do
      getAllPlayers >>= traverse_ (\you -> corruptionChoice you 1 $ placeThreatOn a (mainSchemeId a) 3)
      pure e
    AdvanceMainScheme mid | mid == mainSchemeId a -> do
      case toCardCode a of
        "65001b" -> nextMainScheme mid
        "65060b" -> getAllPlayers >>= traverse_ (\you -> corruptionChoice you 2 $ damageIdentity you a (toDamage 3 FromAbility) >> dealEncounterCard you)
        "65038b" -> pure ()
        _ -> gameOver Lost
      pure $ if toCardCode a == "65060b" then ShadowMainScheme a {mainSchemeThreat = 0} else e
    MainSchemeMessage mid (MainSchemePlaceThreat n) | mid == mainSchemeId a, toCardCode a == "65038b" -> pure $ ShadowMainScheme a {mainSchemeThreat = mainSchemeThreat a + n}
    _ -> do
      a' <- runMessage msg a
      when (toCardCode a == "65038b" && mainSchemeThreat a > 0 && mainSchemeThreat a' == 0) $ nextMainScheme (mainSchemeId a)
      pure $ ShadowMainScheme a'
