module Marvel.Villain.Villains.TricksterTakeover where

import System.Random.Shuffle (shuffleM)
import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Entity
import Marvel.Cost
import Marvel.Damage
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hp
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Stats
import Marvel.TricksterTakeover.Cards qualified as Cards
import Marvel.TricksterTakeover.Helpers
import Marvel.Window qualified as W
import Marvel.Villain.Types

newtype TTVillain = TTVillain (Attrs Villain)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)
  deriving anyclass IsVillain

tt55001 :: VillainCard TTVillain
tt55001 = villain TTVillain Cards.tt55001 (Sch 2) (Atk 1) (HP $ PerPlayer 15)

tt55002 :: VillainCard TTVillain
tt55002 = villainWith TTVillain Cards.tt55002 (Sch 2) (Atk 2) (HP $ PerPlayer 16) (stageL .~ 2)

tt55003 :: VillainCard TTVillain
tt55003 = villainWith TTVillain Cards.tt55003 (Sch 3) (Atk 2) (HP $ PerPlayer 18) (stageL .~ 3)

tt55027a :: VillainCard TTVillain
tt55027a = villain TTVillain Cards.tt55027a (Sch 0) (Atk 0) (HP $ PerPlayer 20)

tt55027b :: VillainCard TTVillain
tt55027b = villain TTVillain Cards.tt55027b (Sch 0) (Atk 0) (HP $ PerPlayer 20)

tt55029a :: VillainCard TTVillain
tt55029a = villain TTVillain Cards.tt55029a (Sch 1) (Atk 1) (HP $ PerPlayer 15)

tt55029b :: VillainCard TTVillain
tt55029b = villain TTVillain Cards.tt55029b (Sch 0) (Atk 0) (HP $ PerPlayer 99)

tt55030a :: VillainCard TTVillain
tt55030a = villain TTVillain Cards.tt55030a (Sch 1) (Atk 2) (HP $ PerPlayer 15)

tt55030b :: VillainCard TTVillain
tt55030b = villain TTVillain Cards.tt55030b (Sch 0) (Atk 0) (HP $ PerPlayer 99)

tt55031a :: VillainCard TTVillain
tt55031a = villain TTVillain Cards.tt55031a (Sch 2) (Atk 1) (HP $ PerPlayer 15)

tt55031b :: VillainCard TTVillain
tt55031b = villain TTVillain Cards.tt55031b (Sch 0) (Atk 0) (HP $ PerPlayer 99)

tt55032a :: VillainCard TTVillain
tt55032a = villain TTVillain Cards.tt55032a (Sch 1) (Atk 1) (HP $ PerPlayer 15)

tt55032b :: VillainCard TTVillain
tt55032b = villain TTVillain Cards.tt55032b (Sch 0) (Atk 0) (HP $ PerPlayer 99)

instance HasModifiersFor TTVillain

instance HasAbilities TTVillain where
  getAbilities (TTVillain a)
    | toCardCode (getCardDef a) `elem` ["55001", "55002", "55003"] =
      [windowAbility a 1 (W.EnemyAttacked W.After (EnemyWithId $ toEnemyId a) AnyIdentity) ForcedResponse NoCost (runAbility a 1)]
      <> [windowAbility a 2 (W.VillainRevealed W.When (VillainWithId $ villainId a) W.RevealedFromVillain) WhenRevealed NoCost (runAbility a 2) | toCardCode (getCardDef a) /= "55001"]
    | otherwise =
      [windowAbility a 3 (W.TreacheryResolvedBy AnyIdentity) ForcedResponse NoCost (runAbility a 3) | toCardCode (getCardDef a) `elem` ["55030a", "55031a"]]
      <> [windowAbility a 4 (W.EnemyActivates W.When (EnemyWithId $ toEnemyId a) AnyIdentity) ForcedInterrupt NoCost (runAbility a 4) | toCardCode (getCardDef a) == "55032a"]
      <> [windowAbility a 5 (W.BoostCardFlipped W.After $ EnemyWithId $ toEnemyId a) ForcedResponse NoCost (runAbility a 5) | toCardCode (getCardDef a) == "55029a"]

instance RunMessage TTVillain where
  runMessage msg e@(TTVillain a) = runQueueT $ case msg of
    RanAbility you (isTarget a -> True) 1 windows _ -> do
      charm $ fromMaybe you $ headMay [p | W.EnemyAttack _ p <- windows]
      pure e
    RanAbility _ (isTarget a -> True) 2 _ _ -> do
      hard <- expert
      when (toCardCode (getCardDef a) == "55003" || not hard) $ do
        first <- selectJust FirstPlayer
        searchEverywhereAndPutIntoPlayEncounterCard first (CardIs Cards.tt55006)
        cardSpecific a "ttDespairThreat" (if toCardCode (getCardDef a) == "55003" then 4 else 3 :: Natural)
      pure e
    CardSpecific (isTarget a -> True) "ttDespairThreat" (fromJSON -> Success (n :: Natural)) -> do
      count <- getPlayerCount
      schemes <- selectList (SideSchemeIs Cards.tt55006)
      traverse_ (\sid -> placeThreatOn a sid (n * fromIntegral count)) schemes
      pure e
    VillainMessage vid VillainDefeated | vid == villainId a, toCardCode (getCardDef a) `elem` ["55001", "55002"] -> do
      hard <- expert
      case toCardCode (getCardDef a) of
        "55001" -> advanceVillain vid >> pure (advanceVillainTo tt55002 a)
        _ | hard -> advanceVillain vid >> pure (advanceVillainTo tt55003 a)
        _ -> TTVillain <$> runMessage msg a
    VillainMessage vid VillainWouldBeDefeated | vid == villainId a, toCardCode (getCardDef a) `elem` ["55029a", "55030a", "55031a", "55032a"] -> do
      count <- getPlayerCount
      placeCounters vid (5 * fromIntegral count)
      villainStep vid FlipVillain
      pure e
    VillainMessage vid FlipVillain | vid == villainId a -> do
      let other = case toCardCode (getCardDef a) of
            "55029a" -> tt55029b
            "55030a" -> tt55030b
            "55031a" -> tt55031b
            _ -> tt55032b
      cardSpecific a "ttShatterIllusion" ()
      pure $ flipVillainTo other a
    CardSpecific (isTarget a -> True) "ttShatterIllusion" _ -> do
      count <- getPlayerCount
      let reward = case toCardCode (getCardDef a) of
            "55029b" -> "55055"
            "55030b" -> "55054"
            "55031b" -> "55052"
            _ -> "55053"
      cardSpecific GameRef "ttLokiDamage" (villainCounters a)
      cardSpecific a "ttSwapAvatar" True
      getAllPlayers >>= traverse_ dealEncounterCard
      synergy reward (fromIntegral count)
      pure $ TTVillain a {villainCounters = 0}
    CardSpecific (isTarget a -> True) "ttSwapAvatar" (fromJSON -> Success (reset :: Bool)) -> do
      options <- shuffleM [c | c <- [tt55029a, tt55030a, tt55031a, tt55032a], toCardCode c `notElem` (toCardCode (getCardDef a) : cdAlternateCardCodes (getCardDef a))]
      case options of
        c : _ -> do
          count <- getPlayerCount
          let TTVillain swapped = flipVillainTo c a
          pure $ TTVillain $ if reset then swapped {villainHp = HP (15 * count)} else swapped
        _ -> pure e
    RanAbility you (isTarget a -> True) 3 windows _ -> do
      let who = fromMaybe you $ headMay [p | W.ResolvedTreacheryCard p _ <- windows]
      if toCardCode (getCardDef a) == "55030a" then do
        mainThreat a 1
        schemes <- selectList AnySideScheme
        chooseOne who [targeting sid (placeThreatOn a sid 1) | sid <- schemes]
      else do
        damageIdentity who a (toDamage 1 FromAbility)
        allies <- selectList (AllyControlledBy $ IdentityWithId who)
        chooseOne who [targeting aid (damageCharacter aid a $ toDamage 1 FromAbility) | aid <- allies]
      pure e
    RanAbility _ (isTarget a -> True) 4 _ _ -> do
      discardTopOfEncounterDeckThen 1 $ \cards -> when (any isTreachery cards) $ enemyActivationBonus (villainId a) 1
      pure e
    RanAbility you (isTarget a -> True) 5 windows _ -> do
      for_ [c | W.FlippedBoostCard c _ <- windows, isTreachery c] $ \c -> do
        payable <- canAffordCost you (toSource a) (ResourceCost Nothing)
        chooseOne you $ [labeled "$label.ttPayResource" (payForTreachery you $ ResourceCost Nothing) | payable]
          <> [cardLabeled (toCardCode c) $ dealBoostAsEncounterCard c you]
      pure e
    _ -> TTVillain <$> runMessage msg a
