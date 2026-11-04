module Marvel.Shadowland.Encounters where

import Data.Aeson.KeyMap qualified as KeyMap
import Marvel.Prelude
import Marvel.Attack
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hp
import Marvel.Id
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Types
import Marvel.Minion.Runner ()
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Shadowland.Cards qualified as Cards
import Marvel.Shadowland.Corruption
import Marvel.Shadowland.Position
import Marvel.SideScheme.Types
import Marvel.Stats
import Marvel.Treachery.Types
import Marvel.Window qualified as W

data ShadowMinion = ShadowMinion (Attrs Minion) Natural
  deriving stock (Show, Eq)
instance IsMinion ShadowMinion where
  toMinionAttrs (ShadowMinion a _) = a
  overMinionAttrs f (ShadowMinion a n) = ShadowMinion (f a) n
  minionDisplayCounters (ShadowMinion _ n) = n
instance HasCardCode ShadowMinion where
  toCardCode (ShadowMinion a _) = toCardCode a
instance IsRef ShadowMinion where
  toRef (ShadowMinion a _) = toRef a
instance ToJSON ShadowMinion where
  toJSON (ShadowMinion a n) = case toJSON a of
    Object o -> Object $ KeyMap.insert "shadowCounters" (toJSON n) o
    value -> value
instance FromJSON ShadowMinion where
  parseJSON = withObject "ShadowMinion" $ \o -> ShadowMinion <$> parseJSON (Object o) <*> o .:? "shadowCounters" .!= 0
shadowMinions :: [MinionCard ShadowMinion]
shadowMinions = [minion (`ShadowMinion` 0) def (Sch sch) (Atk atk) (HP hp) | (def,sch,atk,hp) <-
  [(Cards.street65005,2,3,5),(Cards.street65006,1,3,4),(Cards.street65041,2,2,5),(Cards.street65066,2,2,6),
   (Cards.street65067,1,3,7),(Cards.street65068,1,4,9),(Cards.street65069,1,2,8)]]
instance HasModifiersFor ShadowMinion where
  getModifiersFor _ target (ShadowMinion a n) = pure [KeywordModifier keyword | target == toRef a,keyword <- case toCardCode a of
    "65068" -> [Overkill]
    "65069" | n > 0 -> [Guard,Patrol]
    _ -> []]
instance HasAbilities ShadowMinion where
  getAbilities (ShadowMinion a _) =
    [windowAbility a 1 (W.MinionDefeated W.When $ MinionWithId $ minionId a) WhenRevealed NoCost (runAbility a 1) | toCardCode a `elem` ["65006","65067"]]
    <> [ability a 2 HeroAction hasCorruption NoCost (runAbility a 2) | toCardCode a == "65066"]
    <> [ability a 3 HeroAction (EnemyExists (EnemyWithId (toEnemyId a) <> NotEnemy (EnemyWithRemainingHitPoints $ AtLeast $ Static 3))) (ExhaustCharacterCost $ IdentityCharacters You) (runAbility a 3) | toCardCode a == "65067"]
    <> [windowAbility a 4 (W.EnemyWouldAttack (EnemyWithId $ toEnemyId a) AnyIdentity) Interrupt NoCost (runAbility a 4) | toCardCode a == "65068"]
   where hasCorruption = UpgradeExists (UpgradeControlledBy You <> UpgradeNamed "Creeping Corruption")
instance RunMessage ShadowMinion where
  runMessage msg e@(ShadowMinion a counters) = runQueueT $ case msg of
    MinionMessage mid (RevealMinion you) | mid == minionId a -> do
      case toCardCode a of
        "65005" -> changePosition True >> cardSpecific a "blackTarantulaPosition" you
        "65041" -> do
          corridors <- selectAny (VillainWithCardCode "65037")
          if corridors then cardSpecific GameRef "slMixTraps" (2 :: Natural) else cardSpecific GameRef "slRevealTrap" you
        "65069" -> corruptionChoice you 1 (placeCounters mid 1)
        _ -> pure ()
      (\attrs -> ShadowMinion attrs counters) <$> runMessage msg a
    CardSpecific (isTarget a -> True) "blackTarantulaPosition" (fromJSON -> Success you) -> do
      atop <- villainAtop
      if atop then enemyAttacks (minionId a) you else do
        giveStatus (minionId a) ToughStatus
        selectOne ActiveVillain >>= traverse_ (`giveStatus` ToughStatus)
      pure e
    RanAbility you (isTarget a -> True) 1 windows _ -> do
      who <- maybe (pure you) (fmap (fromMaybe you) . getSourceController) $ headMay [source | W.DefeatedMinion _ _ source _ <- windows]
      if toCardCode a == "65006" then changePosition True >> cardSpecific a "ghostMakerPosition" who
        else corrupt who 2 >> heal who 6
      pure e
    CardSpecific (isTarget a -> True) "ghostMakerPosition" (fromJSON -> Success you) -> do
      atop <- villainAtop
      if atop then discardableControlledCardsMatching you AnyCard >>= chooseAndDiscardControlled you else dealIndirectDamage you a 2
      pure e
    RanAbility you (isTarget a -> True) 2 _ _ -> corrupt you 1 >> dealDamage (minionId a) a (toDamage 4 FromAbility) >> pure e
    RanAbility you (isTarget a -> True) 3 _ _ -> removeFromPlay a >> purify you 1 >> pure e
    RanAbility you (isTarget a -> True) 4 windows _ -> do
      let who = fromMaybe you $ headMay [p | W.EnemyAttack _ p <- windows]
      corrupt who 1
      changeAttack (minionId a) (-3)
      pure e
    PlaceCounters target n | isTarget a target -> pure $ ShadowMinion a (counters+n)
    RemoveCounters target n | isTarget a target -> pure $ ShadowMinion a (subtractNatural n counters)
    Boost (RevealedAsBoost target enemy) | isTarget a target -> do
      you <- getBoostedPlayer enemy
      case toCardCode a of
        "65006" -> do
          attack <- getEnemyAttack enemy
          case attack of Just atk | attackDefended atk, AllyCharacter _ <- attackCharacter atk -> revealBoost a you; _ -> pure ()
        "65041" -> putBoostIntoPlay a you
        _ -> pure ()
      pure e
    _ -> (\attrs -> ShadowMinion attrs counters) <$> runMessage msg a

newtype ShadowSideScheme = ShadowSideScheme (Attrs SideScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance IsSideScheme ShadowSideScheme where
  sideSchemeAbilities = getAbilities
shadowSideSchemes :: [SideSchemeCard ShadowSideScheme]
shadowSideSchemes = [sideScheme (\a -> ShadowSideScheme a {sideSchemeCrisis=crisis}) def threat | (def,threat,crisis) <-
  [(Cards.street65007,Static 4,True),(Cards.street65008,PerPlayer 3,False),(Cards.street65009,PerPlayer 2,True),
   (Cards.street65042,PerPlayer 2,True),(Cards.street65043,Static 3,False),(Cards.street65070,PerPlayer 5,False)]]
instance HasModifiersFor ShadowSideScheme where
  getModifiersFor _ target (ShadowSideScheme a)
    | target == toRef a, toCardCode a == "65008" = do
      atop <- villainAtop
      pure [if atop then HazardIconModifier 1 else AccelerationIconModifier 1]
    | otherwise = pure []
instance HasAbilities ShadowSideScheme where
  getAbilities (ShadowSideScheme a) =
    [windowAbility a 1 (W.SideSchemeDefeated W.When $ SideSchemeWithId $ sideSchemeId a) WhenRevealed NoCost (runAbility a 1) | toCardCode a `elem` ["65007","65043"]]
    <> [ability a 2 HeroAction (UpgradeExists $ UpgradeControlledBy You <> UpgradeNamed "Creeping Corruption") NoCost (runAbility a 2) | toCardCode a == "65070"]
instance RunMessage ShadowSideScheme where
  runMessage msg e@(ShadowSideScheme a) = runQueueT $ case msg of
    SideSchemeMessage sid RevealSideScheme | sid == sideSchemeId a -> do
      case toCardCode a of
        "65009" -> changePosition True >> cardSpecific a "surroundPosition" ()
        "65042" -> do
          corridor <- selectOne (VillainWithCardCode "65037")
          case corridor of Just vid -> removeCounters vid 1; Nothing -> fromIntegral <$> getStartingPlayerCount >>= placeThreatOn a sid
        _ -> pure ()
      pure e
    CardSpecific (isTarget a -> True) "surroundPosition" _ -> do
      atop <- villainAtop
      n <- fromIntegral <$> getStartingPlayerCount
      if atop then selectOne EncounterMainScheme >>= traverse_ (\mid -> placeThreatOn a mid n) else placeThreatOn a (sideSchemeId a) n
      pure e
    RanAbility you (isTarget a -> True) 1 windows _ -> do
      let who = fromMaybe you $ headMay [p | W.DefeatedSideSchemeBy _ p _ <- windows]
      if toCardCode a == "65007" then changePosition True else do
        removeThreat who a 5 EncounterMainScheme
        discardUntil FromEncounterDeck (CardMatchesAny [CardWithType MinionType,CardWithType SideSchemeType]) a
      pure e
    WithDiscardedMatch (isTarget a -> True) FromEncounterDeck (EncounterCard card) -> getYou >>= \you -> revealFoundEncounterCard you card >> pure e
    RanAbility you (isTarget a -> True) 2 _ _ -> corrupt you 1 >> removeThreat you a 5 (SchemeWithId $ SchemeSideSchemeId $ sideSchemeId a) >> pure e
    Boost (RevealedAsBoost target _) | isTarget a target, toCardCode a == "65043" -> selectOne EncounterMainScheme >>= traverse_ (\mid -> placeThreatOn a mid 3) >> pure e
    _ -> ShadowSideScheme <$> runMessage msg a

newtype ShadowTreachery = ShadowTreachery (Attrs Treachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
  deriving anyclass IsTreachery
shadowTreacheries :: [TreacheryCard ShadowTreachery]
shadowTreacheries = map (treachery ShadowTreachery) [Cards.street65010,Cards.street65011,Cards.street65044,Cards.street65045,Cards.street65071]
instance RunMessage ShadowTreachery where
  runMessage msg e@(ShadowTreachery a) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId a -> resolveCard you >> pure e
    CardSpecific (isTarget a -> True) "chaosPosition" (fromJSON -> Success you) -> do
      atop <- villainAtop
      if atop then selectOne EncounterMainScheme >>= traverse_ (\mid -> placeThreatOn a mid 2) else damageIdentity you a (toDamage 2 FromTreachery)
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget a target -> do
      you <- getBoostedPlayer enemy
      case toCardCode a of
        "65010" -> resolveCard you
        "65044" -> whenM (isJust <$> getEnemyAttack enemy) $ addEnemyBoost enemy 2 >> attackGains enemy Overkill
        "65045" -> cardSpecific GameRef "slMixTraps" (2 :: Natural)
        "65071" -> corruptionChoice you 1 (stunIdentity you)
        _ -> pure ()
      pure e
    _ -> ShadowTreachery <$> runMessage msg a
   where
    resolveCard you = case toCardCode a of
      "65010" -> changePosition True >> cardSpecific a "chaosPosition" you
      "65011" -> do
        hero <- identityMatches HeroIdentity you
        if hero then do
          atop <- villainAtop
          selectOne ActiveVillain >>= traverse_ (\vid -> if atop then villainSchemesAgainst vid you else enemyAttacks vid you)
        else changePosition True >> confuseIdentity you
      "65044" -> selectOne ActiveVillain >>= traverse_ (\vid -> villainActivatesAgainst vid you)
      "65045" -> cardSpecific GameRef "slMixTraps" (3 :: Natural) >> cardSpecific GameRef "slRevealTrap" you
      "65071" -> corruptionChoice you 1 $ purify you 1 >> (selectOne ActiveVillain >>= traverse_ (\vid -> enemyAttacks vid you))
      _ -> pure ()
