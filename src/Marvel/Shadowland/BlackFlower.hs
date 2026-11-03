module Marvel.Shadowland.BlackFlower where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hp
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
import Marvel.SideScheme.Types
import Marvel.Stats
import Marvel.Treachery.Types
import Marvel.Window qualified as W

newtype FlowerMinion = FlowerMinion (Attrs Minion)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
  deriving anyclass (IsMinion, HasModifiersFor)
flowerMinions :: [MinionCard FlowerMinion]
flowerMinions = [minion FlowerMinion Cards.street65073 (Sch 1) (Atk 3) (HP 6),minion FlowerMinion Cards.street65074 (Sch 1) (Atk 2) (HP 1)]
instance HasAbilities FlowerMinion where
  getAbilities (FlowerMinion a)
    | toCardCode a == "65073" = [windowAbility a 1 (W.EnemyWouldAttack (EnemyWithId $ toEnemyId a) AnyIdentity) Interrupt NoCost (runAbility a 1)]
    | otherwise = [windowAbility a 2 (W.MinionDefeated W.When $ MinionWithId $ minionId a) WhenRevealed NoCost (runAbility a 2)]
instance RunMessage FlowerMinion where
  runMessage msg e@(FlowerMinion a) = runQueueT $ case msg of
    RanAbility you (isTarget a -> True) 1 windows _ -> do
      for_ [p | W.EnemyAttack _ p <- windows] $ \p -> do
        present <- isJust <$> corruption p
        when present $ corrupt p 1 >> cancelEnemyActivation (minionId a)
      when (null [p | W.EnemyAttack _ p <- windows]) $ corrupt you 1 >> cancelEnemyActivation (minionId a)
      pure e
    RanAbility you (isTarget a -> True) 2 windows _ -> do
      who <- maybe (pure you) (fmap (fromMaybe you) . getSourceController) $ headMay [s | W.DefeatedMinion _ _ s _ <- windows]
      corrupt who 2
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget a target -> do
      you <- getBoostedPlayer enemy
      corruptionChoice you 1 $ if toCardCode a == "65073" then discardFrom you RandomFromHand 1 else addEnemyBoost enemy 3
      pure e
    _ -> FlowerMinion <$> runMessage msg a

newtype FlowerScheme = FlowerScheme (Attrs SideScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
  deriving anyclass HasModifiersFor
instance IsSideScheme FlowerScheme where
  sideSchemeAbilities = getAbilities
flowerSchemes :: [SideSchemeCard FlowerScheme]
flowerSchemes = [sideScheme FlowerScheme Cards.street65075 (PerPlayer 1),sideScheme FlowerScheme Cards.street65076 (Static 1)]
instance HasAbilities FlowerScheme where
  getAbilities (FlowerScheme a) = [windowAbility a 1 (W.SideSchemeDefeated W.When $ SideSchemeWithId $ sideSchemeId a) WhenRevealed NoCost (runAbility a 1) | toCardCode a == "65076"]
instance RunMessage FlowerScheme where
  runMessage msg e@(FlowerScheme a) = runQueueT $ case msg of
    SideSchemeMessage sid RevealSideScheme | sid == sideSchemeId a, toCardCode a == "65075" -> do
      getAllPlayers >>= traverse_ (\you -> corruptionChoice you 1 $ placeThreatOn a sid 3)
      pure e
    RanAbility you (isTarget a -> True) 1 windows _ -> do
      let who = fromMaybe you $ headMay [p | W.DefeatedSideSchemeBy _ p _ <- windows]
      corrupt who 2
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget a target, toCardCode a == "65076" -> do
      you <- getBoostedPlayer enemy
      corruptionChoice you 1 $ selectOne EncounterMainScheme >>= traverse_ (\mid -> placeThreatOn a mid 4)
      pure e
    _ -> FlowerScheme <$> runMessage msg a

newtype FlowerTreachery = FlowerTreachery (Attrs Treachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
  deriving anyclass IsTreachery
flowerTreacheries :: [TreacheryCard FlowerTreachery]
flowerTreacheries = map (treachery FlowerTreachery) [Cards.street65077,Cards.street65078]
instance RunMessage FlowerTreachery where
  runMessage msg e@(FlowerTreachery a) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId a -> do
      if toCardCode a == "65078" then corrupt you 1 >> cardSpecific a "invasiveThoughts" you else do
        hero <- identityMatches HeroIdentity you
        if hero then do
          overpowering <- overpowered you
          activate you overpowering 1
        else corruptionChoice you 2 (activate you True 2)
      pure e
    CardSpecific (isTarget a -> True) "invasiveThoughts" (fromJSON -> Success you) -> do
      overpowering <- overpowered you
      if overpowering then selectOne EncounterMainScheme >>= traverse_ (\mid -> placeThreatOn a mid 1)
        else damageIdentity you a (toDamage 1 FromTreachery)
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget a target, toCardCode a == "65078" -> do
      you <- getBoostedPlayer enemy
      corruptionChoice you 1 $ dealIndirectDamage you a 4
      pure e
    _ -> FlowerTreachery <$> runMessage msg a
   where
    activate you scheme bonus = selectOne ActiveVillain >>= traverse_ (\vid ->
      if scheme then enemyActivationBonus vid bonus >> villainSchemesAgainst vid you else enemyAttacks vid you)
