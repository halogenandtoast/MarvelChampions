module Marvel.Shadowland.Villains where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Difficulty
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hp
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Shadowland.Cards qualified as Cards
import Marvel.Shadowland.Corruption
import Marvel.Shadowland.ProtegeAbilities
import Marvel.Stats
import Marvel.Villain.Types
import Marvel.Window qualified as W

newtype ShadowVillain = ShadowVillain (Attrs Villain)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)
  deriving anyclass IsVillain
shadowVillains :: [VillainCard ShadowVillain]
shadowVillains = [villain ShadowVillain def (Sch sch) (Atk atk) (HP $ PerPlayer hp) | (def,sch,atk,hp) <-
  [(Cards.street65012a,2,1,15),(Cards.street65012b,2,2,18),(Cards.street65013a,1,2,15),(Cards.street65013b,2,2,18),
   (Cards.street65014a,1,2,15),(Cards.street65014b,2,2,18),(Cards.street65015a,1,2,15),(Cards.street65015b,2,2,18),
   (Cards.street65016a,2,1,15),(Cards.street65016b,2,2,18),(Cards.street65017a,1,0,15),(Cards.street65017b,1,1,18)]]

krahllak :: [VillainCard ShadowVillain]
krahllak = [villainWith ShadowVillain def (Sch sch) (Atk atk) (HP $ PerPlayer hp) (stageL .~ stage) | (def,sch,atk,hp,stage) <-
  [(Cards.street65057,2,3,18,1),(Cards.street65058,2,4,21,2),(Cards.street65059,3,4,25,3)]]

instance HasModifiersFor ShadowVillain where
  getModifiersFor _ target (ShadowVillain a) = pure [KeywordModifier key | target == toRef a,
    key <- case toCardCode (getCardDef a) of "65058" -> [Overkill]; "65059" -> [Overkill,Piercing]; _ -> []]

instance HasAbilities ShadowVillain where
  getAbilities (ShadowVillain a) = protegeAbilities a (toCardCode $ getCardDef a) (toEnemyId a)
    <> [windowAbility a 7 (W.EnemyWouldAttack (EnemyWithId $ toEnemyId a) AnyIdentity) Interrupt NoCost (runAbility a 7) | toCardCode (getCardDef a) `elem` ["65057","65058","65059"]]

instance RunMessage ShadowVillain where
  runMessage msg e@(ShadowVillain a) = runQueueT $ case msg of
    VillainMessage vid VillainDefeated | vid == villainId a -> do
      hard <- (== Expert) <$> getDifficulty
      case toCardCode (getCardDef a) of
        "65057" -> replaceVillain vid "65058" Advanced >> pure e
        "65058" | hard -> replaceVillain vid "65059" Advanced >> pure e
        _ -> ShadowVillain <$> runMessage msg a
    RanAbility you (isTarget a -> True) 7 windows _ -> do
      let who = fromMaybe you $ headMay [p | W.EnemyAttack _ p <- windows]
      corrupt who 1
      changeAttack (villainId a) (-3)
      pure e
    RanAbility you (isTarget a -> True) index windows _ -> do
      resolveProtegeAbility a (toCardCode $ getCardDef a) (toEnemyId a) (villainAttacking a) you index windows
      pure e
    WithDiscarded (isTarget a -> True) _ cards -> resolveElektraDiscard (toEnemyId a) cards >> pure e
    _ -> ShadowVillain <$> runMessage msg a

newtype ShiftingCorridors = ShiftingCorridors (Attrs Villain)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)
  deriving anyclass (IsVillain, HasAbilities)
shiftingCorridors :: VillainCard ShiftingCorridors
shiftingCorridors = villain ShiftingCorridors Cards.street65037 (Sch 0) (Atk 0) (HP $ Static 0)
instance HasModifiersFor ShiftingCorridors where
  getModifiersFor _ target (ShiftingCorridors a) = pure [InfiniteHitPoints | target == toRef a]
instance RunMessage ShiftingCorridors where
  runMessage msg e@(ShiftingCorridors a) = runQueueT $ case msg of
    VillainMessage vid declaration | vid == villainId a, Just recipient <- activationRecipient declaration -> do
      blocked <- elem CannotActivate <$> getModifiers a
      unless blocked $ maybe getActivePlayerId pure recipient >>= replaceActivation
      pure e
    VillainMessage vid VillainDefeated | vid == villainId a -> pure e
    RemoveCounters target n | isTarget a target -> do
      when (n >= villainCounters a) $ gameOver Lost
      ShiftingCorridors <$> runMessage msg a
    _ -> ShiftingCorridors <$> runMessage msg a
   where
    activationRecipient = \case
      VillainAttacks you -> Just (Just you)
      VillainAttacksWithoutBoost you -> Just (Just you)
      VillainAttacksGaining you _ -> Just (Just you)
      VillainAttacksWithBonus you _ _ -> Just (Just you)
      VillainAttacksWithAdditionalBoostCards you _ -> Just (Just you)
      VillainAttacksAlly you _ -> Just (Just you)
      VillainAttacksCharacter you _ -> Just (Just you)
      VillainAttacksCharacterGaining you _ _ -> Just (Just you)
      VillainAttacksCharacterWithBonus you _ _ _ -> Just (Just you)
      VillainSchemesAgainst you -> Just (Just you)
      VillainSchemesWithBonus you _ -> Just (Just you)
      VillainSchemes -> Just Nothing
      _ -> Nothing
    replaceActivation you = do
      removeCounters (villainId a) 1
      cardSpecific GameRef "slRevealTrap" you
