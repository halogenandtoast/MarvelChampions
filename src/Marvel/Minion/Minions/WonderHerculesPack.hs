module Marvel.Minion.Minions.WonderHerculesPack where

import Marvel.Prelude
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Game.Source
import Marvel.Id
import Marvel.Keyword qualified as K
import Marvel.Matchers
import Marvel.Query
import Marvel.Question
import Marvel.Resource.Types
import Marvel.Window qualified as W
import Marvel.Ability
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Minion.Types
import Marvel.WonderMan.Cards qualified as WM
import Marvel.Hercules.Cards qualified as HC
import Marvel.Minion.Runner

wm58026 :: MinionCard WmHcMinion
wm58026 = minion WmHcMinion WM.wm58026 (Sch 2) (Atk 2) (HP 3)

hc59036 :: MinionCard WmHcMinion
hc59036 = minion WmHcMinion HC.hc59036 (Sch 0) (Atk 3) (HP 5)

hc59037 :: MinionCard WmHcMinion
hc59037 = minion WmHcMinion HC.hc59037 (Sch 0) (Atk 2) (HP 6)

hc59042 :: MinionCard WmHcMinion
hc59042 = minion WmHcMinion HC.hc59042 (Sch 2) (Atk 0) (HP 4)

hc59043 :: MinionCard WmHcMinion
hc59043 = minion WmHcMinion HC.hc59043 (Sch 0) (Atk 3) (HP 7)

hc59044 :: MinionCard WmHcMinion
hc59044 = minion WmHcMinion HC.hc59044 (Sch 1) (Atk 3) (HP 5)

newtype WmHcMinion = WmHcMinion (Attrs Minion)
  deriving anyclass IsMinion
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor WmHcMinion where
  getModifiersFor _ target (WmHcMinion attrs) | isTarget attrs target && toCardCode attrs == "59044" = pure [KeywordModifier K.Piercing, KeywordModifier K.Ranged]
  getModifiersFor _ _ _ = pure []

instance HasAbilities WmHcMinion where
  getAbilities m@(WmHcMinion attrs) = case toCardCode attrs of
    "58026" -> [limitedWindowAbility m 1 (W.CharacterDefeatedByEnemyAttack W.After $ EnemyWithId $ EnemyMinionId mid) ForcedResponse NoCriteria NoCost $ runAbility m 1]
    "59036" -> [limitedWindowAbility m 1 (W.MinionHasSchemed W.After $ MinionWithId mid) ForcedResponse NoCriteria NoCost $ runAbility m 1]
    "59037" -> [limitedWindowAbility m 1 (W.WindowOneOf [W.EnemyTookDamage (EnemyWithId $ EnemyMinionId mid) (AttackFromPlayer AnyIdentity), W.EnemyTookDamage (EnemyWithId $ EnemyMinionId mid) (AttackFromAlly AnyAlly)]) ForcedResponse NoCriteria NoCost $ runAbility m 1]
    code | code `elem` ["59042", "59043"] -> [limitedWindowAbility m 1 (W.EnemyActivates W.After (EnemyWithId $ EnemyMinionId mid) AnyIdentity) ForcedResponse NoCriteria NoCost $ runAbility m 1]
    _ -> []
   where mid = minionId attrs

instance RunMessage WmHcMinion where
  runMessage msg m@(WmHcMinion attrs) = runQueueT $ case msg of
    RanAbility _you (isTarget attrs -> True) _idx windows _ -> do
      case toCardCode attrs of
        "58026" -> for_ [aid | W.EnemyAttackDefeatedCharacter _ (AllyCharacter aid) <- windows] $ \aid -> getCharacterOwner (AllyCharacter aid) >>= traverse_ dealEncounterCard
        "59036" -> dealEncounterCard (minionEngagedIdentity attrs)
        "59037" -> do
          owners <- catMaybes <$> traverse getSourceController [source | W.EnemyTookDamageFrom enemy _ source <- windows, enemy == EnemyMinionId (minionId attrs)]
          for_ owners $ \who -> do
            affordable <- canAffordCost who (toSource attrs) $ ResourceCost $ Just Physical
            chooseOne who $ [labeled "$label.heal2Damage" $ heal (minionId attrs) 2]
              <> [labeled "$label.spendPhysicalResource" $ payCostThen who attrs (ResourceCost $ Just Physical) [] | affordable]
        code | code `elem` ["59042", "59043"] -> do
          schemes <- selectList $ SideSchemeIs HC.hc59041
          if null schemes then when (code == "59043") $ placeThreatOnEach attrs 1 MainScheme
          else traverse_ (\sid -> placeThreatOn attrs sid 2) schemes
        _ -> pure ()
      pure m
    MinionMessage mid (RevealMinion _) | mid == minionId attrs && toCardCode attrs == "59042" -> do
      searchEverywhereAndRevealEncounterCard (minionEngagedIdentity attrs) $ CardIs HC.hc59041
      pure m
    MinionMessage mid MinionDefeated | mid == minionId attrs && toCardCode attrs == "59042" -> do
      searchEverywhereAndRevealEncounterCard (minionEngagedIdentity attrs) $ CardIs HC.hc59041
      pure m
    _ -> WmHcMinion <$> runMessage msg attrs
