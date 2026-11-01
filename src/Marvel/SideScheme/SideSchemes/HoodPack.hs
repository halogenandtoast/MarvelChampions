module Marvel.SideScheme.SideSchemes.HoodPack where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hood.Helpers
import Marvel.Id
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Cards qualified as Minions
import Marvel.Modifier
import Marvel.Question (runAbility)
import Marvel.Query
import Marvel.Ref
import Marvel.Scenario.Types (Scenario (..), Attrs (scenarioEncounterDeck))
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W

newtype HoodSideScheme = HoodSideScheme (Attrs SideScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

hood24014, hood24023, hood24033, hood24042, hood24055, hood24056, hood24057, hood24058, hood24064 :: SideSchemeCard HoodSideScheme
hood24014 = sideScheme HoodSideScheme Cards.hood24014 (Static 3)
hood24023 = sideScheme HoodSideScheme Cards.hood24023 (Static 2)
hood24033 = sideScheme HoodSideScheme Cards.hood24033 (Static 5)
hood24042 = sideScheme HoodSideScheme Cards.hood24042 (Static 3)
hood24055 = sideScheme HoodSideScheme Cards.hood24055 (Static 3)
hood24056 = sideScheme HoodSideScheme Cards.hood24056 (Static 3)
hood24057 = sideScheme HoodSideScheme Cards.hood24057 (Static 3)
hood24058 = sideScheme HoodSideScheme Cards.hood24058 (Static 3)
hood24064 = sideScheme HoodSideScheme Cards.hood24064 (Static 3)

instance IsSideScheme HoodSideScheme where
  sideSchemeAbilities = getAbilities

instance HasModifiersFor HoodSideScheme where
  getModifiersFor _ target (HoodSideScheme a) | toCardCode a == "24064" = do
    elite <- case target of
      VillainRef _ -> pure True
      MinionRef mid -> minionMatches (MinionWithTrait Trait.Elite) mid
      _ -> pure False
    pure [KeywordModifier (Retaliate 1) | elite]
  getModifiersFor _ _ _ = pure []

instance HasAbilities HoodSideScheme where
  getAbilities (HoodSideScheme a) = case toCardCode a of
    "24014" ->
      [windowAbility a 1 (W.CharacterWithConditionWouldTakeDamage [StunnedStatus, ConfusedStatus]) ForcedInterrupt NoCost (runAbility a 1)]
    "24033" -> [windowAbility a 3 (W.EnemyWouldTakeDamage $ EnemyWithTrait Trait.Brute) ForcedInterrupt NoCost (runAbility a 3)]
    _ -> []

instance RunMessage HoodSideScheme where
  runMessage msg e@(HoodSideScheme attrs) = runQueueT $ case msg of
    Boost inner@(CardSpecific target _ _) | target == toRef attrs -> runMessage inner e
    SideSchemeMessage sid RevealSideScheme | sid == sideSchemeId attrs -> do
      you <- getActivePlayerId
      revealEffect you
      pure e
    CardSpecific (isTarget attrs -> True) "repeatOutForBlood" _ -> do
      you <- getActivePlayerId
      outForBlood you
      pure e
    CardSpecific (isTarget attrs -> True) "checkOutForBlood" (fromJSON -> Success c) -> do
      present <- selectAny (CharacterWithId c)
      unless present $ cardSpecific attrs "repeatOutForBlood" ()
      pure e
    RanAbility _ (isTarget attrs -> True) n windows _ | n `elem` [1,2] -> do
      let damaged = [IdentityCharacter you | W.IdentityTakeDamage you damage <- windows, damageAmount damage > 0]
            <> [AllyCharacter aid | W.AllyTakeDamage aid damage <- windows, damageAmount damage > 0]
      for_ (headMay damaged) $ \c -> do
        stunned <- characterHasCondition StunnedStatus c
        confused <- characterHasCondition ConfusedStatus c
        when (stunned || confused) $ increasePendingCharacterDamage c 1
      pure e
    RanAbility _ (isTarget attrs -> True) 3 windows _ -> do
      for_ (headMay [(enemy,damage) | W.EnemyWouldBeDamaged enemy _ damage <- windows]) $ \(enemy,damage) -> do
        cancelEnemyDamage enemy
        removeSideSchemeThreat (sideSchemeId attrs) (damageAmount damage)
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget attrs target, toCardCode attrs == "24023" -> do
      outForBlood =<< getBoostedPlayer enemy
      pure e
    _ -> HoodSideScheme <$> runMessage msg attrs
   where
    revealEffect you = case toCardCode attrs of
      "24023" -> outForBlood you
      "24033" -> searchAndRevealEncounterCard you (CardIs Minions.hood24035)
      "24042" -> do
        cards <- getEncounterDeck
        let found = filter (cardMatch $ CardWithType MinionType <> CardWithTrait Trait.Criminal) cards
        if null found then surge you else chooseOrRunOne you [targeting c (putEncounterCardIntoPlay you c) | c <- found]
        shuffleEncounterDeck
      "24055" -> discardHandCost True you
      "24056" -> dealIndirectDamage you attrs 3
      "24057" -> void $ discardControlledCost False you [AllyType,SupportType,UpgradeType]
      "24058" -> discardUntilIntoPlay you (CardWithType MinionType)
      _ -> pure ()
    outForBlood _ = do
      chars <- friendlyCharactersWithFewestRemainingHitPoints
      first <- selectJust FirstPlayer
      chooseOrRunOne first [targeting (characterRef c) do
        damageCharacter c attrs (toDamage 1 FromAbility)
        cardSpecific attrs "checkOutForBlood" c
        | c <- chars]
