module Marvel.Minion.Minions.AgeOfApocalypseSavageLand where

import Marvel.Prelude
import Marvel.Keyword ()
import Marvel.Damage ()
import Marvel.Ability
import Marvel.AgeOfApocalypse.Settings
import Marvel.Card
import Marvel.Entity
import Marvel.Cost
import Marvel.Criteria
import Marvel.Domino (discardedResourceCount)
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner ()
import Marvel.Minion.Types
import Marvel.Modifier
import Marvel.Query
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.Stats
import Marvel.Hp
import Marvel.Window qualified as W
import Marvel.Environment.Cards qualified as Environments

newtype AoaSavageLandMinion = AoaSavageLandMinion (Attrs Minion `With` Maybe IdentityId)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsMinion AoaSavageLandMinion where
  toMinionAttrs (AoaSavageLandMinion (attrs `With` _)) = attrs
  overMinionAttrs f (AoaSavageLandMinion (attrs `With` actor)) = AoaSavageLandMinion (f attrs `With` actor)

pterosaur :: MinionCard AoaSavageLandMinion
pterosaur = minion (AoaSavageLandMinion . (`With` Nothing)) Cards.pterosaur (Sch 0) (Atk 3) (HP 4)

velociraptor :: MinionCard AoaSavageLandMinion
velociraptor = minion (AoaSavageLandMinion . (`With` Nothing)) Cards.velociraptor (Sch 0) (Atk 1) (HP 3)

giantApe :: MinionCard AoaSavageLandMinion
giantApe = minion (AoaSavageLandMinion . (`With` Nothing)) Cards.giantApe (Sch 1) (Atk 2) (HP 5)

instance HasModifiersFor AoaSavageLandMinion

instance HasAbilities AoaSavageLandMinion where
  getAbilities a@(AoaSavageLandMinion (attrs `With` _)) =
    [limitedWindowAbility a 1 (W.EnemyAttacked W.When (EnemyWithId $ EnemyMinionId $ minionId attrs) AnyIdentity)
      ForcedInterrupt NoCriteria NoCost (runAbility a 1) | toCardCode attrs == "45129"]

instance RunMessage AoaSavageLandMinion where
  runMessage msg e@(AoaSavageLandMinion (attrs `With` actor)) = runQueueT $ case msg of
    MinionMessage mid (RevealMinion you) | mid == minionId attrs, toCardCode attrs == "45128" -> e <$ resolveSettingSpecial you
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ ([you | W.EnemyAttack _ you <- windows] <> [you | W.EnemyAttackAlly _ _ you <- windows]) (\you -> discardFromDeck you 1 attrs)
      pure e
    WithDiscarded (isTarget attrs -> True) _ cards | toCardCode attrs == "45129" -> do
      you <- getBoostedPlayer (EnemyMinionId $ minionId attrs)
      amount <- discardedResourceCount you cards
      changeAttack (minionId attrs) (fromIntegral amount)
      pure e
    MinionMessage mid (MinionDefeatedWithExcess source _ _) | mid == minionId attrs -> do
      who <- getSourceController source
      updated <- runMessage msg attrs
      pure $ AoaSavageLandMinion (updated `With` who)
    MinionMessage mid MinionDefeated | mid == minionId attrs, toCardCode attrs == "45130" -> do
      who <- maybe getActivePlayerId pure actor
      resolveSettingSpecial who
      AoaSavageLandMinion . (`With` actor) <$> runMessage msg attrs
    Boost (RevealedAsBoost target enemy) | target == toRef attrs, toCardCode attrs == "45128" -> do
      savage <- selectAny (EnvironmentIs Environments.theSavageLand)
      when savage $ getBoostedPlayer enemy >>= dealBoostAsEncounterCard attrs
      pure e
    _ -> AoaSavageLandMinion . (`With` actor) <$> runMessage msg attrs
