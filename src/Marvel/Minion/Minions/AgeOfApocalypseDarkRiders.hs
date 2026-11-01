module Marvel.Minion.Minions.AgeOfApocalypseDarkRiders where

import Marvel.Prelude
import Marvel.Id
import Marvel.Ability
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Window qualified as W
import Marvel.Card
import Marvel.Entity
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner ()
import Marvel.Minion.Types
import Marvel.Ability ()
import Marvel.Modifier ()
import Marvel.Ref
import Marvel.GameValue ()
import Marvel.Stats
import Marvel.Hp
import Marvel.Message.Lifted

newtype AoaDarkRidersMinion = AoaDarkRidersMinion (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

gauntlet :: MinionCard AoaDarkRidersMinion
gauntlet = minion AoaDarkRidersMinion Cards.gauntlet (Sch 2) (Atk 2) (HP 5)

barrage :: MinionCard AoaDarkRidersMinion
barrage = minion AoaDarkRidersMinion Cards.barrage (Sch 1) (Atk 2) (HP 4)

hardDrive :: MinionCard AoaDarkRidersMinion
hardDrive = minion AoaDarkRidersMinion Cards.hardDrive (Sch 2) (Atk 1) (HP 4)

tusk :: MinionCard AoaDarkRidersMinion
tusk = minion AoaDarkRidersMinion Cards.tusk (Sch 0) (Atk 2) (HP 6)

psynapse :: MinionCard AoaDarkRidersMinion
psynapse = minion AoaDarkRidersMinion Cards.psynapse (Sch 2) (Atk 1) (HP 3)

instance HasAbilities AoaDarkRidersMinion where
  getAbilities a@(AoaDarkRidersMinion attrs) = [limitedWindowAbility a 1
    (W.EnemyAttacked W.After (EnemyWithId $ EnemyMinionId $ minionId attrs) AnyIdentity)
    ForcedResponse NoCriteria NoCost (runAbility a 1)]
instance RunMessage AoaDarkRidersMinion where
  runMessage msg a@(AoaDarkRidersMinion attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ ([you | W.EnemyAttack _ you <- windows] <> [you | W.EnemyAttackAlly _ _ you <- windows]) $ \you -> do
        case toCardCode attrs of
          "45112" -> do
            upgrades <- selectList $ UpgradeControlledBy $ IdentityWithId you
            chooseAndDiscardControlled you $ map UpgradeRef upgrades
          "45113" -> do
            characters <- selectList $ CharacterControlledBy $ IdentityWithId you
            damageCharacters attrs (toDamage 1 FromAbility) characters
          "45114" -> placeThreatOnEach attrs 1 AnyScheme
          "45115" -> stunIdentity you
          "45116" -> confuseIdentity you
          _ -> pure ()
      pure a
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      you <- getBoostedPlayer enemy
      case toCardCode attrs of
        "45115" -> stunIdentity you
        "45116" -> confuseIdentity you
        _ -> pure ()
      pure a
    _ -> AoaDarkRidersMinion <$> runMessage msg attrs
