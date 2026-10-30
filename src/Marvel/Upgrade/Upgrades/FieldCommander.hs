module Marvel.Upgrade.Upgrades.FieldCommander where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hand
import Marvel.Id
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Stats
import Marvel.Window qualified as W
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

fieldCommander :: UpgradeCard FieldCommander
fieldCommander = upgrade FieldCommander Cards.fieldCommander

newtype FieldCommander = FieldCommander (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor FieldCommander where
  getModifiersFor _ (IdentityRef you) (FieldCommander attrs) =
    pure [TakesFirstPlayerPhaseTurn | you == upgradeController attrs]
  getModifiersFor _ (UpgradeRef uid) _ = do
    cyclopsUpgrade <- upgradeMatches (UpgradeOneOf $ map UpgradeIs
      [Cards.rubyQuartzVisor, Cards.fieldCommander, Cards.exploitWeakness, Cards.practicedDefense, Cards.priorityTarget]) uid
    onMinion <- selectAny (MinionEnemy <> EnemyWithUpgrade (UpgradeWithId uid))
    pure [LoseKeyword Temporary | cyclopsUpgrade && onMinion]
  getModifiersFor _ _ _ = pure []

instance RunMessage FieldCommander where
  runMessage msg (FieldCommander attrs) = runQueueT $ FieldCommander <$> runMessage msg attrs
