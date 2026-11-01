module Marvel.Event.Events.FlurryOfBlades where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Upgrades
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types

flurryOfBlades :: EventCard FlurryOfBlades
flurryOfBlades = event FlurryOfBlades Cards.flurryOfBlades

newtype FlurryOfBlades = FlurryOfBlades (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage FlurryOfBlades where
  runMessage msg a@(FlurryOfBlades attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      chooseDamage you attrs (toDamage 2 $ FromPlayerAttack you) AttackableEnemy
      cardSpecific attrs "knives" you
      pure a
    CardSpecific (isTarget attrs -> True) "knives" (fromJSON -> Success you) -> do
      n <- selectListCount $ UpgradeControlledBy (IdentityWithId you) <> UpgradeIs Upgrades.psiKnife
      for_ [1..n] $ \_ -> cardSpecific attrs "confuse" you
      cardSpecific attrs "katanas" you
      pure a
    CardSpecific (isTarget attrs -> True) "confuse" (fromJSON -> Success you) -> do
      enemies <- selectList AttackableEnemy
      chooseOne you [targeting (enemyRef enemy) do
        attackEnemyWithoutDamage you enemy attrs
        confuse enemy attrs
        | enemy <- enemies]
      pure a
    CardSpecific (isTarget attrs -> True) "katanas" (fromJSON -> Success you) -> do
      n <- selectListCount $ UpgradeControlledBy (IdentityWithId you) <> UpgradeIs Upgrades.psiKatana
      for_ [1..n] $ \_ -> cardSpecific attrs "damage" you
      pure a
    CardSpecific (isTarget attrs -> True) "damage" (fromJSON -> Success you) -> do
      whenM (selectAny AttackableEnemy) $
        chooseDamage you attrs (toDamage 2 $ FromPlayerAttack you) AttackableEnemy
      pure a
    _ -> FlurryOfBlades <$> runMessage msg attrs
