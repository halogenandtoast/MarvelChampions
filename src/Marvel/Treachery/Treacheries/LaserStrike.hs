module Marvel.Treachery.Treacheries.LaserStrike (laserStrike, LaserStrike (..)) where

import Marvel.Prelude
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

laserStrike :: TreacheryCard LaserStrike
laserStrike = treachery LaserStrike Cards.laserStrike

newtype LaserStrike = LaserStrike (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage LaserStrike where
  runMessage msg t@(LaserStrike attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      upgrades <- selectList (UpgradeControlledBy (IdentityWithId you) <> UpgradeThatCanBeDiscarded)
      if null upgrades
        then pure . LaserStrike $ attrs & surgeL .~ True
        else do
          chooseOne you [targeting upgrade (discardUpgrade upgrade) | upgrade <- upgrades]
          pure t
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      undefended <- selectAny (EnemyWithId enemy <> UndefendedEnemy)
      you <- getBoostedPlayer enemy
      upgrades <- selectList (UpgradeControlledBy (IdentityWithId you) <> UpgradeThatCanBeDiscarded)
      when (undefended && notNull upgrades) $
        chooseOne you [targeting upgrade (discardUpgrade upgrade) | upgrade <- upgrades]
      pure t
    _ -> LaserStrike <$> runMessage msg attrs
