module Marvel.Treachery.Treacheries.FieryRage where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Cards qualified as Minions
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

fieryRage :: TreacheryCard FieryRage
fieryRage = treachery FieryRage Cards.fieryRage

newtype FieryRage = FieryRage (Attrs Treachery)
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage FieryRage where
  runMessage msg e@(FieryRage attrs) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId attrs -> do
      phoenix <- selectOne (MinionIs Minions.darkPhoenix)
      case phoenix of
        Just mid -> do
          hero <- identityMatches HeroIdentity you
          if hero then enemyAttacks mid you else minionSchemesAgainst mid you
          pure e
        Nothing -> do
          traverse_ (`placeThreat` 1) =<< selectList (SideSchemeIs SideSchemes.consumeTheWorld)
          gainSurge attrs
          pure e
    _ -> FieryRage <$> runMessage msg attrs
