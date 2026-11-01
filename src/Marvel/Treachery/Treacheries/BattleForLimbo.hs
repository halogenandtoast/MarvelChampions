module Marvel.Treachery.Treacheries.BattleForLimbo where

import Marvel.Prelude
import Marvel.Id ()
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types
import Marvel.Matchers
import Marvel.Query
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Trait qualified as Trait

battleForLimbo :: TreacheryCard BattleForLimbo
battleForLimbo = treachery BattleForLimbo Cards.battleForLimbo

newtype BattleForLimbo = BattleForLimbo (Attrs Treachery)
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage BattleForLimbo where
  runMessage msg a@(BattleForLimbo attrs) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId attrs -> do
      minions <- selectList (MinionWithTrait Trait.Limbo)
      if null minions then surge you else for_ minions \mid -> do
        engaged <- selectJust (IdentityEngagedWith $ MinionWithId mid)
        minionActivatesAgainst mid engaged
      pure a
    Boost (RevealedAsBoost target _) | target == toRef attrs -> do
      ruler <- selectOne (SideSchemeIs Schemes.rulerOfLimbo)
      for_ ruler (\scheme -> placeThreatOn attrs scheme 2)
      pure a
    _ -> BattleForLimbo <$> runMessage msg attrs
