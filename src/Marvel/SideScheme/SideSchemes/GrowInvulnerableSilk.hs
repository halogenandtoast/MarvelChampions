module Marvel.SideScheme.SideSchemes.GrowInvulnerableSilk where
import Marvel.Prelude
import Marvel.Id ()
import Marvel.Query
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Matchers
import Marvel.Question ()
import Marvel.Window qualified as W ()
import Marvel.Trait qualified as T ()
import Marvel.Damage ()
import Marvel.Game.Source
import Marvel.Card

import Marvel.Ability
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Silk.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.GameValue

growInvulnerableSilk :: SideSchemeCard GrowInvulnerableSilk
growInvulnerableSilk = sideScheme GrowInvulnerableSilk Cards.growInvulnerableSilk (Static 5)
newtype GrowInvulnerableSilk = GrowInvulnerableSilk (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance RunMessage GrowInvulnerableSilk where
  runMessage msg u@(GrowInvulnerableSilk attrs) = runQueueT $ case msg of
    SideSchemeMessage sid RevealSideScheme | sid == sideSchemeId attrs -> do
      minions <- selectList (MinionIs Cards.atlasSilk)
      for_ minions $ \mid -> getCountersOn (MinionRef mid) >>= placeThreatOn attrs sid
      pure u
    CardSpecific (isTarget attrs -> True) "checkGrowth" _ -> do
      present <- selectAny (SideSchemeWithId $ sideSchemeId attrs)
      when present $ do
        minions <- selectList (MinionIs Cards.atlasSilk)
        counters <- traverse (getCountersOn . MinionRef) minions
        when (any (>=10) counters) (gameOver Lost)
      pure u
    _ -> do
      whenM (selectAny $ SideSchemeWithId $ sideSchemeId attrs) $ cardSpecific attrs "checkGrowth" ()
      GrowInvulnerableSilk <$> runMessage msg attrs
