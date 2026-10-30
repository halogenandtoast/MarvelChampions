module Marvel.Treachery.Treacheries.PoliticalRetribution where

import Marvel.Prelude
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Cards qualified as Minions
import Marvel.Query
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

politicalRetribution :: TreacheryCard PoliticalRetribution
politicalRetribution = treachery PoliticalRetribution Cards.politicalRetribution

newtype PoliticalRetribution = PoliticalRetribution (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage PoliticalRetribution where
  runMessage msg t@(PoliticalRetribution attrs) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery _) | tid == treacheryId attrs -> do
      lucia <- selectList (MinionIs Minions.luciaVonBardas)
      schemes <- selectList (SideSchemeIs Schemes.ruleByForce)
      traverse_ minionSchemes lucia
      traverse_ (\sid -> placeThreat sid 3) schemes
      pure $ PoliticalRetribution $ attrs & surgeL .~ (null lucia && null schemes)
    _ -> PoliticalRetribution <$> runMessage msg attrs
