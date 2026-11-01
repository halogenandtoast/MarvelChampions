module Marvel.SideScheme.SideSchemes.LeaderOfTheMorlocks where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.Id ()
import Marvel.Matchers ()
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query ()
import Marvel.Question ()
import Marvel.Ref
import Marvel.Status ()
import Marvel.Window qualified as W ()
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.GameValue
import Marvel.Treachery.Cards qualified as Treacheries

leaderOfTheMorlocks :: SideSchemeCard LeaderOfTheMorlocks
leaderOfTheMorlocks = sideScheme LeaderOfTheMorlocks Cards.leaderOfTheMorlocks (PerPlayer 2)

newtype LeaderOfTheMorlocks = LeaderOfTheMorlocks (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage LeaderOfTheMorlocks where
  runMessage msg s@(LeaderOfTheMorlocks attrs) = runQueueT $ case msg of
    SideSchemeMessage sid DefeatSideScheme | sid == sideSchemeId attrs -> do
      you <- getDefeatedBy attrs
      searchEverywhereAndRevealEncounterCard you (CardIs Treacheries.knifeFight)
      pure s
    _ -> LeaderOfTheMorlocks <$> runMessage msg attrs
