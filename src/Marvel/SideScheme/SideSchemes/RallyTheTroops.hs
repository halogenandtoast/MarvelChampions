module Marvel.SideScheme.SideSchemes.RallyTheTroops where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question ()
import Marvel.Ref
import Marvel.Window qualified as W ()
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.GameValue

rallyTheTroops :: SideSchemeCard RallyTheTroops
rallyTheTroops = sideScheme RallyTheTroops Cards.rallyTheTroops (PerPlayer 3)

newtype RallyTheTroops = RallyTheTroops (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage RallyTheTroops where
  runMessage msg a@(RallyTheTroops attrs) = runQueueT $ case msg of
    SideSchemeMessage sid DefeatSideScheme | sid == sideSchemeId attrs -> do
      allies <- selectList AnyAlly
      for_ allies (`heal` 2)
      pure a
    _ -> RallyTheTroops <$> runMessage msg attrs
